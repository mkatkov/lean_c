# Complexity Classes Proposal — Time & Memory Bounds

Parent task: `doc/next_task.md`.
Current code: `LeanC/Complexity.lean:9-87`, `LeanC/Context.lean:18-20`,
`LeanC/Processes.lean:23-58`.

## 1. Problem

We need a Lean-level vocabulary to state and check **worst-case resource
bounds** for programs that will be emitted as C:

- **Time**: abstract operation / step count of the fragment.
- **Memory**: allocation required by the fragment (stack frame + heap
  high-water mark; per-block sizes already tracked in `LeanC/Arrays.lean`
  via `CGlobalStaticMemoryBlock / CFixedSizeMemoryBlock / CDynamicMemoryBlock`).

Bounds must live in the **context** (`CContext`), attach to every
statement/process (`CStatement`), and compose (sequence / branch / loop),
so that a closed program yields one provable `(timeBound, memBound)` pair.

What exists today is only a qualitative chain
`CZeroComplexity < CTerminates < CUndecidable` (`Complexity.lean:35-54`)
with ordering and insertion left as `False` stubs. This proposal keeps the
`CComplexityRelationship` + `CComplexityGraph` mechanism, fixes its
semantics, renames to the required vocabulary, adds the missing endpoints,
duplicates it over two resource axes, and defines how new quantitative
classes slot in.

Quantitative ordering (§3–§4) is grounded in **standard Big-O notation**
rather than hand-rolled lattice proofs, so each new class insertion
reduces to exhibiting Big-O inclusions proved with reusable lemmas.

## 2. Base classes

### 2.1 Time axis

| Lean name (proposed) | Replaces | Semantics |
|---|---|---|
| `ZeroTimeComplexity` | `CZeroComplexity` | Empty computation. Exactly 0 ops. Definition/bottom. Every program is `≥` it. |
| `TimeComplexity_O1` | (new) | Constant-bounded fragment: `≤ K` ops for a fixed `K` (take `K = 1` as canonical representative; any fixed constant folds here). Covers single assignment, single arithmetic op, single guarded dereference with proven `within_bounds`. |
| `TimeComplexity_HALTS` | `CTerminates` | Terminates in *some* finite number of steps, but no named quantitative bound is claimed. Ceiling for all quantitative classes. |
| `TimeComplexity_UNBOUND` | (new) | Known divergence: provably never stops (intentional server loop, proven infinite trace). |
| `TimeComplexity_UNDECIDABLE` | `CUndecidable` | Unknown: no termination/bound claim. Default for unanalysed code. Top of the lattice. |

Intended partial order (time):

```text
ZeroTime < O1 < HALTS < UNDECIDABLE
              UNBOUND < UNDECIDABLE
```

`UNBOUND` is incomparable with `Zero / O1 / HALTS`: a diverging program is
neither cheaper nor more expensive in finite steps — it is outside the
finite-step hierarchy. `UNDECIDABLE` is the unique top: both "known finite"
and "known infinite" refine "unknown".

Rationale for `O1` as "≤ K" rather than "exactly 1": statement-level
counting is only meaningful up to a constant factor once emitted to C
(one Lean op may become several C ops). Fixing "exactly 1" would make
composition lemmas brittle; "bounded by a fixed constant" is stable under
the refinement that codegen introduces (cf. roadmap §9 "preserved up to a
constant factor"). §3 makes this precise: `O1` is the Big-O class of the
constant function `fun _ => 1`.

### 2.2 Memory axis (mirror)

Same lattice shape, same insertion machinery, separate graph:

| Lean name (proposed) | Semantics |
|---|---|
| `ZeroMemoryComplexity` | No allocation beyond the ambient frame (pure register/temporary use). Bottom. |
| `MemoryComplexity_O1` | Constant-bounded allocation (`≤ K` bytes/cells, fixed at construction; covers one scalar local, one fixed block). |
| `MemoryComplexity_BOUNDED` | Finite but not (yet) quantitatively named bound. Analogue of `HALTS`. |
| `MemoryComplexity_GROWING` | Allocation grows without a proven ceiling in the analysed scope (analogue of `UNBOUND`; e.g. unbounded loop appending). Known-unbounded. |
| `MemoryComplexity_UNKNOWN` | Unknown allocation behaviour. Default/top. |

```text
ZeroMem < O1Mem < BOUNDED < UNKNOWN
                GROWING < UNKNOWN
```

Time and memory are tracked as a **pair** in context (see §5). A program's
worst bound is `(timeClass, memClass)`; either component may independently
be `UNDECIDABLE/UNKNOWN`.

### 2.3 Migration from current names

- `CZeroComplexity` → `ZeroTimeComplexity` (keep old name as deprecated abbrev for one commit).
- `CTerminates` → `TimeComplexity_HALTS`.
- `CUndecidable` → `TimeComplexity_UNDECIDABLE`.
- Add `TimeComplexity_O1`, `TimeComplexity_UNBOUND` + the five memory classes.
- `CComplexity` itself becomes either axis-parameterised
  (`CComplexity (axis : ResourceAxis)`) or split into `CTimeComplexity` /
  `CMemoryComplexity` sharing one relationship structure. Recommendation:
  one `ResourceAxis = .time | .memory` parameter + two graphs, so insertion
  proofs and `LE` are written once.

## 3. Big-O foundation and bridge to classes

### 3.1 Standard notation (Mathlib)

Mathlib provides the standard Landau vocabulary in
`Mathlib.Analysis.Asymptotics.Asymptotics`:

```lean
open Asymptotics Filter

-- f is Big-O of g along filter l (take l = atTop for asymptotic growth):
--   f =O[l] g  :=  Asymptotics.IsBigO l f g
-- meaning: ‖f‖ is eventually bounded by a constant multiple of ‖g‖.
```

For our purposes `α = ℕ` (input size), `l = Filter.atTop`, and cost/bound
functions are `ℕ → ℕ` (or `ℕ → ℝ` via coercion where norm lemmas are
needed). The library supplies the reusable proof kit we want to inherit:
reflexivity, transitivity, and congruence/addition/max lemmas for `=O`
(e.g. `IsBigO.refl`, `IsBigO.trans`, `IsBigO.add`), so chains like
`1 =O log =O poly` are proved by lemma application, not by new lattice
metatheory per class.

### 3.2 Dependency decision: local definition now, Mathlib-compatible

This repo currently has **zero dependencies** (`lake-manifest.json` has
`packages: []`), and one project goal is tiny portable binaries. Pulling
all of Mathlib just for `=O` is disproportionate at this stage.

Recommendation (two-stage):

- **Now (this task):** define a minimal local `BigO` over `ℕ → ℕ` in
  `LeanC/Complexity.lean` (no new dependency), stated so it is
  definitionally compatible with the Mathlib one:

  ```lean
  /-- Local Big-O on sizes: f is eventually bounded by c * max (g ·) 1. -/
  def BigO (f g : ℕ → ℕ) : Prop :=
    ∃ c N₀, ∀ n ≥ N₀, f n ≤ c * Nat.max (g n) 1
  ```

  Notes: the `max · 1` guards the `g n = 0` case (`Nat.log2 1 = 0`);
  the `N₀` is the `atTop` eventuality; the `c` is the Landau constant.
  Prove the small kit we need locally (`refl`, `trans`, `add`, `max`,
  const-folding) — a dozen short lemmas — and record a compatibility
  remark: this is the `ℕ → ℕ` restriction of
  `Asymptotics.IsBigO Filter.atTop`.

- **Later (optional):** add the Mathlib dependency and replace the local
  `BigO` by `Asymptotics.IsBigO Filter.atTop` (ideally behind the same
  name/notation `f =O[g]`), discharging the local kit as instances of the
  Mathlib lemmas. No class/lattice code should depend on which backing is
  active — only on the `=O` interface.

Either way, all ordering proofs in §4 are written against the `=O`
interface, so the migration is proof-preserving.

### 3.3 Bridge: a quantitative class *is* a Big-O set

Each quantitative complexity class is identified with the Big-O class of
a **representative bound function** `g`:

```lean
-- sketch
structure ComplexityBound where
  rep : ℕ → ℕ                       -- representative, e.g. fun _ => 1
  cost_le_rep : BigO cost rep       -- the program's cost is O(rep)
```

i.e. `Class(g) = { f | BigO f g }`. Then:

- **Class ordering = Big-O entailment = set inclusion:**
  `Class(g₁) ≤ Class(g₂)  ↔  BigO g₁ g₂`.
- **Inserting a new class** with representative `g_new` between `g_lo`
  and `g_hi` reduces to proving two `=O` facts:
  `BigO g_lo g_new` and `BigO g_new g_hi` (§4, requirement 2).
- **Strictness** (`O1 < O_log`, `O_log < O_poly`) is the
  non-inclusion direction, proved once per adjacent pair by exhibiting a
  separating growth fact (e.g. `¬ BigO log 1`, `¬ BigO poly log`); the
  rest of the lattice (transitive edges, `LE` over `CComplexityGraph`)
  is then *derived*, not asserted.

Canonical representatives (time; memory is analogous over live cells):

| Class | Representative `g : ℕ → ℕ` | Reads as |
|---|---|---|
| `ZeroTimeComplexity` | `fun _ => 0` | exactly 0 (definition; outside `=O` machinery, bottom by stipulation) |
| `TimeComplexity_O1` | `fun _ => 1` | `O(1)` |
| `TimeComplexity_log` | `Nat.log2` (or `fun n => Nat.log2 (n+1)`) | `O(log n)` |
| `TimeComplexity_poly` (degree `k`) | `fun n => n ^ k` | `O(n^k)` |
| `TimeComplexity_HALTS` | — (no representative) | `∃ g computable, BigO cost g` |
| `TimeComplexity_UNBOUND` | — | no finite `g` exists (proven divergence) |
| `TimeComplexity_UNDECIDABLE` | — | unknown (no claim) |

So the finite fragment of the lattice is exactly the divisor chain of
`BigO` over representatives:

```text
0 < 1 < log n < n^k < …  (in =O order)
```

and `HALTS` is its existential closure, `UNBOUND`/`UNDECIDABLE` the
explicitly non-finite / unknown cases (§3.4).

### 3.4 Qualitative classes sit outside Big-O (wrapping it)

Big-O only classifies *finite* cost functions. The three non-quantitative
time classes are therefore defined in terms of it, not inside it:

- `HALTS`: `∃ g : ℕ → ℕ, Computable g ∧ BigO cost g` — some finite bound
  exists, unnamed. Every quantitative class refines it by instantiation.
- `UNBOUND`: `¬ ∃ g, BigO cost g` *with a divergence witness* (infinite
  trace / non-termination proof). Known outside the finite hierarchy —
  hence incomparable with `Zero/O1/HALTS`, below `UNDECIDABLE`.
- `UNDECIDABLE`: no claim at all (neither a `g` nor a divergence proof).
  Unique top: both `HALTS` and `UNBOUND` refine it by forgetting
  information.

Same pattern on the memory axis with `cost` = peak live cells as a
function of input size.

### 3.5 Why this simplifies ordering proofs (worked mini-example)

Without Big-O, each insertion (`log`, `poly`, …) would need bespoke
`minimalStrictlySmaller/Larger` entries plus manual transitivity/acyclicity
arguments. With the bridge, the first insertion exercise becomes:

```lean
-- representatives
--   g1  := fun _ => 1
--   glog := fun n => Nat.log2 (n + 1)
--   gpoly2 := fun n => n ^ 2
example : BigO g1 glog := …       -- 1 =O log: from monotonicity + const lemma
example : BigO glog gpoly2 := …   -- log =O n^2: standard growth fact
example : Class g1 ≤ Class glog := …    -- by inclusion iff BigO
example : Class glog ≤ Class gpoly2 := …-- ditto
-- LE edges Zero < O1 < O_log < O_poly < HALTS then follow by transitivity
-- of =O; can_insert is discharged by the two BigO proofs, not by fiat.
```

Composition (§5.3) likewise reuses the kit: sequential time-addition is
`IsBigO.add`, branching max is the `=O`-of-`max` lemma, constant folding
(`O1 + O1 = O1`) is `BigO` const-absorption. New classes inherit all of
this for free.

## 4. Intermediate classes (log, poly, …)

Quantitative classes inserted later live strictly between `O1` and `HALTS`
(time) / `O1Mem` and `BOUNDED` (memory):

```text
Zero < O1 < O_log < O_poly < … < HALTS < UNDECIDABLE
```

Examples for the first insertion exercises:

- `TimeComplexity_log`: representative `Nat.log2` (i.e. `BigO cost log`);
  steps `≤ c · log n + d` in an explicit size parameter `n` (loop halving
  a range). The `+ d` is absorbed by the `max · 1` / constant in §3.2.
- `TimeComplexity_poly`: representative `fun n => n ^ k` for explicit `k`
  (nested fixed loops).

Requirements for any new class `X` with representative `g_X`:

1. Declare `minimalStrictlySmaller : List Type` (immediate predecessors),
   `minimalStrictlyLarger : List Type` (immediate successors),
   `equalTo : List Type` — i.e. fill `CComplexityRelationship` honestly,
   not `[]` by default. The entries must agree with the `=O` facts below.
2. Prove the insertion is **sound via Big-O**: `BigO g_pred g_X` and
   `BigO g_X g_succ` for each claimed adjacent edge, in the cost semantics
   of §6; the graph stays acyclic and transitive edges are respected. This
   discharges `can_insert_complexity_class_to_grapth`
   (currently `:= False`, `Complexity.lean:75-76`) — the witness is the
   pair of `=O` proofs, not an assertion.
3. Exhibit the **representative function** `g_X` (e.g.
   `fun n => Nat.log2 (n+1)`, `fun n => n ^ k`) and derive
   `O1 < X` and `X ≤ HALTS` as `BigO` corollaries (plus a strictness
   non-inclusion fact where `<` rather than `≤` is claimed).

No change to the base lattice files beyond adding the edge-split is
allowed: insertion must be additive. The existing `CComplexityShadowNode`
sketch (`Complexity.lean:56-73`) is one admissible implementation (shadow /
rebuild the chain when an edge `A < B` is split by `X` into `A < X < B`);
if a simpler "list of types + derived partial order on the fly" (also
mentioned there) is chosen, the shadow-node comment must be removed so
there is exactly one documented mechanism.

## 5. Context and composition

### 5.1 Bounds in context

Extend `CContext` (currently just `isCContext` + a placeholder
`appendContext`, `Context.lean:18-20`) with the current worst bound:

```lean
-- sketch, names to be frozen at implementation
structure ResourceBound where
  time : TimeClass   -- a type carrying CTimeComplexity + its BigO rep
  mem  : MemClass
```

`CContext` carries (among branch propositions and variable scopes) the
accumulated `ResourceBound` for the path so far. Branching extends the
context with the guard proposition (as already documented in
`Context.lean:10-12`) **and** joins bounds on exit.

### 5.2 Statement bound and worst-bound theorem

Each `CStatement Γ α` (today only `isStatementSound`, `Processes.lean:23-24`)
additionally exposes `stmtBound : ResourceBound` with the proof obligation:

> Executing the statement from any state satisfying `Γ` takes at most
> `stmtBound.time` steps and allocates at most `stmtBound.mem` — i.e. with
> `cost : ℕ → ℕ` the statement's size-indexed step/allocation function,
> `BigO cost stmtBound.time.rep` (and symmetrically for memory).

Closed-program theorem shape (to be proven per program, with lemmas per
combinator):

> `worst_bound prog = (T, M)` → every execution of `prog` terminates
> within `T` (unless `T = UNBOUND/UNDECIDABLE`, in which case the
> corresponding claim is vacuous/unknown) and never exceeds `M` live
> allocation — with "within `T`" meaning `BigO cost T.rep`.

### 5.3 Composition rules (to be proven as lemmas, via the =O kit)

- **Empty / nil** (`SequentialProcess.nil`): `(ZeroTime, ZeroMem)`.
- **Sequence** (`cons`, `Processes.lean:30-43`): time **adds**
  (lattice join of the sum — for base classes: anything + `Zero` = itself;
  `O1 + O1 = O1` under constant folding; anything finite + `HALTS = HALTS`;
  anything + `UNDECIDABLE = UNDECIDABLE`; anything + `UNBOUND = UNBOUND`);
  memory takes the **high-water mark** (max), since stack slots are reused
  and heap blocks persist per `CArray.isAllocated` state.
  Proof reuse: addition case is `BigO.add` (`(f₁+f₂) =O max g₁ g₂` then
  folded); const-folding is the `O(1)+O(1)=O(1)` corollary.
- **Binary branch** (`BinaryBranchingProcess`, `Processes.lean:47-57`):
  time and memory are both the **max** of the two branch bounds (guard
  evaluation itself costs `O1` and is sequenced first).
  Proof reuse: `=O`-of-`max` lemma.
- **Loops** (not yet in `Processes.lean`): out of scope for the bound
  proof beyond a stub — require an explicit variant + per-iteration bound;
  unbounded/`UNBOUND` loops propagate `UNBOUND` time. Iterated addition
  over `n` iterations is the `∑`-of-`=O` lemma (to be added with loops).

## 6. Cost semantics (minimal, to pin meaning down)

To keep this task small, fix only this:

- **Time**: one abstract step = one Lean-level primitive statement
  (assignment, guard test, call, proven-in-bounds load/store). `ZeroTime`
  programs contain none. Codegen to C is allowed to multiply steps by a
  fixed constant (recorded per emitter, default 1:1 for now) — sound
  precisely because `=O` absorbs constants. Each fragment exposes
  `cost : ℕ → ℕ` mapping input size to step count; its class membership is
  `BigO cost rep`.
- **Memory**: one abstract cell = one `CTypeSize.size_of` unit of a live
  allocated block. `within_bounds` proofs (`Arrays.lean:117-179`) are
  prerequisites for counting an access as `O1` rather than `UNKNOWN`.
  Peak live cells as a function of input size is likewise classified by
  `BigO`.

Full small-step semantics and CompCert-style refinement stay in roadmap §9
future work; this proposal only needs the two definitions above to make
`Zero / O1 / HALTS / UNBOUND / UNDECIDABLE` decidable claims rather than
`:= True` placeholders.

## 7. Lean implementation sketch

```lean
inductive ResourceAxis | time | memory

class CComplexity (axis : ResourceAxis) (α : Type u) where
  isComplexity : Prop

-- §3.2 local Big-O (Mathlib-compatible interface)
def BigO (f g : ℕ → ℕ) : Prop :=
  ∃ c N₀, ∀ n ≥ N₀, f n ≤ c * Nat.max (g n) 1
-- kit: BigO.refl, BigO.trans, BigO.add, BigO.max, BigO.const_fold …

-- quantitative class = representative + membership proof obligation
structure ComplexityClass (axis : ResourceAxis) where
  rep : ℕ → ℕ

-- time axis base
inductive ZeroTimeComplexity | mk                      -- rep fun _ => 0
inductive TimeComplexity_O1 | mk                       -- rep fun _ => 1
inductive TimeComplexity_log | mk                      -- rep fun n => Nat.log2 (n+1)
inductive TimeComplexity_poly (k : Nat) | mk           -- rep fun n => n ^ k
inductive TimeComplexity_HALTS | mk                    -- ∃ rep, no fixed rep
inductive TimeComplexity_UNBOUND | mk                  -- proven no-rep
inductive TimeComplexity_UNDECIDABLE | mk              -- unknown
-- + instances CComplexity .time … and CComplexityRelationship entries
--   encoding the §2.1 order, each quantitative edge backed by a BigO proof.

-- memory axis base: ZeroMemoryComplexity, MemoryComplexity_O1,
-- MemoryComplexity_BOUNDED, MemoryComplexity_GROWING, MemoryComplexity_UNKNOWN

-- first extensibility exercise (one of):
inductive TimeComplexity_log (c d : Nat) | mk
-- with CComplexityRelationship: smaller := [TimeComplexity_O1],
--   larger := [TimeComplexity_HALTS],
-- backed by: BigO (fun _ => 1) logRep and BigO logRep polyRep-or-HALTS.
```

`LE` over `CComplexityGraph` and `can_insert_…` are then proven from the
relationship tables + acyclicity, replacing the `False` stubs — with each
quantitative edge's evidence being the corresponding `BigO` proof, so
`≤` over classes is sound by construction w.r.t. `=O` inclusion.

## 8. Tests

New `Tests/TestComplexity.lean` (name flexible) with `def test : IO UInt32`:

1. Big-O kit smoke checks: `BigO.refl`, `BigO.trans`,
   `BigO (fun _ => 1) (fun n => Nat.log2 (n+1))`,
   `BigO (fun n => Nat.log2 (n+1)) (fun n => n ^ 2)`.
2. Base order checks derived from (1): `Zero < O1 < HALTS < UNDECIDABLE`,
   `UNBOUND < UNDECIDABLE`, `¬ (HALTS ≤ UNBOUND)`.
3. One intermediate insertion: `O1 < O_log < HALTS` via `can_insert`,
   evidence = the two `BigO` proofs from (1).
4. One `SequentialProcess` of 2–3 `O1` statements with proven bound `O1`
   (constant folding via `BigO.add` + const corollary) and one binary
   branch with proven bound `max(branch bounds)` (via `BigO.max`).
5. Wired into `test.lean:runAll` alongside `TestMain.test`.

## 9. Decision log (to fill at review)

- [x] Ordering proofs via standard Big-O (`=O` interface): adopted (§3).
- [ ] `O1` = `≤ K` (recommended) vs exactly-1. (§3.3 fixes `≤ K` as `O(1)`.)
- [ ] Local `BigO` now + Mathlib later (recommended, §3.2) vs Mathlib dependency now.
- [ ] `Nat.log2 (·+1)` vs real-valued `log` via coercion for the `O_log` representative.
- [ ] One axis-parameterised `CComplexity` vs two separate classes.
- [ ] Shadow-node vs rebuild-on-insert for the graph.
- [ ] First intermediate class: `log` vs `poly`.
