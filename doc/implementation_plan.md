# Implementation Plan — Complexity Classes (Time & Memory)

Self-contained execution plan for implementing the complexity-class system.
An agent with only this document + a checkout of the repo must be able to
implement, build, and test its assigned task without reading the design
history.

Parent docs (background only, not required for execution):
`doc/next_task.md`, `doc/complexity_proposal.md`, `doc/roadmap.md` §9 M2.

## 0. Goal and scope

Implement Lean-level resource bounds for generated C programs along two axes
(time = abstract step count, memory = peak live allocation as functions of
input size `ℕ → ℕ`), ordered by a local Big-O relation, threaded through
program context and process combinators, with one proven intermediate-class
insertion and one runnable test.

In scope: Big-O kit, base time lattice (5 classes), base memory lattice
(5 classes), graph ordering + insertion predicate, first intermediate class
(`O_log`), context/process threading + composition lemmas, `Tests/`
example, roadmap status update.

Out of scope: Mathlib dependency, loop/variant rules (stub only), full
Clight semantics, C codegen changes, inference of bounds, benchmarks.

## 1. Repo facts (frozen at plan time)

- Toolchain: `lean-toolchain` = `leanprover/lean4:v4.23.0`.
- Deps: none (`lake-manifest.json`: `packages: []`). **Do not add any.**
- Layout:
  - `LeanC/Complexity.lean` (88 lines) — stubs to replace: `CComplexity`,
    `CComplexityRelationship`, `CZeroComplexity/CTerminates/CUndecidable`
    + instances, `can_insert_complexity_class_to_grapth := False`,
    `CComplexityGraph`, `LE … := False`.
  - `LeanC/Context.lean` (23 lines) — `CContext` with broken
    `appendContext {Γ1 Γ2} : Γ1 → Γ2` to be redesigned (see T6).
  - `LeanC/Processes.lean` (59 lines) — `CStatement` (only
    `isStatementSound`), `SequentialProcess`, `BinaryBranchingProcess`
    (see T7). Has `TODO: link to CComplexity`.
  - `LeanC/Arrays.lean`, `LeanC/Types.lean` — read-only context
    (`within_bounds`, `CTypeSize`); **do not modify**.
  - Tests: `test.lean` runner (`runAll : List (IO UInt32) → IO UInt32`,
    `main` runs list in order, first non-zero wins); each test module
    exposes `def test : IO UInt32` (`0` = success). Existing example:
    `Tests/TestMain.lean`.
- Commands (run from repo root):
  - `lake build` — must pass with no `sorry`/`axiom` in new code.
  - `./.lake/build/bin/test` — must exit `0`.
  - `rg -n "sorry|axiom|admit" LeanC Tests` — must print nothing for new code.

## 2. Frozen interfaces (do not rename without updating all tasks)

All parallel work builds against these exact names/signatures.
If a change is unavoidable, the changing agent must update this section
and notify all others.

```lean
-- T1: local Big-O (LeanC/Complexity.lean, top of file after namespace/universe)
def BigO (f g : ℕ → ℕ) : Prop :=
  ∃ c N₀, ∀ n ≥ N₀, f n ≤ c * Nat.max (g n) 1

-- T1 kit (exact names):
theorem BigO.refl  (f : ℕ → ℕ) : BigO f f
theorem BigO.trans {f g h : ℕ → ℕ} : BigO f g → BigO g h → BigO f h
theorem BigO.const_le_one {K : Nat} (c : ℕ → ℕ) (hc : ∀ n, c n ≤ K) :
  BigO c (fun _ => 1)
theorem BigO.add {f₁ g₁ f₂ g₂ : ℕ → ℕ} :
  BigO f₁ g₁ → BigO f₂ g₂ → BigO (fun n => f₁ n + f₂ n) (fun n => g₁ n + g₂ n)
theorem BigO.max_bound {f₁ g₁ f₂ g₂ : ℕ → ℕ} :
  BigO f₁ g₁ → BigO f₂ g₂ →
  BigO (fun n => Nat.max (f₁ n) (f₂ n)) (fun n => Nat.max (g₁ n) (g₂ n))

-- T2/T3: axis + class (LeanC/Complexity.lean)
inductive ResourceAxis | time | memory

class CComplexity (axis : ResourceAxis) (α : Type u) where
  isComplexity : Prop

-- representative bundle (quantitative classes carry one)
structure ComplexityRep where
  rep : ℕ → ℕ

-- T2 time base (exact inductive names):
inductive ZeroTimeComplexity | mk            -- rep fun _ => 0 (bottom, by stipulation)
inductive TimeComplexity_O1 | mk             -- rep fun _ => 1
inductive TimeComplexity_HALTS | mk          -- no rep: ∃ g, BigO cost g
inductive TimeComplexity_UNBOUND | mk        -- proven no-rep (divergence witness)
inductive TimeComplexity_UNDECIDABLE | mk    -- unknown (top)

-- T3 memory base (exact inductive names):
inductive ZeroMemoryComplexity | mk
inductive MemoryComplexity_O1 | mk
inductive MemoryComplexity_BOUNDED | mk
inductive MemoryComplexity_GROWING | mk
inductive MemoryComplexity_UNKNOWN | mk

-- T5 first intermediate (exact name + rep):
inductive TimeComplexity_log | mk            -- rep fun n => Nat.log2 (n + 1)

-- canonical reps (used by T2–T5, T8):
--   gZero := fun _ => 0 | g1 := fun _ => 1
--   glog  := fun n => Nat.log2 (n + 1) | gpoly (k) := fun n => n ^ k

-- T4: relationships + graph (keep existing shape, fix semantics):
-- class CComplexityRelationship (α : Type u) [CComplexity axis α] where
--   minimalStrictlySmaller : List Type
--   minimalStrictlyLarger  : List Type
--   equalTo                : List Type     -- NB: fixes typo `eqialTo`
-- inductive CComplexityGraph : List (Type u) → Type (u+1) | nil | insert …
-- def can_insert_complexity_class_to_grapth … : Prop  -- real predicate, not False
-- instance : LE (CComplexityGraph α)                 -- real order, not False

-- T6: context bound (LeanC/Context.lean):
structure ResourceBound where
  timeRep : ℕ → ℕ
  memRep  : ℕ → ℕ

-- T7: statement bound (LeanC/Processes.lean, added field + lemmas):
-- class CStatement (Γ : Type v) [CContext Γ] (α : Type u) where
--   isStatementSound : Prop
--   stmtBound : ResourceBound
--   bound_sound_time : BigO (stmtCostTime α) stmtBound.timeRep  -- exact cost
--   ...                                                        -- fn TBD by T7,
--                                                              -- interface to T8
--                                                              -- is `stmtBound` only
-- lemmas: seq_bound_add (via BigO.add + const folding),
--         branch_bound_max (via BigO.max_bound)

-- T8: test (Tests/TestComplexity.lean):
-- def test : IO UInt32   -- 0 on success, wired into test.lean runAll
```

Conventions: `Prop`-valued class fields (existing style); no `sorry`/
`axiom`/`admit`; `O1` means `≤ K` for fixed `K` (i.e. `O(1)`), never
"exactly 1 step"; `Nat.log2` representative always used as
`fun n => Nat.log2 (n + 1)` to avoid `log2 0/1 = 0` degenerate cases.

## 3. Task DAG and agent assignment

```text
T1 (BigO kit) ─┬─▶ T2 (time lattice) ──▶ T4 (graph+insert) ──▶ T5 (O_log)
               ├─▶ T3 (memory lattice) ──┘
               ├─▶ T6 (context) ──▶ T7 (processes) ──▶ T8 (tests) ──▶ T9 (docs/review)
               └─▶ (T6/T7 may start from §2 interfaces alone; sync with T1 on BigO name only)
```

Suggested 4-agent split (any grouping works if interfaces frozen):
A: T1+T4 | B: T2+T3+T5 | C: T6+T7 | D: T8+T9.
Estimated total 3–5 focused days; per-task budgets below.

## 4. Tasks

### T1 — Local Big-O kit — `LeanC/Complexity.lean` — 0.5–1d — no deps

Objective: replace dependence on future Mathlib `=O` with the frozen local
`BigO` + prove the kit. Everything downstream depends only on the names in §2.

Steps:
1. Read `LeanC/Complexity.lean:1-20`.
2. At top (after `namespace LeanC`, before `CComplexity`) add the frozen
   `def BigO` from §2 verbatim + doc comment noting it is the `ℕ → ℕ`
   restriction of `Asymptotics.IsBigO Filter.atTop` (compat remark).
3. Prove in order: `BigO.refl` (take `c := 1, N₀ := 0`);
   `BigO.trans` (multiply constants: `c₁*c₂`, `max N₁ N₂`; needs
   `Nat.le_trans` + `Nat.mul_le_mul` monotonicity);
   `BigO.const_le_one` (take `c := K, N₀ := 0`, use `Nat.max … 1 ≥ 1`);
   `BigO.add`, `BigO.max_bound` (take `c := c₁+c₂`, pointwise
   `Nat.add_le_add` / `Nat.max_le` reasoning).
4. Keep proofs elementary (`omega` + `simp` + `Nat.*` lemmas welcome);
   no new imports.

Accept: `lake build` passes; each lemma has `#check @BigO.<name>`
resolving; no `sorry`. Interface check: exact names/signatures of §2.

Pitfalls: `Nat.max (g n) 1` — never drop the `max`; `trans` needs
`c₁ * (c₂ * m) = (c₁*c₂) * m` (`Nat.mul_assoc`).

### T2 — Base time lattice — `LeanC/Complexity.lean` — 0.5d — needs T1 names

Objective: the 5 time classes + `CComplexity` migration + relationships.

Steps:
1. Change `class CComplexity (Γ : Type v)` → frozen axis-parameterised
   `class CComplexity (axis : ResourceAxis) (α : Type u)`; add
   `inductive ResourceAxis | time | memory` above it.
2. Add the 5 frozen time inductives; add `CComplexity .time` instances
   (`isComplexity := True`, matching existing style).
3. Add `CComplexityRelationship` entries encoding exactly:
   `Zero < O1 < HALTS < UNDECIDABLE`, `UNBOUND < UNDECIDABLE`,
   `UNBOUND` incomparable otherwise. (Field renamed `equalTo`; keep
   deprecated `eqialTo` alias only if cheap — else rename outright and
   fix all uses in-file.)
4. Keep deprecated abbrevs for one commit:
   `abbrev CZeroComplexity := ZeroTimeComplexity`,
   `abbrev CTerminates := TimeComplexity_HALTS`,
   `abbrev CUndecidable := TimeComplexity_UNDECIDABLE`.
5. Add `example`s (compile checks, not comments) for each instance.

Accept: builds; `rg eqialTo` shows only alias (or nothing);
`Zero/O1/HALTS/UNBOUND/UNDECIDABLE` all resolve. Do not touch `LE`/
`can_insert` (T4).

### T3 — Base memory lattice — `LeanC/Complexity.lean` — 0.5d — needs T1 names

Mirror of T2, independent of T2 (can run in parallel).
Add the 5 frozen memory inductives + `CComplexity .memory` instances +
relationships `ZeroMem < O1Mem < BOUNDED < UNKNOWN`,
`GROWING < UNKNOWN`, `GROWING` otherwise incomparable.
Add `example` compile checks. Same conventions as T2.

Accept: builds alongside T2; all 10 base classes coexist; no `False` order
claims added (T4 owns ordering).

### T4 — Graph ordering + insertion predicate — `LeanC/Complexity.lean` — 1d — needs T2+T3

Objective: replace the two `False` stubs with real semantics backed by `BigO`.

Steps:
1. Implement `LE (CComplexityGraph α)` from the relationship tables:
   reflexive-transitive closure over declared edges; prove `refl`/`trans`
   instances used by T5/T8 (`le_refl`, `le_trans` examples must compile).
2. Implement `can_insert_complexity_class_to_grapth` as: predecessors'
   reps are `BigO`-below the new rep AND new rep is `BigO`-below
   successors' reps AND acyclicity (new type not already in `τs`, no path
   back). Statement shape:
   `can_insert … τ cg ↔ (∀ p ∈ preds, BigO rep[p] rep[τ]) ∧ (∀ s ∈ succs, BigO rep[τ] rep[s]) ∧ τ ∉ τs ∧ …`.
3. Decide + document exactly one mechanism for edge-splitting
   (`A < B` → `A < X < B`): either keep the `CComplexityShadowNode`
   sketch (`Complexity.lean:56-73`) or delete that comment and use
   "list of types + derived order on the fly" — never both. Record choice
   in a 3-line comment at the predicate.
4. Add `example`s: `Zero ≤ O1`, `O1 ≤ HALTS`, `HALTS ≤ UNDECIDABLE`,
   `UNBOUND ≤ UNDECIDABLE`, plus `¬ (HALTS ≤ UNBOUND)` (incomparability).

Accept: no `:= False` remains in ordering/insertion paths; T5 can call the
predicate with `BigO` evidence; incomparability example compiles.

### T5 — First intermediate class `O_log` — `LeanC/Complexity.lean` — 1–1.5d — needs T4

Objective: prove the extensibility mechanism works once, end to end.

Steps:
1. Add frozen `inductive TimeComplexity_log | mk` + `CComplexity .time`
   instance; define `glog := fun n => Nat.log2 (n + 1)` next to it.
2. Prove `BigO g1 glog` (hint: `Nat.log2 (n+1) ≥ 1` for `n ≥ 1`; take
   `N₀ := 1, c := 1`; handle `n = 0` by `N₀`. Alternatively via
   monotonicity + `const_le_one` direction — either is fine).
3. Prove `BigO glog (fun n => n ^ 2)` — i.e. `log =O n²` (hint:
   `Nat.log2 (n+1) ≤ n` for all `n` (`Nat.log2_le_self` family / induction),
   then `n ≤ n^2` for `n ≥ 1`; chain with `BigO.trans`. Budget most of the
   task here; if `Nat.log2` lemmas fight back, pin `N₀ := 1` early and use
   `omega`-friendly bounds rather than sharp ones).
4. Register relationships `smaller := [TimeComplexity_O1]`,
   `larger := [TimeComplexity_HALTS]` and discharge `can_insert_…` with
   the proofs from steps 2–3 (+ `O_log ≤ HALTS` as existential
   instantiation).
5. Strictness (required): `¬ BigO glog g1` sketch — exhibit growth
   (for every `c`, pick `n = 2^(c+1)` so `log2(n+1) > c`). Full proof
   preferred; `O_log < HALTS`-strictness may stay `≤` if time-boxed
   (record in code comment).

Accept: `O1 < O_log < HALTS` examples compile via `can_insert`; evidence
terms are the `BigO` proofs, not assertions; builds clean.

### T6 — Context threading — `LeanC/Context.lean` — 0.5–1d — needs T1 names only

Objective: `CContext` carries the current worst bound.

Steps:
1. Read `LeanC/Context.lean` fully (23 lines). Keep `isCContext`.
2. Add frozen `structure ResourceBound` (`timeRep`, `memRep : ℕ → ℕ`).
3. Replace broken `appendContext {Γ1 Γ2} : Γ1 → Γ2` with an explicit
   bound-carrying extension, e.g.:
   `extendTime : Γ → (ℕ → ℕ) → Γ` / `extendMem` (exact names agent's
   choice, but must be used consistently by T7) OR a single
   `extend : Γ → ResourceBound → Γ`. Keep old field removed (no compat:
   it is unusable as typed).
4. Document the invariant in a doc comment: context holds the path's
   accumulated `(timeRep, memRep)`; branching joins on exit (T7).

Accept: builds; `ResourceBound` resolves from `LeanC.Processes`;
`rg appendContext` shows only new signature or nothing.

### T7 — Statement bounds + composition — `LeanC/Processes.lean` — 1d — needs T1+T6

Objective: every statement exposes `stmtBound`; seq/branch compose.

Steps:
1. Import `LeanC.Complexity`; extend `CStatement` with `stmtBound :
   ResourceBound` (keep `isStatementSound`). Cost-function plumbing
   (`stmtCostTime`) is agent's choice but must not leak into T8's
   interface beyond `stmtBound`.
2. `SequentialProcess.nil` → bound `(fun _ => 0, fun _ => 0)`;
   `cons` → time `fun n => t₁ n + t₂ n` folded via `BigO.add` (+ const
   corollary `O1+O1=O1`), memory `fun n => Nat.max m₁ m₂` via
   `BigO.max_bound`. Prove `seq_bound_add`, `seq_bound_max_mem` lemmas.
3. `BinaryBranchingProcess` → both components `max` of branches (+ `O1`
   guard sequenced first); prove `branch_bound_max` via `BigO.max_bound`.
4. Loops: stub only — note `UNBOUND` propagation in a comment; no new
   inductives required.
5. Update the two existing `CStatement` instances (nil/cons) + branch
   instance to supply `stmtBound`; keep soundness conjunctions as-is.

Accept: builds; `stmtBound` available on `SequentialProcess` and
`BinaryBranchingProcess`; composition lemmas compile; T8 can state worst
bounds using only `stmtBound` + `BigO` kit.

### T8 — Tests — `Tests/TestComplexity.lean` + `test.lean` — 0.5d — needs T1–T7

Objective: runnable proof the system works.

Steps:
1. Create `Tests/TestComplexity.lean` following `Tests/TestMain.lean`
   pattern (`namespace TestComplexity`, `def test : IO UInt32`, `0` =
   success). Content in order:
   a. Kit smoke checks (as `example`s + runtime `IO.println` confirmations):
      `BigO.refl`, `BigO.trans`,
      `BigO (fun _ => 1) (fun n => Nat.log2 (n + 1))`,
      `BigO (fun n => Nat.log2 (n + 1)) (fun n => n ^ 2)`.
   b. Order checks: `Zero < O1 < HALTS < UNDECIDABLE`,
      `UNBOUND < UNDECIDABLE`, `¬ (HALTS ≤ UNBOUND)`.
   c. Insertion: `O1 < O_log < HALTS` via `can_insert_…`.
   d. One 2–3-statement `SequentialProcess` of `O1` bounds with proven
      `O1` worst bound (const folding) + one binary branch with proven
      `max` bound.
2. Wire into `test.lean`: `import Tests.TestComplexity`, add
   `TestComplexity.test` to `runAll [...]` after `TestMain.test`.
3. Run `lake build && ./.lake/build/bin/test` — exit `0`, all lines `OK`.

Accept: new file + 2-line `test.lean` diff; binary exits 0; no test
bypasses (no unconditional `pure 0` without running checks).

### T9 — Docs + final review — 0.5d — needs T8 green

1. `doc/roadmap.md` §9: mark M2 in-progress, link `doc/next_task.md` +
   this plan.
2. `doc/next_task.md`: flip `Status: planned` → `implemented (date)`.
3. Final sweep: `lake build`, test binary, `rg sorry|axiom|admit`,
   confirm §2 frozen interfaces match code (fix doc if code chose
   documented-flexible names in T6/T7).

## 5. Definition of done (global)

- [ ] `lake build` clean, no `sorry`/`axiom`/`admit` in `LeanC/`+`Tests/`.
- [ ] `./.lake/build/bin/test` exits `0`.
- [ ] 10 base classes + `O_log` present with frozen names; `LE` +
      `can_insert` real; `¬ (HALTS ≤ UNBOUND)` compiles.
- [ ] `CStatement` exposes `stmtBound`; seq-add + branch-max lemmas compile.
- [ ] `Tests/TestComplexity.lean` covers kit + order + insertion +
      seq/branch bounds.
- [ ] T9 doc updates landed.

## 6. Risks and fallbacks

- `Nat.log2` lemma gaps (T5) — fallback: weaken to `BigO glog (fun n => n)`
  first (`log ≤ id` is easier), then `id =O n²`; still discharges insertion.
- `List Type` graph proofs painful (T4) — fallback: index classes by `Nat`
  rank and define `LE` on ranks; keep `List Type` API as thin wrapper.
- Universe friction (`Type u` vs `Type v` in Processes) — fallback: pin new
  fields to `ℕ → ℕ` (no universe polymorphism) and leave existing binders
  untouched.
- `O1` disputes — frozen as `O(1)` (`≤ K`); any "exactly-1" reading is out
  of scope by decision (§2 conventions).
