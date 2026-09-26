import LeanC.Complexity.BigO
import LeanC.Complexity.Growth
import LeanC.Complexity.Classes
import LeanC.Complexity.Lattice
import LeanC.Complexity.Graph
import LeanC.Complexity.Quant
import LeanC.Complexity.Bridge

/-!
# Complexity classes — worst-case time & memory bounds

**What this module is:** a Lean vocabulary for stating and checking
worst-case resource bounds of programs that will be emitted as C.
Every fragment has a *cost function* `cost : Nat → Nat` (input size →
step count for time, peak live cells for memory). A *complexity class*
is the set of cost functions inside one Big-O envelope. A closed
program yields one provable `(timeClass, memClass)` pair.

**Why Big-O (not a hand-rolled lattice):** ordering classes then means
proving standard growth facts (`1 =O log =O n^k`), reusing one small
lemma kit (`refl/trans/add/max`). Inserting a new class reduces to
exhibiting two Big-O inclusions — no new metatheory per class.
The local `BigO` (`Complexity/BigO.lean`) is deliberately stated to match
`Asymptotics.IsBigO Filter.atTop`, so a future Mathlib dependency can
replace it behind the same name without touching lattice code.

**The two axes:** time (`Zero < O1 < log < poly < HALTS`, plus
`UNBOUND` on the side, all below `UNDECIDABLE`) and memory (mirror:
`Zero < O1 < BOUNDED`, plus `GROWING`, below `UNKNOWN`). Either axis
may independently be unknown. `O1` means "bounded by *some fixed
constant* (`≤ K`), never "exactly 1 step" — one Lean op may become
several C ops, and `=O` absorbs constants, which is what keeps
codegen sound. `Zero` is separated from `BigO` by stipulation: `BigO`
with the `max · 1` guard equates `0` and `1` (both `bigO_zero_le_one`
and `bigO_one_le_zero` hold), so `TagLE` stores every `Zero` edge as
`True` and exposes no `O1 ≤ Zero` row — see `Lattice.lean`. `poly 0`
is degenerate (`gpoly 0 = 1`): it is recorded as *equal* to `O1`
(`equalTo`), not strictly above.

**Extended resource model (generic core + per-resource bridge):**
core (`LeanC/Context.lean`) defines only the generic mechanism —
`CResource` combine ops, `HasCost` extraction (`cost : R → Nat → Nat`),
type-keyed `RStore`. Classes live here. The seam is
`costInClass` (`Complexity/Bridge.lean`): `v` is in `C` iff
`BigO (cost v) (rep C)`, transporting along `QuantLE` via `BigO.trans`.
Per-resource composition (`seq` adds time / maxes memory, `branch` maxes
+ `O1` guard) is proved per resource via `BigO.add`/`max_bound` — see
`Examples/Resources.lean` (`time_seq_preserves`, `time_branch_preserves`,
`mem_seq_preserves`, `O1+O1=O1`). No hardwired `ResourceBound` pair in
core; new resources add a type + instances + preservation lemmas.

**How ordering is stored (read this before extending):**
- `ComplexityTag` (`Complexity/Lattice.lean`) is a *closed* snapshot of
  the base lattice used for decidable checks (`decide`) and the `LE`
  instance. Base tags never change shape — new quantitative classes do
  NOT need new tags (see below).
- `HasTag` / `TagOf` map the base class *types* to those tags.
- `HasQuantRep` (open typeclass, `Complexity/Quant.lean`) is the
  **extension point**: any new file can give a new class type a
  representative function `rep : Nat → Nat` without touching this module.
  Ordering for such classes is plain `BigO` on reps (`QuantLE`), and
  insertion needs no new machinery: prove the `BigO` edges plus the
  list memberships. Worked example: `Examples/ComplexityLinear.lean`
  adds `TimeComplexity_linear` (`rep = fun n => n`) in its own file —
  no base edits, not core library.
- `CComplexityRelationship` (open typeclass, `Complexity/Classes.lean`)
  records immediate predecessors/successors/equalities; the
  *evidence* for a quantitative edge is always the `BigO` proof, never
  an assertion. The only base equality is `poly 0 = O1`.
- `CComplexityGraph` (`Complexity/Graph.lean`) is just the knowledge base
  (list of known class types). `LE` on same-base graphs is trivial
  (`True`); the real order lives on tags/reps.
  `can_insert_complexity_class_to_graph` (spelled `graph`; the old
  `grapth` spelling remains as a deprecated alias) is the closed predicate
  for the base tags; new classes are inserted via the unified open
  predicate `can_insert_quant_to_list` (`Complexity/Quant.lean`: same
  memberships, `BigO` evidence on explicit reps, no tags) instead.
- `costInClass` (`Complexity/Bridge.lean`) bridges resource *values* to
  classes (`BigO (cost v) rep`, mono along `QuantLE`).

**How to add a new quantitative class `X` (copy-paste recipe):**
1. `inductive TimeComplexity_X where | mk` (+ `CComplexity .time`
   instance with `isComplexity := True`).
2. `instance : HasQuantRep .time TimeComplexity_X where rep := <your g_X>`
   (e.g. `fun n => n`, `fun n => 2 ^ n`).
3. `instance : CComplexityRelationship .time TimeComplexity_X where`
   `minimalStrictlySmaller := [<pred>]`,
   `minimalStrictlyLarger := [<succ>]`, `equalTo := []`
   (equality only for degenerate envelopes like `poly 0 = O1`).
4. Prove the two Big-O facts (`BigO g_pred g_X`, `BigO g_X g_succ`;
   `X ≤ HALTS` then holds by forgetting) and the memberships
   `pred ∈ yourList`, `succ ∈ yourList` — see `ComplexityLinear.lean`.

**Layout (this file is a facade; code lives in `LeanC/Complexity/`):**
- `BigO.lean` — `BigO` def + kit (`refl`/`trans`/`const_le_one`/`add`/`max_bound`).
- `Growth.lean` — canonical reps (`gZero`/`g1`/`glog`/`gpoly`) + growth facts
  (incl. `bigO_one_le_zero` collapse witness, `gpoly_zero_eq_one`).
- `Classes.lean` — `ResourceAxis`, `CComplexity` + base inductives, `CComplexityRelationship`.
- `Lattice.lean` — `ComplexityTag`, `TagLE`, `HasTag`/`TagOf`, `ComplexityLE` edges
  (incl. `complexityLE_o1_not_zero` separation witness).
- `Graph.lean` — `CComplexityGraph`, insertion predicates, `baseGraph`,
  `can_insert_log/poly/poly_zero`.
- `Quant.lean` — open extension (`HasQuantRep`, `QuantLE`, `can_insert_quant_to_list`,
  `quant_poly0_le_o1/o1_le_poly0`) + recipe.
- `Bridge.lean` — `costInClass` + `costInClass_mono` (resources ↔ classes).

`import LeanC.Complexity` re-exports all of the above, so existing
importers (`Examples/ComplexityLinear`, `Tests/TestComplexity`) are unaffected.
-/
