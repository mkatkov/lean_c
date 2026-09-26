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
exhibiting Big-O inclusions plus strictness diagonals — no new
metatheory per class.
The local `BigO` (`Complexity/BigO.lean`) is the `Nat → Nat` analogue of
`Asymptotics.IsBigO Filter.atTop`: interface-compatible in spirit only
(Mathlib is norm-based over general types, so migration would rework
proofs via `Nat → ℝ` coercions; only the kit interface is stable).

**The two axes:** time (`Zero < O1 < log < poly < HALTS`, plus
`UNBOUND` on the side, all below `UNDECIDABLE`) and memory (mirror:
`Zero < O1 < log < poly < BOUNDED`, plus `GROWING`, below `UNKNOWN` —
memory quantitative classes live in the open `HasQuantRep` world, base
tags stay closed). Either axis
may independently be unknown. `O1` means "bounded by *some fixed
constant* (`≤ K`), never "exactly 1 step" — one Lean op may become
several C ops, and `=O` absorbs constants, which is what keeps
codegen sound. `Zero` is separated from `BigO` by stipulation AND by
construction: `BigO`
with the `max · 1` guard equates `0` and `1` (both `bigO_zero_le_one`
and `bigO_one_le_zero` hold), so `TagLE` stores every `Zero` edge as
`True` and exposes no `O1 ≤ Zero` row — see `Lattice.lean` — and `Zero`
has NO `HasQuantRep`, so there is no `QuantLE` involving `Zero` at all;
value-level `Zero` is the pointwise `costInZero` predicate (`Bridge`),
with `not_costInZero_time_one` as the regression that constant-`1` is
`O1` but not `Zero`. `poly 0`
is degenerate (`gpoly 0 = 1` pointwise, no `funext`): it is recorded as
*equal* to `O1` (`equalTo`, mutual `BigO` both ways), not strictly above.

**Qualitative ceiling, formalized where possible:** `HALTS`/`BOUNDED`
mean `IsFiniteCost` (`∃ g, BigO cost g` — `Bridge`); every
`costInClass` promotes via its own rep (`costInClass_to_halts/bounded`),
which derives the tag-table `True` rows quant→qual. `UNBOUND`/`GROWING`
mean `IsDivergentCost` (`¬ ∃ g, …`), currently EMPTY under total
`Nat → Nat` costs (`no_divergent_cost` via `BigO.refl`): true divergence
awaits a partial-cost/trace model, so `UNBOUND`-involving tag edges stay
stipulated. `UNDECIDABLE`/`UNKNOWN` are `True` (both sides forget).

**Extended resource model (generic core + per-resource bridge):**
core (`LeanC/Context.lean`) defines only the generic mechanism —
`CResource` combine ops, `HasCost` extraction (`cost : R → Nat → Nat`),
type-keyed `RStore`. Classes live here. The seam is
`costInClass` (`Complexity/Bridge.lean`): `v` is in `C` iff
`BigO (cost v) (rep C)`, transporting along `QuantLE` via `BigO.trans`;
promotion to the qualitative ceiling goes via `costInClass_to_halts` /
`costInClass_to_bounded` (existential forgetting, since `HALTS` has no
rep). Per-resource composition (`seq` adds time / maxes memory, `branch`
maxes + `O1` guard) is proved per resource via `BigO.add`/`max_bound` —
see `Examples/Resources.lean` (`time_seq_preserves`,
`time_branch_preserves`, `mem_seq_preserves`, `energy_*`, `exact_*`,
`O1+O1=O1`, `costInZero` vs `O1`). No hardwired `ResourceBound` pair in
core; new resources add a type + instances + preservation lemmas.

**How ordering is stored (read this before extending):**
- `ComplexityTag` (`Complexity/Lattice.lean`) is a *closed* snapshot of
  the base lattice (12 tags: 7 time + 5 memory) used for decidable checks
  (`decide`) and the `LE`
  instance. Base tags never change shape — new quantitative classes do
  NOT need new tags (see below), including memory `log`/`poly` which live
  purely in the open world.
- `HasTag` / `TagOf` map the base class *types* to those tags.
- `HasQuantRep` (open typeclass, `Complexity/Quant.lean`) is the
  **extension point**: any new file can give a new class type a
  representative function `rep : Nat → Nat` without touching this module.
  Ordering for such classes is plain `BigO` on reps (`QuantLE`), and
  insertion needs no new machinery: prove the `BigO` edges plus
  strictness (`StrictQuantBelow`: the reverse `¬ BigO`) plus the
  list memberships. Worked examples: `Examples/ComplexityLinear.lean`
  adds `TimeComplexity_linear` (`rep = fun n => n`) in its own file —
  no base edits, not core library; memory `log`/`poly` (`Graph`:
  `mem_log_inserted`, `mem_poly_inserted`) show the same open path on the
  memory axis.
- `CComplexityRelationship` (open typeclass, `Complexity/Classes.lean`)
  records immediate predecessors/successors/equalities; the
  *evidence* for a quantitative `≤` edge is always the `BigO` proof,
  never an assertion, and a `<` claim additionally needs its `¬ BigO`
  diagonal (`StrictQuantBelow` — tag-level `≠` is syntactic freshness
  only). Base equalities are `poly 0 = O1` on both axes.
- `CComplexityGraph` (`Complexity/Graph.lean`) is just the knowledge base
  (list of known class types). `LE` on same-list graphs is intra-list
  equivalence (`True`); knowledge growth across lists is `graphInclusion`
  (`refl`/`trans`/`cons`); the real class order lives on tags/reps.
  `can_insert_complexity_class_to_graph` (spelled `graph`; the old
  `grapth` spelling remains as a deprecated alias) is the closed predicate
  for the base tags; new classes (including memory quantitative classes)
  are inserted via the open
  predicate `can_insert_quant_to_list` (`Complexity/Quant.lean`: same
  memberships, `BigO` evidence on explicit reps, no tags) instead.
- `costInClass` / `costInZero` / `IsFiniteCost` (`Complexity/Bridge.lean`)
  bridge resource *values* to classes (`BigO (cost v) rep`, mono along
  `QuantLE`; `Zero` pointwise; finite existential).

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
   `X ≤ HALTS` then holds by forgetting via `costInClass_to_halts`) plus
   strictness (`StrictQuantBelow`: the reverse `¬ BigO` for each
   quant–quant `<`; qualitative successors need none) and the memberships
   `pred ∈ yourList`, `succ ∈ yourList` — see `ComplexityLinear.lean`
   (`strict_o1_linear`, `strict_linear_poly2`, `linear_inserted`).

**Layout (this file is a facade; code lives in `LeanC/Complexity/`):**
- `BigO.lean` — `BigO` def + kit (`refl`/`trans`/`const_le_one`/`add`/`max_bound`).
- `Growth.lean` — canonical reps (`gZero`/`g1`/`glog`/`gpoly`) + growth facts
  (incl. `bigO_one_le_zero` collapse witness, `gpoly_zero_eq_one`,
  `self_le_pow`, strictness `not_bigO_*`).
- `Classes.lean` — `ResourceAxis`, `CComplexity` + base inductives (time
  `Zero/O1/log/poly/HALTS/UNBOUND/UNDECIDABLE`, memory
  `Zero/O1/log/poly/BOUNDED/GROWING/UNKNOWN`), `CComplexityRelationship`.
- `Lattice.lean` — `ComplexityTag` (12 base tags), `TagLE`, `HasTag`/`TagOf`,
  `ComplexityLE` edges (incl. `complexityLE_o1_not_zero` separation witness
  + symmetric incomparabilities).
- `Graph.lean` — `CComplexityGraph`, `graphInclusion` preorder, insertion
  predicates, `baseGraph`, `can_insert_log/poly/poly_zero` (closed, time),
  `mem_log/poly_inserted` (open, memory), base membership helpers.
- `Quant.lean` — open extension (`HasQuantRep` (no `Zero`), `QuantLE`,
  `StrictQuantBelow` + `strict_*`, `can_insert_quant_to_list`,
  `quant_poly0_le_o1/o1_le_poly0` + memory pair, no-funext proofs) + recipe.
- `Bridge.lean` — `costInClass` + `costInClass_mono`, `costInZero`,
  `IsFiniteCost`/`IsDivergentCost` + `costInHalts/Bounded/...` promotion
  (resources ↔ classes).

`import LeanC.Complexity` re-exports all of the above, so existing
importers (`Examples/ComplexityLinear`, `Tests/TestComplexity`) are unaffected.
-/
