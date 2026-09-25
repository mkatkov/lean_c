import LeanC.Complexity.BigO
import LeanC.Complexity.Growth
import LeanC.Complexity.Classes
import LeanC.Complexity.Lattice
import LeanC.Complexity.Graph
import LeanC.Complexity.Quant

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
codegen sound.

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
  records immediate predecessors/successors for documentation; the
  *evidence* for a quantitative edge is always the `BigO` proof, never
  an assertion.
- `CComplexityGraph` (`Complexity/Graph.lean`) is just the knowledge base
  (list of known class types). `LE` on same-base graphs is trivial
  (`True`); the real order lives on tags/reps.
  `can_insert_complexity_class_to_grapth` is the closed predicate for the
  base tags; new classes are inserted via the open `QuantLE` + membership
  path instead.

**How to add a new quantitative class `X` (copy-paste recipe):**
1. `inductive TimeComplexity_X where | mk` (+ `CComplexity .time`
   instance with `isComplexity := True`).
2. `instance : HasQuantRep .time TimeComplexity_X where rep := <your g_X>`
   (e.g. `fun n => n`, `fun n => 2 ^ n`).
3. `instance : CComplexityRelationship .time TimeComplexity_X where`
   `minimalStrictlySmaller := [<pred>]`,
   `minimalStrictlyLarger := [<succ>]`, `equalTo := []`.
4. Prove the two Big-O facts (`BigO g_pred g_X`, `BigO g_X g_succ`;
   `X ≤ HALTS` then holds by forgetting) and the memberships
   `pred ∈ yourList`, `succ ∈ yourList` — see `ComplexityLinear.lean`.

**Layout (this file is a facade; code lives in `LeanC/Complexity/`):**
- `BigO.lean` — `BigO` def + kit (`refl`/`trans`/`const_le_one`/`add`/`max_bound`).
- `Growth.lean` — canonical reps (`gZero`/`g1`/`glog`/`gpoly`) + growth facts.
- `Classes.lean` — `ResourceAxis`, `CComplexity` + base inductives, `CComplexityRelationship`.
- `Lattice.lean` — `ComplexityTag`, `TagLE`, `HasTag`/`TagOf`, `ComplexityLE` edges.
- `Graph.lean` — `CComplexityGraph`, insertion predicate, `baseGraph`, `can_insert_log`.
- `Quant.lean` — open extension (`HasQuantRep`, `QuantLE`) + recipe.

`import LeanC.Complexity` re-exports all of the above, so existing
importers (`Examples/ComplexityLinear`, `Tests/TestComplexity`) are unaffected.
-/
