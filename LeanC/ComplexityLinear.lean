import LeanC.Complexity

/-!
# Extension demo — `TimeComplexity_linear`, i.e. O(n)

**WHAT this file is:** the worked answer to "are complexity classes
extendable?". It adds a brand-new quantitative class — linear time
(single pass over the input, e.g. `list map`, `fold`) — in its OWN
file, with **zero edits to `LeanC/Complexity.lean`**. If this file
compiles, extension works; it does (see `linear_inserted`).

**WHY linear, and WHERE it sits:** `O1 < O_log < O_linear < O_poly < HALTS`.
A halving loop is `log`; a single full scan is `linear`; nested loops
are `poly`. Linear is the missing rung a `map`/`fold` bound needs, and
it exercises both edge directions (above `log`, below `poly₂`).

**HOW it follows the 6-step recipe** (see `HasQuantRep` in
`Complexity.lean`; each step is marked `Step N` below):
1. `inductive TimeComplexity_linear` — the marker type.
2. `CComplexity .time` instance — registers the axis.
3. `HasQuantRep .time` with `rep := glinear` — the envelope.
4. `CComplexityRelationship` with `smaller := [O1]`,
   `larger := [HALTS]` — declares intent (mirrors `log`).
5. Three `BigO` facts on explicit reps — the evidence (no typeclass
   lookup on variables, so no coherence trap; see recipe note 1).
6. Memberships via `Mem.head`/`Mem.tail` — never `decide` on types
   (see recipe note 2).

**WHAT is deliberately NOT here:** a `τ ∉ list` freshness clause and a
new `ComplexityTag` constructor. Freshness holds by construction (this
inductive postdates every list it joins; type disequalities between
distinct single-constructor inductives are unprovable, so the open path
does not ask for them). Tags stay base-only on purpose — quantitative
order needs only reps (`QuantLE`), and `linear ≤ HALTS` additionally
holds by forgetting: `linear ≤ poly₂` (proved below) and `poly₂ ≤ HALTS`
(base table) chain to it. Strictness (`log < linear`, `linear < poly`
proper, by the same diagonal argument as `not_bigO_log_le_one`) is left
as future work — see `linear_strictness_note` below.
-/

namespace LeanC

-- Step 3 (envelope first, so the instances below can name it).
/-- WHAT `glinear` is: the representative `n ↦ n` — cost grows with the
input, one step per element. WHY `fun n => n` and not `gpoly 1`
(`fun n => n ^ 1`): extensionally equal but intensionally distinct terms;
naming the rung keeps `map`/`fold` bounds readable (`O(n)`, not
`O(n^1)`), and the `BigO` proofs below go through `Nat` directly
instead of unfolding `HPow`. -/
def glinear : Nat → Nat := fun n => n

-- Step 1. WHAT: the marker (empty inductive — classes are static
-- knowledge; meaning lives in `glinear` + the proofs, not in values).
inductive TimeComplexity_linear where | mk

-- Step 2. WHAT: registers linear as a *time* class. WHY needed: every
-- lattice API (`CComplexityRelationship`, `HasQuantRep`, graphs) is
-- axis-indexed, so the axis determines which chain the class joins.
instance : CComplexity .time TimeComplexity_linear where
  isComplexity := True

-- Step 3 (instance). WHAT: the envelope claim — linear *means*
-- `BigO cost glinear`. WHY a separate instance from `CComplexity`:
-- axis registration (step 2) says *which* chain; this says *where* on
-- it. Qualitative classes (`HALTS`, …) have step 2 but no step 3 — that
-- absence is exactly "outside Big-O".
instance : HasQuantRep .time TimeComplexity_linear where
  rep := glinear

-- Step 4. WHAT: declares intent — linear sits between `O1` and `HALTS`.
-- WHY honest, not `[]`: an empty predecessor list would claim "nothing
-- is below linear", contradicting the `O1 ≤ linear` proof in step 5.
-- Mirrors `log` deliberately: a second class in the same gap shows the
-- gap accepts many insertions without disturbing existing rows.
instance : CComplexityRelationship .time TimeComplexity_linear where
  minimalStrictlySmaller := [TimeComplexity_O1]
  minimalStrictlyLarger := [TimeComplexity_HALTS]
  equalTo := []

-- Step 5, edge 1. WHAT: `O1 ≤ linear` (`1 =O n`). WHY this proof shape:
-- `1 ≤ max n 1` is just the guard (`le_max_right`) — constants sit
-- below every growing envelope, same one-liner as `bigO_one_le_log`.
theorem linear_above_o1 : BigO g1 glinear := by
  refine ⟨1, 0, fun n _ => ?_⟩
  show (1 : Nat) ≤ 1 * Nat.max n 1
  rw [Nat.one_mul]
  exact Nat.le_max_right _ _

-- Step 5, edge 2. WHAT: `log ≤ linear`. WHY it reuses base lemmas:
-- `log2(n+1) ≤ n` is exactly `log_succ_le_self`, and `n ≤ max n 1`
-- is the guard — so log below linear is one `calc` hop, no new growth
-- argument. This is the payoff of grounding the lattice in `=O`.
theorem linear_above_log : BigO glog glinear := by
  refine ⟨1, 0, fun n _ => ?_⟩
  show Nat.log2 (n + 1) ≤ 1 * Nat.max n 1
  rw [Nat.one_mul]
  calc Nat.log2 (n + 1) ≤ n := log_succ_le_self n
    _ ≤ Nat.max n 1 := Nat.le_max_left _ _

-- Step 5, edge 3. WHAT: `linear ≤ poly₂` (`n =O n²`) — the finiteness
-- witness: exhibiting *any* polynomial envelope places linear below
-- `HALTS` by forgetting (via base `poly₂ ≤ HALTS`). WHY `self_le_sq`:
-- `n ≤ n² ≤ max (n²) 1`, constant `1`, same chain as `bigO_log_le_sq`'s
-- second half.
theorem linear_below_poly2 : BigO glinear (gpoly 2) := by
  refine ⟨1, 0, fun n _ => ?_⟩
  show n ≤ 1 * Nat.max (n ^ 2) 1
  rw [Nat.one_mul]
  calc n ≤ n ^ 2 := self_le_sq n
    _ ≤ Nat.max (n ^ 2) 1 := Nat.le_max_left _ _

-- Open-world ordering statements (no tags involved — `QuantLE` unfolds
-- to the `BigO` facts above definitionally).
example : QuantLE (axis := .time) TimeComplexity_O1 TimeComplexity_linear :=
  linear_above_o1
example : QuantLE (axis := .time) TimeComplexity_log TimeComplexity_linear :=
  linear_above_log

-- Step 6 (target knowledge). WHAT: the list linear joins — base
-- knowledge plus `log` and `poly₂`, so the bonus edges (`log ≤ linear`,
-- `linear ≤ poly₂`) point at already-known classes. WHY prepend rather
-- than append: `insert` conses, so `log :: poly₂ :: baseTimeMem` matches
-- the nested-`insert` value below definitionally.
def linearList : List Type :=
  TimeComplexity_log :: TimeComplexity_poly 2 :: baseTimeMem

def linearGraph : CComplexityGraph linearList :=
  CComplexityGraph.insert (axis := .time) TimeComplexity_log
    (CComplexityGraph.insert (axis := .time) (TimeComplexity_poly 2) baseGraph)

-- Step 6 (insertion accepted). WHAT: the package — both `BigO` edges
-- plus memberships of the declared preds/succs (`O1`, `HALTS`) in the
-- target knowledge. WHY memberships via `Mem.head`/`Mem.tail`
-- constructors: `O1` is `linearList[3]`, `HALTS` is `linearList[4]`
-- (2 prepended + base offsets), so 3 resp. 4 `tail`s then `head` —
-- pure constructors, no type disequalities anywhere.
theorem linear_inserted :
    BigO g1 glinear ∧ BigO glinear (gpoly 2) ∧ BigO glog glinear ∧
    TimeComplexity_O1 ∈ linearList ∧ TimeComplexity_HALTS ∈ linearList := by
  refine ⟨linear_above_o1, linear_below_poly2, linear_above_log, ?memO1, ?memHalts⟩
  · simp only [linearList, baseTimeMem]
    exact List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _)))
  · simp only [linearList, baseTimeMem]
    exact List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _))))

-- NOTE (`linear_strictness_note`): strictness (`¬ BigO glinear glog`,
-- i.e. `n ≠O log n`, and `¬ BigO (gpoly 2) glinear`) is NOT proved here.
-- WHY left out: it needs a diagonal growth argument (for every `c`,
-- exhibit `n` with `n > c * max (log2(n+1)) 1`), same shape as the base
-- `not_bigO_log_le_one` but with fresh constants — ~15 lines each and
-- orthogonal to the insertion mechanism, which only needs `≤`. The `≤`
-- edges above already place linear strictly between the two envelopes
-- modulo that routine diagonal; proving it would not change any API.

end LeanC
