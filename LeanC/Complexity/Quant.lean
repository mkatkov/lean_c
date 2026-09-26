import LeanC.Complexity.BigO
import LeanC.Complexity.Growth
import LeanC.Complexity.Classes

/-! ## Open extension — YES, classes are extendable without touching core files

Short answer to "are complexity classes extendable?": **yes**.
Every mechanism a new class needs is an *open typeclass*
(`CComplexity`, `CComplexityRelationship`, `HasQuantRep` below): any
new file can add instances for its own types. The only closed items
(`ComplexityTag`, `TagOf`, `TagLE` in `Lattice`) cover the base
vocabulary — and new quantitative classes deliberately do NOT need new
tags: their order is plain `BigO` on representatives (`QuantLE`).

Concrete proof: `Examples/ComplexityLinear.lean` adds `TimeComplexity_linear`
(`O(n)`, rep `fun n => n`) outside core (`Examples/`, not `LeanC/`) —
zero edits here — with both `BigO` edges and graph memberships.
Read it as the worked example.

### Recipe (copy into your file; 7 steps, ~35 lines)

```lean
-- 1. The marker type (empty inductive: classes are static knowledge).
inductive TimeComplexity_X where | mk
-- 2. Register the axis (time shown; memory uses `.memory`).
instance : CComplexity .time TimeComplexity_X where isComplexity := True
-- 3. Give the envelope (the mathematical content of your class).
instance : HasQuantRep .time TimeComplexity_X where rep := g_X
-- 4. Declare intent: immediate preds/succs (must match step 5's proofs).
--    The only equality cases in base are `poly 0 = O1` on both axes
--    (`equalTo := [O1]`, no strict neighbours); every genuinely new
--    growth rate uses `[]`.
instance : CComplexityRelationship .time TimeComplexity_X where
  minimalStrictlySmaller := [<pred>]   -- e.g. [TimeComplexity_O1]
  minimalStrictlyLarger := [<succ>]    -- e.g. [TimeComplexity_HALTS]
  equalTo := []
-- 5. Prove the two growth facts on EXPLICIT rep functions
--    (no typeclasses on variables, so no coherence trap):
--    `BigO g_pred g_X` and (`BigO g_X g_succ`, or: any `BigO g_X g`
--    already places X below HALTS by forgetting — say so in a comment).
--    Equality cases prove mutual `BigO` both ways instead
--    (see `quant_poly0_le_o1` / `quant_o1_le_poly0`).
--    Strict `<` claims additionally need the reverse non-inclusion
--    `¬ BigO g_X g_pred` (see `StrictQuantBelow` below and
--    `not_bigO_log_le_one`); without it the insertion shows `≤` only.
-- 6. Prove `StrictQuantBelow` for each strict edge (pairs step 5's `≤`
--    with its `≰` witness), unless the successor is qualitative
--    (`HALTS`/`BOUNDED` have no rep, so strictness there is vacuous
--    forgetting).
-- 7. Prove `can_insert_quant_to_list` memberships (preds/succs/equalities
--    in your list) with `List.Mem.head` / `List.Mem.tail` constructors.
```

### WHY the recipe is shaped this way (four traps it dodges)

1. **Why explicit rep functions in step 5, not
   `∀ p [HasQuantRep p], BigO (rep p) …`?** An instance-implicit over a
   *variable* `p` hands you an *arbitrary* instance transported to the
   concrete predecessor — with no uniqueness guarantee it equals the
   canonical one, so `rep_p = g_pred` is unprovable. `∃`-witnesses and
   explicit functions (`glog`, `glinear`) keep every proof about
   canonical, named terms.
2. **Why memberships via `Mem.head`/`Mem.tail`, never `decide`/`rfl` on
   types?** `a ∈ b :: l` splits into `head` (unifies the variable with
   the head — no equality to discharge) or `tail` (recurse). Proving
   `A ≠ B` for distinct single-constructor inductives is *unprovable*
   (`DecidableEq (Type)` does not exist; there is no distinguishing
   property), so anything needing type disequality — including a
   `τ ∉ τs` freshness clause — is deliberately absent from the open
   path. Freshness holds by construction: the new inductive is a new
   constant postdating every list it joins.
3. **Why is there no `τ ∉ τs` check?** Same reason: unstatable without
   type disequalities. The closed base predicate checks freshness at
   *tag* level (`DecidableEq` on data); the open path covers genuinely
   new growth rates whose reps differ, and documents equality cases
   (`equalTo`) in the relationship table instead of rejecting them.
4. **Why does a strict `<` claim need a `¬ BigO` witness on top of the
   `BigO` edge?** Tag-level `tp ≠ tτ` (`decide` on closed tags) is
   SYNTACTIC freshness only — it says "different constructors", not
   "different growth rates". A duplicate envelope under a fresh tag
   would pass `≤` checks both ways. Semantic strictness is
   `StrictQuantBelow` (`BigO` one way, `¬ BigO` the other); the closed
   predicate does NOT check it (its `HALTS`/`BOUNDED` successors have no
   reps to negate against), so every quantitative–quantitative `<` must
   carry its diagonal proof alongside the insertion (see
   `not_bigO_log_le_one`, `not_bigO_poly_le_one`, `not_bigO_sq_le_log`).
-/

namespace LeanC

/-- WHAT `HasQuantRep` is: THE extension point. An open typeclass giving
a class type its representative envelope `rep : Nat → Nat`, i.e. the
`g` in `Class(g) = { f | BigO f g }`. WHY open (class, not inductive):
any file can add instances for new types — no base edits. WHY axis is
explicit: time and memory envelopes compose differently, so a rep is
meaningless without saying which resource it bounds. Base quantitative
classes get instances below; qualitative ones (`HALTS`, `UNBOUND`,
`UNDECIDABLE`, `BOUNDED`, `GROWING`, `UNKNOWN`) deliberately have NONE —
they sit outside Big-O (see `Bridge`: finite membership is existential,
divergence awaits a partial-cost model).

NOTE on `Zero`: `Zero` has NO `HasQuantRep` on either axis, deliberately.
Both `BigO gZero g1` (`bigO_zero_le_one`) and `BigO g1 gZero`
(`bigO_one_le_zero`) hold as arithmetic facts, so any rep-based order
would equate `Zero` and `O1` both ways and collapse the bottom at value
level too (a constant-`1` cost would be `BigO`-inside `gZero`). `Zero`
membership is therefore NOT `costInClass` — it is the pointwise
`costInZero` predicate in `Bridge` (`cost = gZero`). Lattice strictness
`Zero < O1` lives ONLY in `TagLE` (stipulated `True`, no `o1 → zero`
row), never in reps. There is no `QuantLE` involving `Zero` at all. -/
class HasQuantRep (axis : ResourceAxis) (α : Type) where
  rep : Nat → Nat

instance : HasQuantRep .time TimeComplexity_O1 where rep := g1
instance : HasQuantRep .time TimeComplexity_log where rep := glog
instance (k : Nat) : HasQuantRep .time (TimeComplexity_poly k) where
  rep := gpoly k
instance : HasQuantRep .memory MemoryComplexity_O1 where rep := g1
instance : HasQuantRep .memory MemoryComplexity_log where rep := glog
instance (k : Nat) : HasQuantRep .memory (MemoryComplexity_poly k) where
  rep := gpoly k

/-- WHAT `QuantLE` is: ordering for the open world — plain `BigO` on
canonical reps, no tags. WHY: a new file's `exact bigO_…` proof term
*is* the edge; nothing to look up, nothing closed to extend.
`QuantLE .time O1 X` unfolds to `BigO g1 g_X` — prove it, done. -/
def QuantLE {axis : ResourceAxis} (a b : Type)
    [HasQuantRep axis a] [HasQuantRep axis b] : Prop :=
  BigO (HasQuantRep.rep (axis := axis) (α := a))
    (HasQuantRep.rep (axis := axis) (α := b))

/-- WHY: every `QuantLE` is reflexive — `X ≤ X` needs no growth fact,
just `BigO.refl`. Used for the guard `1 =O 1` and reflexive closes. -/
theorem QuantLE.refl {axis : ResourceAxis} (a : Type) [HasQuantRep axis a] :
    QuantLE (axis := axis) a a :=
  BigO.refl _

/-- WHY: `QuantLE` chains — the open-world counterpart of `TagLE_trans`
for the quantitative fragment. Inserting `X` between `P < S` and later
`Y` between `X < S` composes via this, never touching base rows. -/
theorem QuantLE.trans {axis : ResourceAxis} {a b c : Type}
    [HasQuantRep axis a] [HasQuantRep axis b] [HasQuantRep axis c] :
    QuantLE (axis := axis) a b → QuantLE (axis := axis) b c →
      QuantLE (axis := axis) a c :=
  BigO.trans

/-- WHAT `StrictQuantBelow` is: semantic strictness for a quantitative
`<` edge — `BigO` one way plus the reverse non-inclusion. WHY a
separate def (not folded into `QuantLE` or the insertion predicates):
tag-level `≠` is syntactic freshness (`decide` on constructors), and the
closed insertion predicate must also accept quantitative→qualitative
edges (`log ≤ HALTS`) where the successor has no rep to negate against.
So `≤` (insertion) and `<` (growth separation) are proved side by side:
the insertion carries the `BigO` facts, and each quant–quant `<` carries
its `StrictQuantBelow` witness. Equality cases (`poly 0 = O1`) prove
mutual `BigO` both ways instead and must NOT prove this. -/
def StrictQuantBelow {axis : ResourceAxis} (a b : Type)
    [HasQuantRep axis a] [HasQuantRep axis b] : Prop :=
  QuantLE (axis := axis) a b ∧ ¬ QuantLE (axis := axis) b a

theorem strict_o1_log_time :
    StrictQuantBelow (axis := .time) TimeComplexity_O1 TimeComplexity_log :=
  ⟨bigO_one_le_log, not_bigO_log_le_one⟩

theorem strict_o1_log_mem :
    StrictQuantBelow (axis := .memory) MemoryComplexity_O1 MemoryComplexity_log :=
  ⟨bigO_one_le_log, not_bigO_log_le_one⟩

theorem strict_o1_poly_time {k : Nat} (hk : 1 ≤ k) :
    StrictQuantBelow (axis := .time) TimeComplexity_O1 (TimeComplexity_poly k) :=
  ⟨bigO_one_le_poly k, fun h => not_bigO_poly_le_one hk h⟩

theorem strict_o1_poly_mem {k : Nat} (hk : 1 ≤ k) :
    StrictQuantBelow (axis := .memory) MemoryComplexity_O1 (MemoryComplexity_poly k) :=
  ⟨bigO_one_le_poly k, fun h => not_bigO_poly_le_one hk h⟩

theorem strict_log_poly_time {k : Nat} (hk : 2 ≤ k) :
    StrictQuantBelow (axis := .time) TimeComplexity_log (TimeComplexity_poly k) := by
  constructor
  · exact bigO_log_le_poly (by omega)
  · intro h
    have h2 : BigO (gpoly 2) glog :=
      BigO.trans (bigO_poly_le_poly (by omega : 2 ≤ k)) h
    exact not_bigO_sq_le_log h2

theorem strict_log_poly_mem {k : Nat} (hk : 2 ≤ k) :
    StrictQuantBelow (axis := .memory) MemoryComplexity_log (MemoryComplexity_poly k) := by
  constructor
  · exact bigO_log_le_poly (by omega)
  · intro h
    have h2 : BigO (gpoly 2) glog :=
      BigO.trans (bigO_poly_le_poly (by omega : 2 ≤ k)) h
    exact not_bigO_sq_le_log h2

/-- `poly 0 = O1` both ways (pointwise `n^0 = 1`, no `funext`: the `BigO`
bound is proved at each `n` via `gpoly_zero_eq_one`). This is the
machine-checked side of the `equalTo := [O1]` relationship entry:
equality of classes IS mutual `BigO` inclusion, proved here, not asserted. -/
theorem quant_poly0_le_o1 : QuantLE (axis := .time) (TimeComplexity_poly 0) TimeComplexity_O1 := by
  show BigO (gpoly 0) g1
  refine ⟨1, 0, fun n _ => ?_⟩
  have h1 : (gpoly 0) n = 1 := gpoly_zero_eq_one n
  show (gpoly 0) n ≤ 1 * Nat.max (g1 n) 1
  rw [h1]
  show (1 : Nat) ≤ 1 * Nat.max ((fun _ => 1) n) 1
  simp

theorem quant_o1_le_poly0 : QuantLE (axis := .time) TimeComplexity_O1 (TimeComplexity_poly 0) :=
  bigO_one_le_poly 0

theorem quant_mem_poly0_le_o1 :
    QuantLE (axis := .memory) (MemoryComplexity_poly 0) MemoryComplexity_O1 := by
  show BigO (gpoly 0) g1
  refine ⟨1, 0, fun n _ => ?_⟩
  have h1 : (gpoly 0) n = 1 := gpoly_zero_eq_one n
  show (gpoly 0) n ≤ 1 * Nat.max (g1 n) 1
  rw [h1]
  show (1 : Nat) ≤ 1 * Nat.max ((fun _ => 1) n) 1
  simp

theorem quant_mem_o1_le_poly0 :
    QuantLE (axis := .memory) MemoryComplexity_O1 (MemoryComplexity_poly 0) :=
  bigO_one_le_poly 0

/-- WHAT `can_insert_quant_to_list` is: THE open-world insertion predicate
(unified with the closed `can_insert_complexity_class_to_graph` in
`Graph`, which additionally checks `TagOf`/`TagLE`/tag-freshness for base
types). `τ` may join knowledge `target` iff every declared predecessor,
successor, AND equality witness is already known in `target`
(memberships via `Mem.head`/`Mem.tail` constructors — no type
disequalities, see recipe note 2).

WHY memberships only (no `BigO` inside): the `BigO` evidence lives on
EXPLICIT rep functions (`BigO g_pred g_X`), not on typeclass-projected
`rep`s of quantified variables (coherence trap, recipe note 1). The caller
proves the `BigO` facts separately (e.g. `linear_above_o1`,
`linear_below_poly2`) and conjoins them with this predicate — see
`Examples/ComplexityLinear.lean:linear_inserted`. For `poly 0` the two
strict conjuncts are vacuous (`smaller`/`larger` are `[]`) and only the
`equalTo` membership (`O1 ∈ target`) is required. -/
def can_insert_quant_to_list {axis : ResourceAxis} (τ : Type)
    [CComplexity axis τ] [CComplexityRelationship axis τ]
    (target : List Type) : Prop :=
  (∀ p : Type, p ∈ CComplexityRelationship.minimalStrictlySmaller (axis := axis) (α := τ) → p ∈ target) ∧
  (∀ s : Type, s ∈ CComplexityRelationship.minimalStrictlyLarger (axis := axis) (α := τ) → s ∈ target) ∧
  (∀ e : Type, e ∈ CComplexityRelationship.equalTo (axis := axis) (α := τ) → e ∈ target)

end LeanC
