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

### Recipe (copy into your file; 6 steps, ~30 lines)

```lean
-- 1. The marker type (empty inductive: classes are static knowledge).
inductive TimeComplexity_X where | mk
-- 2. Register the axis (time shown; memory uses `.memory`).
instance : CComplexity .time TimeComplexity_X where isComplexity := True
-- 3. Give the envelope (the mathematical content of your class).
instance : HasQuantRep .time TimeComplexity_X where rep := g_X
-- 4. Declare intent: immediate preds/succs (must match step 5's proofs).
instance : CComplexityRelationship .time TimeComplexity_X where
  minimalStrictlySmaller := [<pred>]   -- e.g. [TimeComplexity_O1]
  minimalStrictlyLarger := [<succ>]    -- e.g. [TimeComplexity_HALTS]
  equalTo := []
-- 5. Prove the two growth facts on EXPLICIT rep functions
--    (no typeclasses on variables, so no coherence trap):
--    `BigO g_pred g_X` and (`BigO g_X g_succ`, or: any `BigO g_X g`
--    already places X below HALTS by forgetting — say so in a comment).
-- 6. Prove memberships `pred ∈ yourList`, `succ ∈ yourList` with
--    `List.Mem.head` / `List.Mem.tail` constructors.
```

### WHY the recipe is shaped this way (three traps it dodges)

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
-/

namespace LeanC

/-- WHAT `HasQuantRep` is: THE extension point. An open typeclass giving
a class type its representative envelope `rep : Nat → Nat`, i.e. the
`g` in `Class(g) = { f | BigO f g }`. WHY open (class, not inductive):
any file can add instances for new types — no base edits. WHY axis is
explicit: time and memory envelopes compose differently, so a rep is
meaningless without saying which resource it bounds. Base quantitative
classes get instances below; qualitative ones (`HALTS`, `UNBOUND`,
`UNDECIDABLE`, …) deliberately have NONE — they sit outside Big-O. -/
class HasQuantRep (axis : ResourceAxis) (α : Type) where
  rep : Nat → Nat

instance : HasQuantRep .time ZeroTimeComplexity where rep := gZero
instance : HasQuantRep .time TimeComplexity_O1 where rep := g1
instance : HasQuantRep .time TimeComplexity_log where rep := glog
instance (k : Nat) : HasQuantRep .time (TimeComplexity_poly k) where
  rep := gpoly k
instance : HasQuantRep .memory ZeroMemoryComplexity where rep := gZero
instance : HasQuantRep .memory MemoryComplexity_O1 where rep := g1

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

end LeanC
