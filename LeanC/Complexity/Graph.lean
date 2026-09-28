import LeanC.Complexity.BigO
import LeanC.Complexity.Growth
import LeanC.Complexity.Classes
import LeanC.Complexity.Lattice
import LeanC.Complexity.Quant

/-! Knowledge-base graphs + the closed insertion predicate, with
`can_insert_log` as the worked core example.

Split out of `LeanC/Complexity.lean`. This is the noisiest section
(freshness enumerates every base tag), which is why it gets its own
file. See `Quant` for the open extension path used by new files. -/
namespace LeanC
universe u

/-- WHAT the graph is: just the knowledge base — the *list* of known
class types. WHY a bare list (not a rich structure): all meaning lives
in the relationship tables + `BigO` proofs; the graph only records
*which* classes exist, so insertion is *additive* (cons one more type)
and existing entries are never rewritten. `insert` takes its `{axis}`
implicitly so one graph mixes both axes (each entry brings its own
`CComplexity axis` proof). -/
inductive CComplexityGraph : (List (Type u)) -> Type (u+1) where
  | nil : CComplexityGraph []
  | insert {τs : List (Type u)} {axis : ResourceAxis}
     (τ : Type u ) [CComplexity axis τ]
     (_: CComplexityGraph τs ): CComplexityGraph (τ :: τs)

/-- Intra-list equivalence (Fix 3 — explicit, NOT an `LE` instance).

Graphs with a fixed base list are all equivalent (same knowledge —
one value per list). Previously this was an `LE` instance with
`le _ _ := True`, which was a footgun: any `g₁ ≤ g₂` on the same list
typechecked, inviting misuse where the class order (`ComplexityLE` /
`QuantLE`) was intended. Now it is the explicit `graphEquiv`
(both directions trivially). Knowledge growth ACROSS lists is
`graphInclusion` below. If you are reaching for graph `≤`, you want
`ComplexityLE` or `QuantLE` instead. -/
def graphEquiv {α : List (Type u)} (_ _ : CComplexityGraph α) : Prop :=
  True

theorem graphEquiv_refl {α : List (Type u)} (g : CComplexityGraph α) :
    graphEquiv g g := trivial

/-- WHAT cross-base inclusion means: every class known in `l₁` is known
in `l₂`. WHY: extending knowledge (`base → base + log`) is list
growth; this is the Prop that growth respects. `refl`/`trans` make it a
preorder on lists; `graphInclusion_cons` is the one-step growth (the
head of `τ :: l₁` is new, the tail is the old knowledge). -/
def graphInclusion (l₁ l₂ : List (Type u)) : Prop :=
  ∀ τ ∈ l₁, τ ∈ l₂

theorem graphInclusion_refl (l : List (Type u)) : graphInclusion l l :=
  fun _ h => h

theorem graphInclusion_trans {l₁ l₂ l₃ : List (Type u)} :
    graphInclusion l₁ l₂ → graphInclusion l₂ l₃ → graphInclusion l₁ l₃ :=
  fun h12 h23 _ h => h23 _ (h12 _ h)

theorem graphInclusion_cons (τ : Type u) (l : List (Type u)) :
    graphInclusion l (τ :: l) :=
  fun _ h => List.Mem.tail _ h

theorem graphInclusion_head_mem (τ : Type u) (l : List (Type u)) :
    τ ∈ (τ :: l) :=
  List.Mem.head _

/-- WHAT insertion acceptance means (closed, base-tag version): `τ` may
join knowledge `τs` iff (1) every declared predecessor already sits
*below* it in `TagLE`, (2) it sits below every declared successor, (3)
its tag is fresh in `τs`, (4–5) preds/succs are already known, (6)
every declared equality witness is already known.
WHY each conjunct:
- (1–2) are the two `≤` facts — the *witness*, not an assertion.
  Quantitative–quantitative (`O1 ≤ log`) is a `BigO`
  proof term; quantitative→qualitative (`log ≤ HALTS`) is `True`
  (forgetting needs no numbers — the value-level shadow is
  `costInClass → IsFiniteCost` in `Bridge`). The accompanying `tp ≠ tτ`
  (`decide`d) is SYNTACTIC freshness only (different constructors), NOT
  semantic strictness: a duplicate envelope under a fresh tag would pass
  here. Genuine `<` additionally needs `StrictQuantBelow` (a `¬ BigO`
  witness, see `Quant` + `not_bigO_log_le_one`/`not_bigO_poly_le_one`/
  `not_bigO_sq_le_log`); the predicate does not check it because
  qualitative successors (`HALTS`/`BOUNDED`) have no reps to negate
  against. `Zero` edges are `True` by stipulation (see `Lattice`: `BigO`
  equates `0`/`1`, so `Zero` never orders via `BigO`, and `Zero` has no
  `HasQuantRep` at all).
- (3) is tag-level freshness (`∃ tp … tp ≠ tτ`, *constructed* per
element) — deliberately NOT `τ ∉ τs`: `¬ (A = B)` for distinct
single-constructor inductives is unprovable in Lean (no distinguishing
property for `cases`/`decide` to exploit; `DecidableEq (Type)` does
not exist), so type-level non-membership cannot be discharged and is
not asked for. Tags are data (`DecidableEq`), so tag freshness is.
- (4–6) need only `Mem.head`/`Mem.tail` constructors (`rfl`-style, no
disequalities) — that is WHY they are provable at all. (6) is vacuous
for all base classes except `poly 0` (`equalTo := [O1]` on time,
`[O1Mem]` on memory); it forces the equality witness to be known.
For new classes use the open `can_insert_quant_to_list`
(`HasQuantRep`, `Quant`) instead — same memberships (4–6), `BigO`
evidence on explicit reps, no tags needed; this closed predicate covers
the base vocabulary the tests close over. -/
def can_insert_complexity_class_to_graph {τs : List Type} {axis : ResourceAxis}
  (τ : Type) [CComplexity axis τ] [CComplexityRelationship axis τ]
  (_ : CComplexityGraph τs) : Prop :=
  ∃ tτ, TagOf τ tτ ∧
    (∀ p : Type, p ∈ CComplexityRelationship.minimalStrictlySmaller (axis := axis) (α := τ) →
      ∃ tp, TagOf p tp ∧ TagLE tp tτ ∧ tp ≠ tτ) ∧
    (∀ s : Type, s ∈ CComplexityRelationship.minimalStrictlyLarger (axis := axis) (α := τ) →
      ∃ ts, TagOf s ts ∧ TagLE tτ ts ∧ tτ ≠ ts) ∧
    (∀ p, p ∈ τs → ∃ tp, TagOf p tp ∧ tp ≠ tτ) ∧
    (∀ p : Type, p ∈ CComplexityRelationship.minimalStrictlySmaller (axis := axis) (α := τ) → p ∈ τs) ∧
    (∀ s : Type, s ∈ CComplexityRelationship.minimalStrictlyLarger (axis := axis) (α := τ) → s ∈ τs) ∧
    (∀ e : Type, e ∈ CComplexityRelationship.equalTo (axis := axis) (α := τ) → e ∈ τs)

/-- Deprecated typo alias: `grapth` → `graph`. Kept so existing importers
outside this repo keep compiling; new code uses the correctly spelled
`can_insert_complexity_class_to_graph`. -/
abbrev can_insert_complexity_class_to_grapth {τs : List Type} {axis : ResourceAxis}
  (τ : Type) [CComplexity axis τ] [CComplexityRelationship axis τ]
  (g : CComplexityGraph τs) : Prop :=
  can_insert_complexity_class_to_graph (τs := τs) (axis := axis) τ g

/-- WHAT `baseTimeMem`/`baseGraph` are: the 10 base classes as a list
plus its knowledge-base value (nested `insert`s, axis given explicitly
so typeclass search never sees a stuck metavariable). WHY 10 not 12:
`baseTimeMem` is the qualitative spine only (5 time + 5 memory, NO
`log`/`poly` — they are quantitative insertions). `ComplexityTag` has
12 constructors (10 + `logTime` + `polyTime k` time rungs that predate
the open path; memory `log`/`poly` have NO tags by design). See Fix 4
coherence note in `Lattice` (`complexityLE_poly0_not_o1` /
`complexityLE_o1_poly0`): `poly 0 = O1` lives in `QuantLE`, not tags.
`can_insert_log` below shows the closed mechanism end to end, with the
two `BigO` proofs as evidence. -/
def baseTimeMem : List Type :=
  [ZeroTimeComplexity, TimeComplexity_O1, TimeComplexity_HALTS,
   TimeComplexity_UNBOUND, TimeComplexity_UNDECIDABLE,
   ZeroMemoryComplexity, MemoryComplexity_O1, MemoryComplexity_BOUNDED,
   MemoryComplexity_GROWING, MemoryComplexity_UNKNOWN]

def baseGraph : CComplexityGraph baseTimeMem :=
  CComplexityGraph.insert (axis := .time) ZeroTimeComplexity
    (CComplexityGraph.insert (axis := .time) TimeComplexity_O1
      (CComplexityGraph.insert (axis := .time) TimeComplexity_HALTS
        (CComplexityGraph.insert (axis := .time) TimeComplexity_UNBOUND
          (CComplexityGraph.insert (axis := .time) TimeComplexity_UNDECIDABLE
            (CComplexityGraph.insert (axis := .memory) ZeroMemoryComplexity
              (CComplexityGraph.insert (axis := .memory) MemoryComplexity_O1
                (CComplexityGraph.insert (axis := .memory) MemoryComplexity_BOUNDED
                  (CComplexityGraph.insert (axis := .memory) MemoryComplexity_GROWING
                    (CComplexityGraph.insert (axis := .memory) MemoryComplexity_UNKNOWN
                      CComplexityGraph.nil)))))))))

/-- WHAT `can_insert_log` shows: `O_log` splitting `O1 < HALTS` into
`O1 < O_log < HALTS`. WHY each bullet: preds `[O1]` → witness
`⟨o1Time, bigO_one_le_log, o1≠log⟩` (the growth fact); succs `[HALTS]`
→ `⟨haltsTime, trivial, log≠halts⟩` (forgetting); freshness → one
canonical tag per base element, each `≠ logTime` by `decide`;
memberships → `Mem.head`/`Mem.tail` constructors. No step asserts an
edge — every `≤` is a proof term. Copy this shape for your own class
(see the open recipe in `Quant`).

NOTE on placement: this stays in core (not `Examples/`) because
`TimeComplexity_log` itself is core vocabulary defined in `Classes`.
`Examples/ComplexityLinear.lean` is the template for *external*
extensions (new type, new file, zero base edits); this theorem is the
*internal* coherence check that the closed base predicate accepts a
core quantitative class. `Tests/TestComplexity.lean` references it by
name, so it is API, not a throwaway `example`. -/
theorem can_insert_log :
    can_insert_complexity_class_to_graph (axis := .time) (τ := TimeComplexity_log) baseGraph := by
  refine ⟨.logTime, .logTime, ?preds, ?succs, ?fresh, ?predMem, ?succMem, ?eqMem⟩
  · intro p hp
    cases hp with
    | head as =>
      refine ⟨.o1Time, .o1Time, bigO_one_le_log, by decide⟩
    | tail b h =>
      cases h
  · intro s hs
    cases hs with
    | head as =>
      refine ⟨.haltsTime, .haltsTime, trivial, by decide⟩
    | tail b h =>
      cases h
  · intro p hp
    cases hp with
    | head as =>
      exact ⟨.zeroTime, .zeroTime, by decide⟩
    | tail b h =>
      cases h with
      | head as =>
        exact ⟨.o1Time, .o1Time, by decide⟩
      | tail b h =>
        cases h with
        | head as =>
          exact ⟨.haltsTime, .haltsTime, by decide⟩
        | tail b h =>
          cases h with
          | head as =>
            exact ⟨.unboundTime, .unboundTime, by decide⟩
          | tail b h =>
            cases h with
            | head as =>
              exact ⟨.undecidableTime, .undecidableTime, by decide⟩
            | tail b h =>
              cases h with
              | head as =>
                exact ⟨.zeroMem, .zeroMem, by decide⟩
              | tail b h =>
                cases h with
                | head as =>
                  exact ⟨.o1Mem, .o1Mem, by decide⟩
                | tail b h =>
                  cases h with
                  | head as =>
                    exact ⟨.boundedMem, .boundedMem, by decide⟩
                  | tail b h =>
                    cases h with
                    | head as =>
                      exact ⟨.growingMem, .growingMem, by decide⟩
                    | tail b h =>
                      cases h with
                      | head as =>
                        exact ⟨.unknownMem, .unknownMem, by decide⟩
                      | tail b h =>
                        cases h
  · intro p hp
    cases hp with
    | head as =>
      exact List.Mem.tail _ (List.Mem.head _)
    | tail b h =>
      cases h
  · intro s hs
    cases hs with
    | head as =>
      exact List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _))
    | tail b h =>
      cases h
  · intro e he
    cases he

/-- WHAT `can_insert_poly` shows: any non-degenerate `poly k` (`1 ≤ k`)
splits `O1 < HALTS` exactly like `log` (`O1 < poly k < HALTS`, evidence
`bigO_one_le_poly k` + forgetting). The `1 ≤ k` hypothesis is what rules
out the degenerate `poly 0 = O1` case (see `can_insert_poly_zero`). -/
theorem can_insert_poly {k : Nat} (hk : 1 ≤ k) :
    can_insert_complexity_class_to_graph (axis := .time)
      (τ := TimeComplexity_poly k) baseGraph := by
  have hk0 : k ≠ 0 := by omega
  have hsmall := poly_small_eq k hk0
  have hlarge := poly_large_eq k hk0
  have hequal := poly_equal_eq k hk0
  refine ⟨.polyTime k, .polyTime k, ?preds, ?succs, ?fresh, ?predMem, ?succMem, ?eqMem⟩
  · intro p hp
    rw [hsmall] at hp
    cases hp with
    | head as =>
      refine ⟨.o1Time, .o1Time, bigO_one_le_poly k, by simp⟩
    | tail b h =>
      cases h
  · intro s hs
    rw [hlarge] at hs
    cases hs with
    | head as =>
      refine ⟨.haltsTime, .haltsTime, trivial, by simp⟩
    | tail b h =>
      cases h
  · intro p hp
    cases hp with
    | head as =>
      exact ⟨.zeroTime, .zeroTime, by simp⟩
    | tail b h =>
      cases h with
      | head as =>
        exact ⟨.o1Time, .o1Time, by simp⟩
      | tail b h =>
        cases h with
        | head as =>
          exact ⟨.haltsTime, .haltsTime, by simp⟩
        | tail b h =>
          cases h with
          | head as =>
            exact ⟨.unboundTime, .unboundTime, by simp⟩
          | tail b h =>
            cases h with
            | head as =>
              exact ⟨.undecidableTime, .undecidableTime, by simp⟩
            | tail b h =>
              cases h with
              | head as =>
                exact ⟨.zeroMem, .zeroMem, by simp⟩
              | tail b h =>
                cases h with
                | head as =>
                  exact ⟨.o1Mem, .o1Mem, by simp⟩
                | tail b h =>
                  cases h with
                  | head as =>
                    exact ⟨.boundedMem, .boundedMem, by simp⟩
                  | tail b h =>
                    cases h with
                    | head as =>
                      exact ⟨.growingMem, .growingMem, by simp⟩
                    | tail b h =>
                      cases h with
                      | head as =>
                        exact ⟨.unknownMem, .unknownMem, by simp⟩
                      | tail b h =>
                        cases h
  · intro p hp
    rw [hsmall] at hp
    cases hp with
    | head as =>
      exact List.Mem.tail _ (List.Mem.head _)
    | tail b h =>
      cases h
  · intro s hs
    rw [hlarge] at hs
    cases hs with
    | head as =>
      exact List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _))
    | tail b h =>
      cases h
  · intro e he
    rw [hequal] at he
    cases he

/-- WHAT `can_insert_poly_zero` shows: the degenerate `poly 0` is NOT a
strict insertion — `smaller`/`larger` are `[]` (vacuous), and the only
obligation is the equality witness `O1 ∈ baseTimeMem` (`equalTo`). Pairs
with `quant_poly0_le_o1` / `quant_o1_le_poly0` (mutual `BigO`), which are
the semantic equality. -/
theorem can_insert_poly_zero :
    can_insert_complexity_class_to_graph (axis := .time)
      (τ := TimeComplexity_poly 0) baseGraph := by
  refine ⟨.polyTime 0, .polyTime 0, ?preds, ?succs, ?fresh, ?predMem, ?succMem, ?eqMem⟩
  · intro p hp
    rw [poly_small_zero] at hp
    cases hp
  · intro s hs
    rw [poly_large_zero] at hs
    cases hs
  · intro p hp
    cases hp with
    | head as =>
      exact ⟨.zeroTime, .zeroTime, by decide⟩
    | tail b h =>
      cases h with
      | head as =>
        exact ⟨.o1Time, .o1Time, by decide⟩
      | tail b h =>
        cases h with
        | head as =>
          exact ⟨.haltsTime, .haltsTime, by decide⟩
        | tail b h =>
          cases h with
          | head as =>
            exact ⟨.unboundTime, .unboundTime, by decide⟩
          | tail b h =>
            cases h with
            | head as =>
              exact ⟨.undecidableTime, .undecidableTime, by decide⟩
            | tail b h =>
              cases h with
              | head as =>
                exact ⟨.zeroMem, .zeroMem, by decide⟩
              | tail b h =>
                cases h with
                | head as =>
                  exact ⟨.o1Mem, .o1Mem, by decide⟩
                | tail b h =>
                  cases h with
                  | head as =>
                    exact ⟨.boundedMem, .boundedMem, by decide⟩
                  | tail b h =>
                    cases h with
                    | head as =>
                      exact ⟨.growingMem, .growingMem, by decide⟩
                    | tail b h =>
                      cases h with
                      | head as =>
                        exact ⟨.unknownMem, .unknownMem, by decide⟩
                      | tail b h =>
                        cases h
  · intro p hp
    rw [poly_small_zero] at hp
    cases hp
  · intro s hs
    rw [poly_large_zero] at hs
    cases hs
  · intro e he
    rw [poly_equal_zero] at he
    cases he with
    | head as =>
      exact List.Mem.tail _ (List.Mem.head _)
    | tail b h =>
      cases h

theorem complexityLE_o1_log_halts :
    ComplexityLE TimeComplexity_O1 TimeComplexity_log ∧
    ComplexityLE TimeComplexity_log TimeComplexity_HALTS :=
  ⟨complexityLE_o1_log, complexityLE_log_halts⟩

/-- Base memberships, centralized so extension files (`Examples/…`)
build target lists from these instead of hardcoding tail offsets twice.
/// Only these four break if `baseTimeMem` is reordered — callers stay
intact. -/
theorem o1_mem_base : TimeComplexity_O1 ∈ baseTimeMem := by
  simp only [baseTimeMem]
  exact List.Mem.tail _ (List.Mem.head _)

theorem halts_mem_base : TimeComplexity_HALTS ∈ baseTimeMem := by
  simp only [baseTimeMem]
  exact List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _))

theorem o1Mem_mem_base : MemoryComplexity_O1 ∈ baseTimeMem := by
  simp only [baseTimeMem]
  exact List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _
    (List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _))))))

theorem bounded_mem_base : MemoryComplexity_BOUNDED ∈ baseTimeMem := by
  simp only [baseTimeMem]
  exact List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _
    (List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _)))))))

/-- SOUND closed insertion (Fix 3): `can_insert_log` + semantic
strictness. The closed predicate alone checks `TagLE` (`BigO` for
quant–quant, `True` for quant→qual) + syntactic `≠`; this bundles the
`StrictQuantBelow` diagonal so a duplicate envelope cannot pass as
`<`. Qualitative successor needs no `¬ BigO` (no rep to negate —
forgetting via `costInClass_to_halts`). -/
theorem can_insert_log_sound :
    can_insert_complexity_class_to_graph (axis := .time)
      (τ := TimeComplexity_log) baseGraph ∧
    StrictQuantBelow (axis := .time) TimeComplexity_O1 TimeComplexity_log :=
  ⟨can_insert_log, strict_o1_log_time⟩

/-- SOUND closed insertion for `poly k` (`1 ≤ k`): memberships + growth. -/
theorem can_insert_poly_sound {k : Nat} (hk : 1 ≤ k) :
    can_insert_complexity_class_to_graph (axis := .time)
      (τ := TimeComplexity_poly k) baseGraph ∧
    StrictQuantBelow (axis := .time) TimeComplexity_O1 (TimeComplexity_poly k) :=
  ⟨can_insert_poly hk, strict_o1_poly_time hk⟩

/-- WHAT `mem_log_inserted` shows: memory mirror of `can_insert_log`
via the OPEN path (no new tags — base tags stay closed by design).
`O1Mem < O_logMem < BOUNDED`: `BigO` evidence is `bigO_one_le_log` plus
forgetting into `BOUNDED` (value-level: `costInClass_to_bounded` in
`Bridge`); strictness is `strict_o1_log_mem` (`Quant`); memberships reuse
the centralized `o1Mem_mem_base` / `bounded_mem_base` helpers so no tail
offsets are hardcoded at call sites.

SOUND form is `mem_log_inserted_sound` below
(`can_insert_quant_sound_below_qual` bundle); this legacy conjunction
is kept for compat. -/
theorem mem_log_inserted :
    BigO g1 glog ∧
    StrictQuantBelow (axis := .memory) MemoryComplexity_O1 MemoryComplexity_log ∧
    can_insert_quant_to_list (axis := .memory) MemoryComplexity_log baseTimeMem := by
  refine ⟨bigO_one_le_log, strict_o1_log_mem, ?predMem, ?succMem, ?eqMem⟩
  · intro p hp
    cases hp with
    | head as => exact o1Mem_mem_base
    | tail b h => cases h
  · intro s hs
    cases hs with
    | head as => exact bounded_mem_base
    | tail b h => cases h
  · intro e he
    cases he

/-- WHAT `mem_poly_inserted` shows: memory mirror of `can_insert_poly`
via the OPEN path (`O1Mem < polyMem k < BOUNDED` for `1 ≤ k`), with
strictness `strict_o1_poly_mem` / `strict_log_poly_mem`. -/
theorem mem_poly_inserted {k : Nat} (hk : 1 ≤ k) :
    BigO g1 (gpoly k) ∧
    StrictQuantBelow (axis := .memory) MemoryComplexity_O1 (MemoryComplexity_poly k) ∧
    can_insert_quant_to_list (axis := .memory) (MemoryComplexity_poly k) baseTimeMem := by
  have hk0 : k ≠ 0 := by omega
  have hsmall := mem_poly_small_eq k hk0
  have hlarge := mem_poly_large_eq k hk0
  have hequal := mem_poly_equal_eq k hk0
  refine ⟨bigO_one_le_poly k, strict_o1_poly_mem hk, ?predMem, ?succMem, ?eqMem⟩
  · intro p hp
    rw [hsmall] at hp
    cases hp with
    | head as => exact o1Mem_mem_base
    | tail b h => cases h
  · intro s hs
    rw [hlarge] at hs
    cases hs with
    | head as => exact bounded_mem_base
    | tail b h => cases h
  · intro e he
    rw [hequal] at he
    cases he

/-- WHAT `mem_poly_zero_inserted` shows: degenerate `polyMem 0 = O1Mem`
via the OPEN path (`equalTo := [O1Mem]`, strict conjuncts vacuous).
Pairs with `quant_mem_poly0_le_o1` / `quant_mem_o1_le_poly0` (mutual
`BigO`), the semantic equality. SOUND form is
`mem_poly_zero_inserted_sound` (`can_insert_quant_sound_equal`). -/
theorem mem_poly_zero_inserted :
    QuantLE (axis := .memory) (MemoryComplexity_poly 0) MemoryComplexity_O1 ∧
    QuantLE (axis := .memory) MemoryComplexity_O1 (MemoryComplexity_poly 0) ∧
    can_insert_quant_to_list (axis := .memory) (MemoryComplexity_poly 0) baseTimeMem := by
  refine ⟨quant_mem_poly0_le_o1, quant_mem_o1_le_poly0, ?predMem, ?succMem, ?eqMem⟩
  · intro p hp
    rw [mem_poly_small_zero] at hp
    cases hp
  · intro s hs
    rw [mem_poly_large_zero] at hs
    cases hs
  · intro e he
    rw [mem_poly_equal_zero] at he
    cases he with
    | head as => exact o1Mem_mem_base
    | tail b h => cases h

/-- SOUND open insertions (Fix 3 bundles — memberships + growth): -/
theorem mem_log_inserted_sound :
    can_insert_quant_sound_below_qual (axis := .memory)
      MemoryComplexity_O1 MemoryComplexity_log baseTimeMem :=
  ⟨bigO_one_le_log, strict_o1_log_mem, mem_log_inserted.2.2⟩

theorem mem_poly_zero_inserted_sound :
    can_insert_quant_sound_equal (axis := .memory)
      (MemoryComplexity_poly 0) MemoryComplexity_O1 baseTimeMem :=
  ⟨quant_mem_poly0_le_o1, quant_mem_o1_le_poly0, mem_poly_zero_inserted.2.2⟩

theorem mem_poly_inserted_sound {k : Nat} (hk : 1 ≤ k) :
    can_insert_quant_sound_below_qual (axis := .memory)
      MemoryComplexity_O1 (MemoryComplexity_poly k) baseTimeMem :=
  ⟨bigO_one_le_poly k, strict_o1_poly_mem hk, (mem_poly_inserted hk).2.2⟩

end LeanC
