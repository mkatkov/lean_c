import LeanC.Context
import LeanC.Complexity.BigO
import LeanC.Complexity.Growth
import LeanC.Complexity.Classes
import LeanC.Complexity.Quant

/-! Cost-function bridge: extended resource model meets `BigO` classes.

`Context` defines the generic mechanism (`CResource` combine ops,
`HasCost` extraction). `Complexity` defines the class envelopes
(`HasQuantRep` reps, `QuantLE`). This file is the seam between them:
a resource *value* is in a class iff its extracted cost function is
`BigO`-inside the class rep.

- `costInClass axis v C`: `BigO (cost v) (rep C)` — quantitative
  membership (only classes WITH a `HasQuantRep`: `O1/log/poly`, never
  `Zero`/`HALTS`/`UNBOUND`/…).
- `costInClass_mono`: membership transports along `QuantLE`
  (`BigO.trans`).
- `costInZero`: `cost v = gZero` (pointwise `0`) — the ONLY `Zero`
  membership. `Zero` has no `HasQuantRep` (see `Quant`): `costInClass`
  cannot even state it, so the `BigO g1 gZero` collapse cannot leak
  into values. `costInZero_to_o1` promotes it to `O1`.
- `IsFiniteCost` / `costInHalts` / `costInBounded`: the formal shadow of
  `HALTS`/`BOUNDED` — `∃ g, BigO cost g`. Every `costInClass` promotes
  via its own rep as witness (`costInClass_to_halts/bounded`). This is
  the derived quant→qual edge; the tag table's `True` records its
  class-level shadow.
- `IsDivergentCost` / `costInUnbound` / `costInGrowing`: `¬ ∃ g,
  BigO cost g`. Under total `Nat → Nat` costs this is EMPTY
  (`no_divergent_cost`, via `BigO.refl`): every total cost is finite, so
  true divergence needs a future partial-cost/trace model. Tag-level
  `UNBOUND ≤ UNDECIDABLE` stays stipulated; value-level it is vacuous
  until that model lands. `costInUndecidable`/`costInUnknown` are `True`
  (forgetting from either side).

Per-resource preservation (`seqCombine`/`branchCombine` stay inside summed
/ maxed envelopes) is proved per resource in its own file via
`BigO.add` / `BigO.max_bound` — see `Examples/Resources.lean`. Core stays
generic; no per-resource code here. -/
namespace LeanC

/-- WHAT `costInClass` is: a resource value `v : R` is in class `C`
(quantitative, on `axis`) iff its cost function is `BigO`-inside `C`'s
rep. WHY `BigO`, not equality: codegen may multiply steps by a constant;
`=O` absorbs it, so `O1 + O1 = O1` holds for values too. -/
def costInClass {axis : ResourceAxis} (R : Type 0) [HasCost R]
    (C : Type) [HasQuantRep axis C] (v : R) : Prop :=
  BigO (HasCost.cost v) (HasQuantRep.rep (axis := axis) (α := C))

/-- WHY `mono`: class membership respects quantitative ordering — if `v`
is in `C₁` and `C₁ ≤ C₂` (`QuantLE`), then `v` is in `C₂`. Proof is one
`BigO.trans`. NOTE: promotion to the qualitative ceiling (`HALTS` /
`BOUNDED`) does NOT go through here (`HALTS` has no rep) — use
`costInClass_to_halts` / `costInClass_to_bounded` below (existential
forgetting). -/
theorem costInClass_mono {axis : ResourceAxis} {R : Type 0} [HasCost R]
    {C₁ C₂ : Type} [HasQuantRep axis C₁] [HasQuantRep axis C₂]
    {v : R} :
    costInClass (axis := axis) R C₁ v →
    QuantLE (axis := axis) C₁ C₂ →
    costInClass (axis := axis) R C₂ v :=
  fun h12 hle => BigO.trans h12 hle

/-- WHAT `costInZero` is: the ONLY `Zero` membership — pointwise `0`
(`cost v = gZero`). WHY not `costInClass`: `Zero` has no `HasQuantRep`,
so `BigO (cost v) gZero` would also cover constant-`1` costs (via
`bigO_one_le_zero`) and collapse the bottom at value level. Equality to
`gZero` keeps `Zero` exact while `O1` stays `≤ K`. -/
def costInZero (R : Type 0) [HasCost R] (v : R) : Prop :=
  HasCost.cost v = gZero

/-- `Zero` promotes to `O1` on either axis (via `bigO_zero_le_one` after
unfolding the equality). The reverse does NOT hold — see
`not_costInZero_const` in `Examples/Resources` / tests. -/
theorem costInZero_to_o1_time {R : Type 0} [HasCost R] {v : R} :
    costInZero R v →
    costInClass (axis := .time) R TimeComplexity_O1 v := by
  intro h
  show BigO (HasCost.cost v) g1
  rw [h]
  exact bigO_zero_le_one

theorem costInZero_to_o1_mem {R : Type 0} [HasCost R] {v : R} :
    costInZero R v →
    costInClass (axis := .memory) R MemoryComplexity_O1 v := by
  intro h
  show BigO (HasCost.cost v) g1
  rw [h]
  exact bigO_zero_le_one

/-- WHAT `IsFiniteCost` is: the formal meaning of `HALTS`/`BOUNDED` for a
cost function — *some* finite envelope exists. This is the `∃ g` half of
proposal §3.4 (minus `Computable`, which awaits a computability import:
without it every total cost is trivially finite via its own rep, see
`every_cost_finite`). -/
def IsFiniteCost (cost : Nat → Nat) : Prop :=
  ∃ g, BigO cost g

/-- WHAT `IsDivergentCost` is: `¬ ∃ g, BigO cost g` — no finite envelope.
Under total `Nat → Nat` costs this is uninhabited (`no_divergent_cost`);
it records the intended meaning of `UNBOUND`/`GROWING` until a
partial-cost (infinite-trace) model provides inhabitants. -/
def IsDivergentCost (cost : Nat → Nat) : Prop :=
  ¬ ∃ g, BigO cost g

/-- Every total cost is finite (witness: itself, via `BigO.refl`). Hence
`IsDivergentCost` is currently empty — the honest reason `UNBOUND`
sits outside `BigO` and its tag edges stay stipulated. -/
theorem every_cost_finite (c : Nat → Nat) : IsFiniteCost c :=
  ⟨c, BigO.refl c⟩

theorem no_divergent_cost (c : Nat → Nat) : ¬ IsDivergentCost c :=
  fun h => h ⟨c, BigO.refl c⟩

theorem finite_not_divergent {c : Nat → Nat} :
    IsFiniteCost c → ¬ IsDivergentCost c := by
  intro ⟨g, hg⟩ hd
  exact hd ⟨g, hg⟩

/-- Qualitative membership, value level: `HALTS`/`BOUNDED` = finite;
`UNBOUND`/`GROWING` = divergent; `UNDECIDABLE`/`UNKNOWN` = no claim
(`True`, so both sides forget into it). -/
def costInHalts (R : Type 0) [HasCost R] (v : R) : Prop :=
  IsFiniteCost (HasCost.cost v)
def costInBounded (R : Type 0) [HasCost R] (v : R) : Prop :=
  IsFiniteCost (HasCost.cost v)
def costInUnbound (R : Type 0) [HasCost R] (v : R) : Prop :=
  IsDivergentCost (HasCost.cost v)
def costInGrowing (R : Type 0) [HasCost R] (v : R) : Prop :=
  IsDivergentCost (HasCost.cost v)
def costInUndecidable (R : Type 0) [HasCost R] (_ : R) : Prop :=
  True
def costInUnknown (R : Type 0) [HasCost R] (_ : R) : Prop :=
  True

/-- Quant→qual forgetting, value level: exhibiting any quantitative
envelope places the value below `HALTS`/`BOUNDED`. This derives the
tag-table `True` rows `o1/log/poly → halts/bounded`. -/
theorem costInClass_to_halts {R : Type 0} [HasCost R]
    {C : Type} [HasQuantRep .time C] {v : R} :
    costInClass (axis := .time) R C v → costInHalts R v :=
  fun h => ⟨_, h⟩

theorem costInClass_to_bounded {R : Type 0} [HasCost R]
    {C : Type} [HasQuantRep .memory C] {v : R} :
    costInClass (axis := .memory) R C v → costInBounded R v :=
  fun h => ⟨_, h⟩

/-- Forgetting into the top: finite and divergent both refine unknown. -/
theorem halts_to_undecidable {R : Type 0} [HasCost R] {v : R} :
    costInHalts R v → costInUndecidable R v :=
  fun _ => trivial

theorem unbound_to_undecidable {R : Type 0} [HasCost R] {v : R} :
    costInUnbound R v → costInUndecidable R v :=
  fun _ => trivial

theorem bounded_to_unknown {R : Type 0} [HasCost R] {v : R} :
    costInBounded R v → costInUnknown R v :=
  fun _ => trivial

theorem growing_to_unknown {R : Type 0} [HasCost R] {v : R} :
    costInGrowing R v → costInUnknown R v :=
  fun _ => trivial

end LeanC
