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

- `costInClass axis v C`: `BigO (cost v) (rep C)` — the membership claim.
- `costInClass_mono`: membership transports along `QuantLE` (`BigO.trans`).
- `cost_zero_in_zero`: `HasCost.cost (zero)` is pointwise `0` implies
  `BigO` into `gZero` — for resources whose `zero` is the `0` function
  (proved per-resource; `TimeCost`/`MemCost` do this in `Examples/Resources`).

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
is in `C₁` and `C₁ ≤ C₂` (`QuantLE`), then `v` is in `C₂`. This is what
lets a `linear ≤ poly₂` proof promote any `linear`-bounded value below
`HALTS` by forgetting (via base `poly₂ ≤ HALTS`). Proof is one
`BigO.trans`. -/
theorem costInClass_mono {axis : ResourceAxis} {R : Type 0} [HasCost R]
    {C₁ C₂ : Type} [HasQuantRep axis C₁] [HasQuantRep axis C₂]
    {v : R} :
    costInClass (axis := axis) R C₁ v →
    QuantLE (axis := axis) C₁ C₂ →
    costInClass (axis := axis) R C₂ v :=
  fun h12 hle => BigO.trans h12 hle

end LeanC
