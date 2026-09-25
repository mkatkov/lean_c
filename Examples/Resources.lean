import LeanC.TypeClasses
import LeanC.Context

/-!
# Example resources — NOT core library

These live under `Examples/`, not `LeanC/`: `Context` defines only the
generic mechanism (`CResourceType` marker, `CResource` ops,
type-keyed `RStore` + `CContext` get/set). Each resource below is a new
type + instances in this file — zero edits to `LeanC/Context.lean`.

Combining uses instances: `seqCombine` for sequencing,
`branchCombine` for branching/loop joins.
-/

namespace LeanC

/-- Example: time as asymptotic cost function. -/
structure TimeCost where
  val : Nat → Nat

instance : CResourceType TimeCost where
  isResourceType := True

instance : CResource TimeCost where
  zero := ⟨fun _ => 0⟩
  seqCombine a b := ⟨fun n => a.val n + b.val n⟩
  branchCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n) + 1⟩

/-- Example: memory as high-water mark. -/
structure MemCost where
  val : Nat → Nat

instance : CResourceType MemCost where
  isResourceType := True

instance : CResource MemCost where
  zero := ⟨fun _ => 0⟩
  seqCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n)⟩
  branchCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n)⟩

/-- Example: energy (new resource without touching core). -/
structure EnergyCost where
  val : Nat → Nat

instance : CResourceType EnergyCost where
  isResourceType := True

instance : CResource EnergyCost where
  zero := ⟨fun _ => 0⟩
  seqCombine a b := ⟨fun n => a.val n + b.val n⟩
  branchCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n)⟩

/-- Example: embedded exact count. -/
structure ExactCount where
  val : Nat

instance : CResourceType ExactCount where
  isResourceType := True

instance : CResource ExactCount where
  zero := ⟨0⟩
  seqCombine a b := ⟨a.val + b.val⟩
  branchCombine a b := ⟨Nat.max a.val b.val⟩

end LeanC
