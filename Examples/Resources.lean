import LeanC.TypeClasses
import LeanC.Context
import LeanC.Complexity.BigO
import LeanC.Complexity.Growth
import LeanC.Complexity.Quant
import LeanC.Complexity.Bridge

/-!
# Example resources — NOT core library

These live under `Examples/`, not `LeanC/`: `Context` defines only the
generic mechanism (`CResourceType` marker, `CResource` ops, `HasCost`
extraction, type-keyed `RStore` + `CContext` get/set). Each resource below
is a new type + instances in this file — zero edits to `LeanC/Context.lean`.

Combining uses instances: `seqCombine` for sequencing,
`branchCombine` for branching/loop joins. Membership in a complexity
class is `costInClass` (`Complexity/Bridge.lean`): `BigO (cost v) rep`.
Preservation lemmas below (`time_seq_preserves`, …) are the per-resource
composition rules — each is one `BigO.add` / `BigO.max_bound` application,
so sequencing time adds and memory maxes WITHOUT a hardwired
`ResourceBound` pair in core.
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

instance : HasCost TimeCost where
  cost := (·.val)

/-- Example: memory as high-water mark. -/
structure MemCost where
  val : Nat → Nat

instance : CResourceType MemCost where
  isResourceType := True

instance : CResource MemCost where
  zero := ⟨fun _ => 0⟩
  seqCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n)⟩
  branchCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n)⟩

instance : HasCost MemCost where
  cost := (·.val)

/-- Example: energy (new resource without touching core). -/
structure EnergyCost where
  val : Nat → Nat

instance : CResourceType EnergyCost where
  isResourceType := True

instance : CResource EnergyCost where
  zero := ⟨fun _ => 0⟩
  seqCombine a b := ⟨fun n => a.val n + b.val n⟩
  branchCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n)⟩

instance : HasCost EnergyCost where
  cost := (·.val)

/-- Example: embedded exact count. -/
structure ExactCount where
  val : Nat

instance : CResourceType ExactCount where
  isResourceType := True

instance : CResource ExactCount where
  zero := ⟨0⟩
  seqCombine a b := ⟨a.val + b.val⟩
  branchCombine a b := ⟨Nat.max a.val b.val⟩

instance : HasCost ExactCount where
  cost := fun e _ => e.val

/-! ## Bridge: `seqCombine`/`branchCombine` preserve `BigO` envelopes.

Each lemma below IS the composition rule for its resource: sequencing
time adds (`BigO.add`), branching / memory maxes (`BigO.max_bound`),
the time-branch guard `+1` chains `max_bound` then `add` with `refl 1`
(same shape as `branch_bound_max_time` in the old paired design, now
per-resource so core stays generic). -/

/-- Time sequencing adds envelopes. -/
theorem time_seq_preserves {a b : TimeCost} {ga gb : Nat → Nat}
    (ha : BigO a.val ga) (hb : BigO b.val gb) :
    BigO (CResource.seqCombine a b).val (fun n => ga n + gb n) :=
  BigO.add ha hb

/-- Time branching maxes envelopes, guard `+1` sequenced after. -/
theorem time_branch_preserves {a b : TimeCost} {ga gb : Nat → Nat}
    (ha : BigO a.val ga) (hb : BigO b.val gb) :
    BigO (CResource.branchCombine a b).val
      (fun n => Nat.max (ga n) (gb n) + 1) := by
  have hmax : BigO (fun n => Nat.max (a.val n) (b.val n))
      (fun n => Nat.max (ga n) (gb n)) :=
    BigO.max_bound ha hb
  have hone : BigO (fun _ : Nat => 1) (fun _ : Nat => 1) := BigO.refl _
  have hadd := BigO.add hmax hone
  simpa [CResource.seqCombine, CResource.branchCombine] using hadd

/-- Memory sequencing takes the high-water mark (max). -/
theorem mem_seq_preserves {a b : MemCost} {ga gb : Nat → Nat}
    (ha : BigO a.val ga) (hb : BigO b.val gb) :
    BigO (CResource.seqCombine a b).val
      (fun n => Nat.max (ga n) (gb n)) :=
  BigO.max_bound ha hb

/-- Memory branching takes the max (guard allocates nothing). -/
theorem mem_branch_preserves {a b : MemCost} {ga gb : Nat → Nat}
    (ha : BigO a.val ga) (hb : BigO b.val gb) :
    BigO (CResource.branchCombine a b).val
      (fun n => Nat.max (ga n) (gb n)) :=
  BigO.max_bound ha hb

/-- `zero` costs nothing pointwise. -/
theorem time_zero_val : (CResource.zero (R := TimeCost)).val = gZero := rfl
theorem mem_zero_val : (CResource.zero (R := MemCost)).val = gZero := rfl

/-- Constant time values are `O1` (`≤ K` folds via `const_le_one`). -/
theorem time_const_in_o1 (K : Nat) :
    costInClass (axis := .time) TimeCost TimeComplexity_O1 ⟨fun _ => K⟩ :=
  BigO.const_le_one _ (fun _ => Nat.le_refl _)

/-- Constant memory values are `O1`. -/
theorem mem_const_in_o1 (K : Nat) :
    costInClass (axis := .memory) MemCost MemoryComplexity_O1 ⟨fun _ => K⟩ :=
  BigO.const_le_one _ (fun _ => Nat.le_refl _)

/-- `O1 + O1 = O1` for time sequencing (const folding): two constant
fragments sequence inside `O1`. -/
theorem time_seq_o1 (K₁ K₂ : Nat) :
    costInClass (axis := .time) TimeCost TimeComplexity_O1
      (CResource.seqCombine ⟨fun _ => K₁⟩ ⟨fun _ => K₂⟩) := by
  show BigO _ g1
  have h : BigO (fun n => K₁ + K₂) g1 :=
    BigO.const_le_one _ (fun _ => Nat.le_refl _)
  simpa using h

/-- Branching two `O1` times stays `O1` (`max 1 1 + 1 = 2 =O 1`). -/
theorem time_branch_o1 :
    costInClass (axis := .time) TimeCost TimeComplexity_O1
      (CResource.branchCombine ⟨fun _ => 1⟩ ⟨fun _ => 1⟩) := by
  show BigO _ g1
  have h : BigO (fun _ : Nat => Nat.max 1 1 + 1) g1 :=
    BigO.const_le_one _ (K := 2) (fun _ => by decide)
  simpa using h

end LeanC
