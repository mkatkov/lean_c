import LeanC.Context
import LeanC.Complexity
import LeanC.TypeClasses

/-!
# Variables — de Bruijn references with load cost (T2)

`CVarRef Γ α` is a typed de Bruijn index into `Γ`'s scope. Name→`idx`
uniqueness is producer duty (cf. `doc/ideas.md`): this file does not enforce
scope membership — `Scopes.CVarScope` is untouched — it only carries the index
plus its type. Load cost is `(O1, Zero)` (one abstract step, no allocation).
-/

namespace LeanC
universe v

/-- Typed de Bruijn variable reference. `Γ` is the context type, `α` the
C type (in `Type 0` — see `Literals.lean` universe note). -/
inductive CVarRef (Γ : Type v) [CContext Γ] (α : Type) [IsCType α] where
| mk : (idx : Nat) → CVarRef Γ α

/-- WHAT `varBound` costs: one load step `(O1)`, no allocation `(Zero)`. -/
def varBound : ResourceBound :=
  { timeRep := g1, memRep := gZero }

/-- Load is `O(1)` by reflexivity. -/
theorem var_load_O1 : BigO varBound.timeRep g1 :=
  BigO.refl _

/-- Project the raw index (producer uses this to resolve names). -/
def CVarRef.toNat {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CVarRef Γ α → Nat
| .mk idx => idx

/-- Bridge stub toward `CVarIndex` (one-line constructor alias).
TODO: link `idx` to the actual integer variable value — `Arrays.lean`
currently pins `CIndex.value (CVarIndex …) := 0`, so variable-index
`within_bounds` proofs stay weak; use `CConstIndex` in examples. -/
def ofUInt {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] (idx : Nat) :
    CVarRef Γ α :=
  .mk idx

end LeanC
