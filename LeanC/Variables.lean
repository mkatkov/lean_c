import LeanC.Context
import LeanC.TypeClasses

/-!
# Variables — de Bruijn references (T2)

`CVarRef Γ α` is a typed de Bruijn index into `Γ`'s scope. Name→`idx`
uniqueness is producer duty (cf. `doc/ideas.md`): this file does not enforce
scope membership — `Scopes.CVarScope` is untouched — it only carries the index
plus its type.

Load costs (if any) are assigned per resource type via
`CContext.setResource` + `CResource` instances in the caller's file.
-/

namespace LeanC
universe v

/-- Typed de Bruijn variable reference. `Γ` is the context type, `α` the
C type (in `Type 0` — see `Literals.lean` universe note). -/
inductive CVarRef (Γ : Type v) [CContext Γ] (α : Type) [IsCType α] where
| mk : (idx : Nat) → CVarRef Γ α

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
