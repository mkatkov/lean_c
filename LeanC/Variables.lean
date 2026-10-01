import LeanC.Context
import LeanC.TypeClasses

/-!
# Variables — de Bruijn references (T2 + P2 E3a enforcing)

`CVarRef Γ α` is a typed de Bruijn index into `Γ`'s scope. P2 E3a:
construction requires a scope-membership proof (`VarScope.scopeContains`),
so dangling `CVarRef.mk 999` without proof does not elaborate. `Scopes.CVarScope`
is untouched (full de Bruijn shifting out of scope).

`VarScope` shape (agent's choice, one place, recorded in friction F15):
`class VarScope (Γ) [CContext Γ] with scopeContains : Nat → (α : Type) →
[IsCType α] → Prop`. The `mk` ctor takes `[VarScope Γ]` + the proof as
ctor-only args, so the *type* `CVarRef Γ α` needs no new constraint (only
construction does) — `CExpr.var`/`addr` and `emitAssign/Decl` signatures
stay unchanged.

`DraftCtx` instance (minimal, suffices for P2): `scopeContains idx _ :=
idx < 2` (pilot scope holds `x0`, `x1` only; `999` is unprovable, `0`/`1`
by `decide`). Full `scope[idx]?` lookup + type equality is future work.

Load costs (if any) are assigned per resource type via
`CContext.setResource` + `CResource` instances in the caller's file.
-/

namespace LeanC
universe v

/-- P2 E3a scope-membership class (one place). -/
class VarScope (Γ : Type v) [CContext Γ] where
  scopeContains : Nat → (α : Type) → [IsCType α] → Prop

/-- Minimal `DraftCtx` instance: pilot scope `x0`, `x1` only. -/
instance : VarScope DraftCtx where
  scopeContains idx _ := idx < 2

/-- Decidable membership for `DraftCtx` (so `by decide` proves `0 < 2`,
`1 < 2`; `999` is unprovable). Unfolds the instance above. -/
instance (idx : Nat) (α : Type) [IsCType α] :
    Decidable (VarScope.scopeContains (Γ := DraftCtx) idx α) :=
  inferInstanceAs (Decidable (idx < 2))

/-- Typed de Bruijn variable reference. `Γ` is the context type, `α` the
C type (in `Type 0` — see `Literals.lean` universe note).
P2 E3a: `mk` requires `[VarScope Γ]` + membership proof (ctor-only, so
the type mentions need no extra constraint). -/
inductive CVarRef (Γ : Type v) [CContext Γ] (α : Type) [IsCType α] where
| mk : (idx : Nat) → [VarScope Γ] → VarScope.scopeContains (Γ := Γ) idx α → CVarRef Γ α

/-- Project the raw index (producer uses this to resolve names; drops proof). -/
def CVarRef.toNat {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CVarRef Γ α → Nat
| @CVarRef.mk _ _ _ _ idx _ _ => idx

-- P2 E3a: proof-free index alias deleted (use `.mk` with proof).
-- Attempting `CVarRef.mk 999` without proof does not elaborate (see friction F15).

end LeanC
