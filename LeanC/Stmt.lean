import LeanC.Context
import LeanC.Processes
import LeanC.Variables
import LeanC.Expr
import LeanC.TypeClasses

/-!
# Statements — `assign/decl/return` stubs (T5)

NOTE (import direction): `Processes`/`Context` stay leaves (imported
upward, never importing `Expr`/`Func`). These stubs live here instead:
`Stmt → Expr + Processes`, `Func → Stmt`, no cycle. See friction F6.

Costs (if any) are assigned per resource type via `CContext.setResource`
+ `CResource` instances in the caller's file.
-/

namespace LeanC
universe v

/-- Marker: assignment statement. -/
inductive CAssign where | mk
/-- Marker: declaration statement. -/
inductive CDecl where | mk
/-- Marker: return statement. -/
inductive CReturn where | mk

instance {Γ : Type v} [CContext Γ] : CStatement Γ CAssign where
  isStatementSound := True

instance {Γ : Type v} [CContext Γ] : CStatement Γ CDecl where
  isStatementSound := True

instance {Γ : Type v} [CContext Γ] : CStatement Γ CReturn where
  isStatementSound := True

/-- C sketch: `x{idx} = <rhs>;`. -/
def emitAssign {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (lhs : CVarRef Γ α) (rhs : CExpr Γ α) : String :=
  s!"x{lhs.toNat} = {emitExpr rhs};"

/-- C sketch: `T x{idx};` or `T x{idx} = <init>;` (type erased to `auto`
in the draft — full type printing is future work). -/
def emitDecl {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (lhs : CVarRef Γ α) : Option (CExpr Γ α) → String
| none => s!"auto x{lhs.toNat};"
| some init => s!"auto x{lhs.toNat} = {emitExpr init};"

/-- C sketch: `return <e>;`. -/
def emitReturn {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) : String :=
  s!"return {emitExpr e};"

end LeanC
