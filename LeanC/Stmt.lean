import LeanC.Context
import LeanC.Complexity
import LeanC.Processes
import LeanC.Variables
import LeanC.Expr
import LeanC.TypeClasses

/-!
# Statements — `assign/decl/return` stubs via `exprBound` (T5)

NOTE (import direction): the plan's "add to `Processes.lean`" would make
`Processes → Expr` while `Expr → Processes` (for `seqBound`/`branchBound`
in `tern`/`call`) — a cycle. `Processes`/`Context` stay leaves (imported
upward, never importing `Expr`/`Func`). These stubs live here instead:
`Stmt → Expr + Processes`, `Func → Stmt`, no cycle. See friction F6.

`CStatement.stmtBound` is per-*type*, not per-*value* — so a fully
value-dependent `assign rhs` (bound = `seqBound (exprBound rhs) …`)
cannot be a `CStatement` instance directly (instance is for the type).
Draft split (friction F6b):
- `assignBound/declBound/returnBound`: value-level bound functions
  (`seqBound (exprBound …) …`) — used by the stdlib pilot's `worst_bound`.
- `CAssign/CDecl/CReturn`: marker types with `O(1)` `CStatement` instances
  (`isStatementSound := True`) — placeholders for the statement list
  (`SequentialProcess`) until value-dependent statements land.
- `emitAssign/emitDecl/emitReturn`: C sketches.
-/

namespace LeanC
universe v

/-- WHAT `assignBound` costs: evaluate `rhs`, then store (`varBound`
load/store slot) — `seqBound` (time adds, memory maxes). -/
def assignBound {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (_lhs : CVarRef Γ α) (rhs : CExpr Γ α) : ResourceBound :=
  seqBound (exprBound rhs) varBound

/-- WHAT `declBound` costs: evaluate initializer (if any), allocate slot.
`none` initializer costs `varBound` alone (slot only). -/
def declBound {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (_lhs : CVarRef Γ α) : Option (CExpr Γ α) → ResourceBound
| none => varBound
| some init => seqBound (exprBound init) varBound

/-- WHAT `returnBound` costs: evaluate returned expr, then one step. -/
def returnBound {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) : ResourceBound :=
  seqBound (exprBound e) { timeRep := g1, memRep := gZero }

/-- Marker: assignment statement (value-level bound is `assignBound`). -/
inductive CAssign where | mk
/-- Marker: declaration statement (value-level bound is `declBound`). -/
inductive CDecl where | mk
/-- Marker: return statement (value-level bound is `returnBound`). -/
inductive CReturn where | mk

instance {Γ : Type v} [CContext Γ] : CStatement Γ CAssign where
  isStatementSound := True
  stmtBound := { timeRep := g1, memRep := g1 }

instance {Γ : Type v} [CContext Γ] : CStatement Γ CDecl where
  isStatementSound := True
  stmtBound := { timeRep := g1, memRep := g1 }

instance {Γ : Type v} [CContext Γ] : CStatement Γ CReturn where
  isStatementSound := True
  stmtBound := { timeRep := g1, memRep := g1 }

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
