import LeanC.Context
import LeanC.Complexity
import LeanC.Expr

/-!
# Verification — preservation skeleton (T5 stub)

Real preservation (`exprBound` soundness: evaluation preserves typing and
stays inside the bound) needs Clight semantics (future work). Draft
skeleton: the statement of what must be proved, with `True` placeholders
so the pipeline typechecks and the stdlib pilot can state its obligations.
-/

namespace LeanC
universe v

/-- WHAT preservation must say: well-typed exprs stay well-typed and
inside `exprBound` (time + memory). Draft placeholder (`True`). -/
def exprPreservationProp {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (_e : CExpr Γ α) : Prop :=
  True

/-- Skeleton: every expression preserves (placeholder proof). -/
theorem expr_preservation_skeleton {Γ : Type v} [CContext Γ]
    {α : Type} [IsCType α] (e : CExpr Γ α) :
    exprPreservationProp e :=
  trivial

end LeanC
