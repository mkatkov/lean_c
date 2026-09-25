import LeanC.Context

/-!
# Abstract process tree for lean_c

This module defines core process related types:

we can have sequencial process, branching process, loop process, and demonic loop process.
Every statement carries a soundness obligation (`isStatementSound`).
Resource costs (if any) are assigned per resource type in the context
via `CContext.setResource`/`getResource` + `CResource.seqCombine`/
`branchCombine` instances — no hardwired bound pair here.
-/

namespace LeanC
universe u v

/-- WHAT a statement is: soundness only. WHY no bound field: bounds are
per-resource values in the context (`CResource` instances combine them);
forcing every statement type to carry a hardwired pair would reintroduce
the closed axes. New resources add instances, never edits here. -/
class CStatement (Γ : Type v) [CContext Γ] (α : Type u) where
  isStatementSound : Prop

/-- WHAT a sequential process is: the statement list executed in order
(`nil` = empty program, `cons` = head statement then the rest). WHY the
`CStatement` constraints on `cons`: each element (and, inductively, the
tail) must already be sound — so `cons` below can conjoin soundness.
Result lives in `Type (u+2)` (one higher than the `u+1` for plain lists)
to accommodate `CContext Γ`'s type-keyed methods (`ResMem`/`RStore` in
`Type 1`). -/
inductive SequentialProcess : (List (Type u)) -> Type (u+2) where
| nil : SequentialProcess []
| cons {Γ : Type u} [CContext Γ] {τs : List (Type u)} (τ : Type u ) [CStatement Γ τ] (_: SequentialProcess τs) : SequentialProcess (τ :: τs)


-- WHAT the two `SequentialProcess` instances say: `nil` is sound;
-- `cons` conjoins head + tail soundness. No bounds folded here.
instance {Γ : Type u} [CContext Γ] : CStatement Γ (SequentialProcess []) where
  isStatementSound := True

instance {Γ : Type v} [CContext Γ] {τ : Type u} [CStatement Γ τ] {τs : List (Type u)} [CStatement Γ (SequentialProcess τs)] :
    CStatement Γ (SequentialProcess (τ :: τs)) where
  isStatementSound :=
    (CStatement.isStatementSound Γ (α := τ)) ∧
      (CStatement.isStatementSound Γ (α := SequentialProcess τs))

/-- WHAT binary branching is: `if α then TrueBranch else FalseBranch`
(`α` the guard proposition). WHY only binary: `if/else` covers every
finite branch (`switch` desugars to nested binary). -/
inductive BinaryBranchingProcess (Γ : Type v) [CContext Γ] (α : Prop)
  ( TrueBranch : Type u) [CStatement Γ TrueBranch]
  (FalseBranch : Type u) [CStatement Γ FalseBranch] where
| mk : BinaryBranchingProcess Γ α TrueBranch FalseBranch

instance {α : Prop} {Γ : Type v} [CContext Γ]
  { TrueBranch : Type u } [CStatement Γ TrueBranch]
  {FalseBranch : Type u} [CStatement Γ FalseBranch] : CStatement Γ (BinaryBranchingProcess Γ α TrueBranch FalseBranch ) where
  isStatementSound :=
    (CStatement.isStatementSound Γ (α := TrueBranch)) ∧
      (CStatement.isStatementSound Γ (α := FalseBranch))

end LeanC
