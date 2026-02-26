import LeanC.Context

/-!
# Abstract process tree for lean_c

This module defines core process related types:

we can have sequencial process, branching process, loop process, and demonic loop process.
The only thing we can require from the process is that it is sound.

TODO: These classes and types should be linked to CComplexity

-/

namespace LeanC
universe u v

/-- C program consists of statements within their context

The only thing in general we can require form the stement is that
it is terminating in some sence.
-/
class CStatement (Γ : Type v) [CContext Γ] (α : Type u) where
  isStatementSound : Prop

/-- Sequential process is a set list of statements that are executed sequentially in time

We should require that all statements in the list are CStatements and all of them are sound
-/
inductive SequentialProcess : (List (Type u)) -> Type (u+1) where
| nil : SequentialProcess []
| cons {Γ : Type u} [CContext Γ] {τs : List (Type u)} (τ : Type u ) [CStatement Γ τ] (_: SequentialProcess τs) : SequentialProcess (τ :: τs)


-- We provide separate instances for the empty list and the cons case.
instance {Γ : Type u} [CContext Γ] : CStatement Γ (SequentialProcess []) where
  isStatementSound := True

instance {Γ : Type v} [CContext Γ] {τ : Type u} [CStatement Γ τ] {τs : List (Type u)} [CStatement Γ (SequentialProcess τs)] :
    CStatement Γ (SequentialProcess (τ :: τs)) where
  isStatementSound :=
    (CStatement.isStatementSound Γ (α := τ)) ∧
      (CStatement.isStatementSound Γ (α := SequentialProcess τs))

/-- We only consider bnary branching
-/
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
