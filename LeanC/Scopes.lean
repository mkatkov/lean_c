import LeanC.TypeClasses

/-!
# C Scope System for lean_c

This module defines the scopes.
Scopes define a binding between variables and their types.

In C we have different scopes that define different rules of possible
types.



-/
namespace LeanC
universe u



/-- CVarScope defines a scope of variables on block/module level.

CScope contains a list of variable types. Without loss of generality
this structure assumes unique binding between variable name and its
location in the scope. The specific naming and its uniquenebss is
a responsibility of code producer. Here we a re concerned with the
representation of variables.
 -/
inductive CVarScope : (List (Type u)) -> Type (u+1) where
  | nil : CVarScope []
  | cons {τs : List (Type u)} (τ : Type u ) [IsCType τ] (_: CVarScope τs) : CVarScope (τ :: τs)

/-- see more examples in `Types.lean` -/
example := CVarScope.nil


end LeanC
