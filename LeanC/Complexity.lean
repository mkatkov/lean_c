

namespace LeanC
universe u v

/-- defines the complexity clasess

-/
class CComplexity (Γ : Type v) where
  isCComplexity : Prop
/-- Since classes can be added we cannot have inductive type here

instead we need a relationship between classes.
instances should provide list of immediate larger classes, immediate smaller classes
and known equal classes.
-/
class CComplexityRelationship (α : Type u) [CComplexity α] where
  minimalStrictlySmaller : List Type
  minimalStrictlyLarger : List Type
  eqialTo : List Type

/-- this si by default -- we do not know the properties of the program apriory
-/
inductive CUndecidable where
| mk
instance : CComplexity CUndecidable where
  isCComplexity := True

/-- this is baic intermediate class -- we want all program to be belobg to this class-/
inductive CTerminates where
|mk
instance : CComplexity CTerminates where
  isCComplexity := True

/-- this is class of empty computation any program is equal or larger that this class-/
inductive CZeroComplexity where
| mk
instance : CComplexity CZeroComplexity where
 isCComplexity := True

 instance : CComplexityRelationship CZeroComplexity where
  minimalStrictlySmaller := [] -- nothing is below
  minimalStrictlyLarger := [CTerminates] -- this is the only class known
  eqialTo := []

instance : CComplexityRelationship CTerminates where
  minimalStrictlySmaller := [CZeroComplexity]
  minimalStrictlyLarger := [CUndecidable]
  eqialTo := []

instance : CComplexityRelationship CUndecidable where
  minimalStrictlySmaller := [CTerminates]
  minimalStrictlyLarger := []
  eqialTo := []

/- we need to have machinery to keep state of art knowledge of complexity classes
when they added and removed.

Above the only stub for newly developed classes we actually need to insert them into knowledge base.
So the judgement about program requirement would be available at building time.
-/

end LeanC
