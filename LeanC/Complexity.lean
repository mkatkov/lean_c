

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

/-- we need to have machinery to keep state of art knowledge of complexity classes
when they added and removed.

Above the only stub for newly developed classes we actually need to insert them into knowledge base.
So the judgement about program requirement would be available at building time.

CComplexityShadowNode is a gluing layer for complexity classes.
The idea it to have current structure with intermediate nodes making correct chain structure
for example, if we already know C1 < c2 < C3 (C. is a collection of classes, c. is specific complexity class)
and we insert c4 with c2 < c4 < C3 the final structure algorithmically shoulld be C1 < c2 < c3 < C4 .
But if we already have fixed typies for c2 this cannot be implemented. So we need a shadow type that will hols c2 and
update its path. So the structure will be Shadow1( C1)  < Shadow2( c2 ) < Shadow3( c4 )  < Shadow(C3)

in this way everytime an intermediate class is intriduced we need to have an extra shadoow class

Alternative way is to rebuild knowledge base from scratch when new class is added covering them with shaddow layers
that represent chains correctly. I guess we can create a list of types and derive partial order on fly.
-/

def can_insert_complexity_class_to_grapth {τs : List (Type u)} (τ : Type u )
  [CComplexity τ] ( cg: CComplexityGraph τs ) : Prop := False

inductive CComplexityGraph : (List (Type u)) -> Type (u+1) where
  | nil : CComplexityGraph []
  | insert {τs : List (Type u)}
     (τ : Type u ) [CComplexity τ]
     (_: CComplexityGraph τs ): CComplexityGraph (τ :: τs)


instance {α : List (Type u)} : LE (CComplexityGraph α) where
  le α β  := False

end LeanC
