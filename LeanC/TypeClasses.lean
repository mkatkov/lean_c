

/-!
# Type classes in lean_c project.

The purpose of the block is to define the basic relationship
between classes irrespectively of their implementation that are useful
for different components of the project.

-/

namespace LeanC
universe u

/-- IsCType is a type class for distinguishing C types from other types. -/
class IsCType (α : Type u) where
  /-- Proof that the type α is a C type -/
  isCType : Prop

/-- We have void* which is valid c type, but void is not exactly valid c type
to distinguish between them we are definig the class what types can be pointed to.
all IsCTypes can be pointed to and additional CVoidType can be pointed to
-/
class IsPointedCType (α : Type u) where
  /-- Proof that the type α is a C type -/
  isPointedCType : Prop
instance {α:Type u} [IsCType α]: IsPointedCType α where
  isPointedCType := True

class IsCFuncReturnType (α : Type u) where
  isCFuncReturnType : Prop
instance {α:Type u} [IsPointedCType α]: IsCFuncReturnType α where
  isCFuncReturnType := True

class IsIntegerType (α : Type u) where
  /-- Proof that the type α is an integer C type -/
  isIntegerType : Prop
class CTypeSize (α : Type u) where
  size_of : α -> Nat


end LeanC
