import LeanC.TypeClasses
import LeanC.Scopes

/-!
# C Type System for lean_c

This module defines the core C type system including:
- Basic types (integers, floats, void)
- Composite types (pointers, structs, unions, functions)
- Type size and alignment calculations
- Type resolution and code generation framework

It provides a foundation for representing and manipulating C types within the LeanC framework.
The implementation has the following structure:
for each actual C Type there is a corresponding Lean inductive type definition.
filtering arguments is performed using type classes withthe following convention:
class IsC<Question> (α : Type u) where
  isC<Question> : Prop

This has two main advantages:
1. It allows us to define generic functions that can operate on any C type by constraining
   the type parameters using these type classes.
2. It provides a clear and extensible way to categorize and manage different C types
3. having Prop for the result allows to prove correctness if one need to do so.


types:
void
char types
integer types
floating point types
pointer types
array types
struct types
union types
function types
typedef types


- there are some minor issues with type qualifications that need to be resolved. in particular in functions.
-/

namespace LeanC
universe u

class CProgramContext where
  is_context := True

inductive CIntSize where
| I8
| I16
| I32
| I64

instance : CTypeSize CIntSize where
  size_of
    | CIntSize.I8 => 1
    | CIntSize.I16 => 2
    | CIntSize.I32 => 4
    | CIntSize.I64 => 8

inductive CFloatSize where
  | F16
  | F32
  | F64

instance : CTypeSize CFloatSize where
  size_of
    | CFloatSize.F16 => 2
    | CFloatSize.F32 => 4
    | CFloatSize.F64 => 8


inductive CTypeQualificationConst (α : Type u) [IsCType α] where
  | mk
instance  (α : Type u) [IsCType α] :  IsCType (CTypeQualificationConst α) where
  isCType := True

inductive CTypeQualificationVolatile (α : Type u) [IsCType α] where
  | mk
instance  (α : Type u) [IsCType α] :  IsCType (CTypeQualificationVolatile α) where
  isCType := True

inductive CTypeQualificationRestrict (α : Type u) [IsCType α] where
  | mk
instance  (α : Type u) [IsCType α] :  IsCType (CTypeQualificationRestrict α) where
  isCType := True

/-- Void type
it is not a CType, only ref to void is a c type that we will deal in pointer section.

-/
inductive CVoidType  where
  | mk : CVoidType
/-- in addition to any c type we can have a pointer to void. -/
instance : IsPointedCType CVoidType where
  isPointedCType := True

/-- Void type (α - type qualification) -/
inductive CCharType (α : Type) where
  | mk : CCharType α
instance {α :Type} : CTypeSize (CCharType α) where
  size_of _ := 1

/-- CTypedef represents a C typedef definition. -/
inductive CTypedef (α:Type u) [IsCType α]   where
  | mk : CTypedef α


inductive CIntType (sz : CIntSize) (sgn : Bool)where
| mk : CIntType sz sgn
instance (sz : CIntSize) (sgn : Bool) :  IsCType (CIntType sz sgn) where
  isCType := True
instance (sz : CIntSize) (sgn : Bool) : CTypeSize (CIntType sz sgn) where
  size_of (_x: CIntType sz sgn ):= CTypeSize.size_of sz

example := CIntType CIntSize.I8 False

abbrev CUInt8Type := CIntType CIntSize.I8 False
abbrev CUInt16Type := CIntType CIntSize.I16 False
abbrev CUInt32Type := CIntType CIntSize.I32 False
abbrev CUInt64Type := CIntType CIntSize.I64 False
abbrev CInt8Type := CIntType CIntSize.I8 True
abbrev CInt16Type := CIntType CIntSize.I16 True
abbrev CInt32Type := CIntType CIntSize.I32 True
abbrev CInt64Type := CIntType CIntSize.I64 True

inductive CFloatType (sz : CFloatSize) where
| mk : CFloatType sz
instance (sz : CFloatSize) : IsCType (CFloatType sz) where
  isCType := True

/-- CPointer_t represents a C pointer type.
There is no restriction to what pointer type can point to.
-/
inductive CPointerType (α : Type u) [IsPointedCType α] where
  | mk : CPointerType α
instance (α : Type u) [IsPointedCType α] : IsCType (CPointerType α) where
  isCType := True

inductive CStructType : (List (Type u)) -> Type (u+1) where
  | nil : CStructType []
  | cons {τs : List (Type u)}
     (τ : Type u ) [IsCType τ]
     (_: CStructType τs ): CStructType (τ :: τs)
  | bitfield {τs : List (Type u)} {α :Type u} [CTypeSize α] [IsIntegerType α ] (size : Nat) (storage_type : α ) ( can_store : size <= 8*(CTypeSize.size_of storage_type))   : CStructType ( α :: τs)

instance {α : List Type}: IsCType (CStructType α) where
  isCType := True

example :CStructType [CUInt32Type] := CStructType.cons CUInt32Type CStructType.nil
/- next  example should fail -> we cannot syntesythe IsCType for void type -/
-- example :CStructType [CVoidType, CUInt32Type] := CStructType.cons CVoidType (CStructType.cons CUInt32Type CStructType.nil )

/-- CUnion is similat to CStruct Type-/
inductive CUnionType (α: List Type) where
  | mk : CVarScope α → CUnionType α
instance {α: List Type} : IsCType (CUnionType α) where
  isCType := True

/-- CFunction represents a C function type.

arguments → (does it have variable arguments? : Bool) → return type
TODO: one have to take care of void ctype would not be a part of argument types
-/

inductive CFunctionType (arg_types : CVarScope γ) (elipses : Bool) (ret_type : Type u) [IsCFuncReturnType ret_type] where
  | mk : CFunctionType arg_types elipses ret_type
instance (arg_types : CVarScope γ) (elipses : Bool) (ret_type : Type u) [IsCFuncReturnType ret_type] :
    IsPointedCType (CFunctionType arg_types elipses ret_type) where
  isPointedCType := True

example := CVarScope.nil
example := CVarScope []
example : CVarScope [CFloatType CFloatSize.F64] := CVarScope.cons (CFloatType .F64) CVarScope.nil
example : CVarScope [CTypeQualificationVolatile CInt32Type, CFloatType CFloatSize.F64] := CVarScope.cons
  (CTypeQualificationVolatile CInt32Type) (CVarScope.cons
    (CFloatType .F64) CVarScope.nil )
example : CFunctionType
  (CVarScope.cons (CTypeQualificationVolatile CInt32Type) (CVarScope.cons
    (CFloatType .F64) CVarScope.nil  ) ) false (CVoidType ) :=
  CFunctionType.mk

example :=
  let args := CVarScope.cons CInt32Type
              (CVarScope.cons (CFloatType .F64)
              CVarScope.nil )
  let ret := CVoidType
  (CFunctionType.mk : CFunctionType args false ret )
example := CVarScope [CInt32Type, CFloatType .F64]

end LeanC
