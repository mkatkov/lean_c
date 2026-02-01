import LeanC.Prod
-- import LeanC.Scope

/-!
# C Type System for lean_c

This module defines the core C type system including:
- Basic types (integers, floats, void)
- Composite types (pointers, arrays, structs, unions, functions)
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

/-- IsCType is a type class for distinguishing C types from other types. -/
class IsCType (α : Type u) where
  /-- Proof that the type α is a C type -/
  isCType : Prop
class IsVoidCType (α : Type u) where
  /-- Proof that the type α is a C type -/
  isVoidCType : Prop
instance {α:Type u}: IsVoidCType α where
  isVoidCType := False

class IsCFuncArgType (α : Type u) where
  /-- Proof that the type α is a C type -/
  isCFuncArgType : Prop
/-- All function types are c types -/
instance {α :Type u} [IsCFuncArgType α] : IsCType α where
  isCType := True
class IsIntegerType (α : Type u) where
  /-- Proof that the type α is an integer C type -/
  isIntegerType : Prop
class CTypeSize (α : Type u) where
  size_of : α -> Nat

/--
IdentifierProducer is a type class for producing unique identifiers for types, labels or identifiers.
α is producer flavor type
β is scope type - by abuse of notation it is also identifier


----- IT should actually be not here -------
-/

class IdentifierProducer (α : Type u)  (β :Type v) where
  produce_identifier : (x:β) → IO.FS.Stream → EIO IO.Error Unit
  can_produce : (x:β) → Prop
  are_identifiers_unique : (x : β) → Prop

/-- CLabelScope represents the scope of labels in C.
labels appear inside function and this would be function scope
But also types have CLabelScope. For instance typedefs can define type labels.
The distinctive feature these scopes do not have types associated with them.
They are resolved with names only.

The only requirements we have is that we can produce unique identifiers for labels.
-/
def CLabelScope := Nat

/-- α is a list of c types -/
inductive CVarScope : (List (Type u)) -> Type (u+1) where
  | nil : CVarScope []
  | cons {τs : List (Type u)} (τ : Type u ) [IsCType τ] : CVarScope (τ :: τs)

example := CVarScope.nil
inductive CFuncArgVarScope : (List (Type u)) -> Type (u+1) where
  | nil : CFuncArgVarScope []
  | cons {τs : List (Type u)} (τ : Type u ) [IsCFuncArgType τ] (_ :CFuncArgVarScope τs)  : CFuncArgVarScope (τ :: τs)

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


class CTypeQualification (α : Type u) where
  isCTypeQualification : Prop
instance {α :Type u} [IsCFuncArgType α] : IsCFuncArgType (CTypeQualification α) where
  isCFuncArgType := True

inductive CTypeQualificationNone (α:Type u) [IsCType α] where
| mk : CTypeQualificationNone α
instance (α:Type u) [IsCType α] : CTypeQualification (CTypeQualificationNone α) where
  isCTypeQualification := True

inductive CTypeQualificationConst  (α : Type u) [IsCType α] where
  | mk

inductive CTypeQualificationVolatile where
  | mk
instance : CTypeQualification CTypeQualificationVolatile where
  isCTypeQualification := True

inductive CTypeQualificationRestrict where
  | mk
instance : CTypeQualification CTypeQualificationRestrict where
  isCTypeQualification := True


inductive CQualifiedType (α : Type u) [IsCFuncArgType α] (β  : Type) [CTypeQualification β] where
  | mk : CQualifiedType α β
instance (α : Type u) [IsCFuncArgType α] {β  :Type} [CTypeQualification β]  : IsCType (CQualifiedType α β) where
  isCType := True

instance {α :Type u} [IsCFuncArgType α] (β  : Type) [CTypeQualification β] : IsCFuncArgType (CQualifiedType α β) where
  isCFuncArgType := True

/-- Void type -/
inductive CVoidType  where
  | mk : CVoidType
instance : IsCType CVoidType where
  isCType := True
instance : IsVoidCType CVoidType where
  isVoidCType := True

/-- Void type (α - type qualification) -/
inductive CCharType (α : Type) [CTypeQualification α]  where
  | mk : CCharType α
instance {α :Type} [CTypeQualification α] : IsCType (CCharType α) where
  isCType := True
instance {α :Type} [CTypeQualification α] : IsCFuncArgType (CCharType α) where
  isCFuncArgType := True
instance {α :Type} [CTypeQualification α] : CTypeSize (CCharType α) where
  size_of _ := 1

/-- CTypedef represents a C typedef definition. -/
inductive CTypedef (α:Type u) [IsCType α]   where
  | mk : CTypedef α
instance (α : Type u) [IsCType α] : IsCFuncArgType (CTypedef α) where
  isCFuncArgType := True


inductive CIntType (sz : CIntSize) (sgn : Bool)where
| mk : CIntType sz sgn
instance (sz : CIntSize) (sgn : Bool) : IsCFuncArgType (CIntType sz sgn) where
  isCFuncArgType := True
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
instance (sz : CFloatSize) : IsCFuncArgType (CFloatType sz) where
  isCFuncArgType := True

/-- CPointer_t represents a C pointer type.
There is no restriction to what pointer type can point to.
-/
inductive CPointer_t (α : Type u) [IsCType α] where
  | mk : CPointer_t α
instance (α : Type u) [IsCType α] : IsCType (CPointer_t α) where
  isCType := True

instance (α : Type u) [IsCType α] : IsCFuncArgType (CPointer_t α) where
  isCFuncArgType := True


/-- CArray_t represents a C array type.

CArray has element type and size. It is usually stack allocated, when declared.
However we can define a pointer to an array type as well. This can be in heap, but we indicate that
each array has fixed size.
-/
inductive CArray_t (α : Type u) [IsCType α] where
  | mk : Nat → CArray_t α
instance (α : Type u) [IsCType α] : IsCType (CArray_t α) where
  isCType := True
instance (α : Type u) [IsCType α] : IsCFuncArgType (CArray_t α) where
  isCFuncArgType := True


/- - we need to build scope together with the type
since for the struct to be usefull we need to be able to acces it
By the contract the first item in the scope is a type that can produce all identifiers in this scope.

The first list is a list of identifier producers for argument types having identifiers in it, like CStruct_t
The rest is just CTypes.
-/

-- def CTrivialLabelScopeProducer := Unit
-- instance : IdentifierProducer CTrivialLabelScopeProducer Nat where
--   produce_identifier val stream := stream.putStr s!"id_{val}"
--   can_produce _ := True
--   are_identifiers_unique _ := True

inductive CStructType : (List (Type u)) -> Type (u+1) where
  | nil : CStructType []
  | cons {τs : List (Type u)}
     (τ : Type u ) [IsCType τ] [IsVoidCType τ]
     (_: CStructType τs )
     (not_void : ¬ (IsVoidCType.isVoidCType τ) ): CStructType (τ :: τs)
  | bitfield {τs : List (Type u)} {α :Type u} [CTypeSize α] [IsIntegerType α ] (size : Nat) (storage_type : α ) ( can_store : size <= 8*(CTypeSize.size_of storage_type))   : CStructType ( α :: τs)

instance {α : List Type}: IsCType (CStructType α) where
  isCType := True
instance {α : List Type}: IsCFuncArgType (CStructType α)  where
  isCFuncArgType := True

example :CStructType [CUInt32Type] := CStructType.cons CUInt32Type CStructType.nil (by simp [IsVoidCType.isVoidCType];  )
/- next  example should fail -> we can prove it is false -/
-- example :CStructType [CVoidType, CUInt32Type] := CStructType.cons CVoidType (CStructType.cons CUInt32Type CStructType.nil (by simp [IsVoidCType.isVoidCType]; ) ) (by simp [IsVoidCType.isVoidCType]; decide; )
example := IsCFuncArgType.isCFuncArgType (CStructType [CUInt32Type, CVoidType])

inductive CUnionType (α: List Type) where
  | mk : CVarScope α → CUnionType α
instance {α: List Type} : IsCType (CUnionType α) where
  isCType := True
instance (α:List Type) : IsCFuncArgType (CUnionType α) where
  isCFuncArgType := True




/-- CFunction represents a C function type.

arguments → (does it have variable arguments? : Bool) → return type
TODO: one have to take care of void ctype would not be a part of argument types
-/

inductive CFunctionType (arg_types : CFuncArgVarScope γ) (elipses : Bool) (ret_type : Type u) [IsCType ret_type] where
  | mk : CFunctionType arg_types elipses ret_type
instance (arg_types : CFuncArgVarScope γ) (elipses : Bool) (ret_type : Type u) [IsCType ret_type] :
    IsCType (CFunctionType arg_types elipses ret_type) where
  isCType := True

example := CFuncArgVarScope.cons (CQualifiedType CInt32Type CTypeQualificationVolatile) (CFuncArgVarScope.cons (CFloatType .F64) CFuncArgVarScope.nil)
example : CFunctionType (CFuncArgVarScope.cons (CQualifiedType CInt32Type CTypeQualificationVolatile) (CFuncArgVarScope.cons (CFloatType .F64) CFuncArgVarScope.nil)) false (CVoidType ) :=
  CFunctionType.mk

example :=
  let args := CFuncArgVarScope.cons CInt32Type
              (CFuncArgVarScope.cons (CFloatType .F64)
              CFuncArgVarScope.nil)
  let ret := CVoidType
  (CFunctionType.mk : CFunctionType args false ret )


/- we need type production:

There are two options here:
1. we have declaration -- "type identifier;"
2. we have casting -- "(type)"
-/


end LeanC
