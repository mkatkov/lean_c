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

/-- IsCType is a type class for distinguishing C types from other types. -/
class IsCType (α : Type u) where
  /-- Proof that the type α is a C type -/
  isCType : Prop
class IsCFuncArgType (α : Type u) where
  /-- Proof that the type α is a C type -/
  isCFuncArgType : Prop
/-- All function types are c types -/
instance {α :Type u} [IsCFuncArgType α] : IsCType α where
  isCType := True
class IsIntegerType (α : Type u) where
  /-- Proof that the type α is an integer C type -/
  isIntegerType : Prop
class CSizeOf (α : Type u) where
  size_of : Nat

/--
IdentifierProducer is a type class for producing unique identifiers for types, labels or identifiers.
α is producer flavor type
β is scope type - by abuse of notation it is also identifier
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
  | cons {τs : List (Type u)} (τ : Type u ) [IsCType τ] (_ :CVarScope τs)  : CVarScope (τ :: τs)


example := CVarScope.nil
inductive CFuncArgVarScope : (List (Type u)) -> Type (u+1) where
  | nil : CFuncArgVarScope []
  | cons {τs : List (Type u)} (τ : Type u ) [IsCFuncArgType τ] (_ :CFuncArgVarScope τs)  : CFuncArgVarScope (τ :: τs)


/--
we want flexible size types, so we defne it as class -/
class CTypeSize (α : Type u) where
  sizeOf : α -> Nat
  alignOf : α -> Nat
  type_str : α -> String

inductive CFloatSize where
  | F16
  | F32
  | F64

instance : CTypeSize CFloatSize where
  sizeOf
    | CFloatSize.F16 => 2
    | CFloatSize.F32 => 4
    | CFloatSize.F64 => 8
  alignOf
    | CFloatSize.F16 => 2
    | CFloatSize.F32 => 4
    | CFloatSize.F64 => 8
  type_str
    | .F16 => "float16_t"
    | .F32 => "float32_t"
    | .F64 => "float64_t"

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

/-- Void type (α - type qualification) -/
inductive CCharType (α : Type) [CTypeQualification α]  where
  | mk : CCharType α
instance {α :Type} [CTypeQualification α] : IsCType (CCharType α) where
  isCType := True
instance {α :Type} [CTypeQualification α] : IsCFuncArgType (CCharType α) where
  isCFuncArgType := True
instance {α :Type} [CTypeQualification α] : CTypeSize (CCharType α) where
  sizeOf _ := 1
  alignOf _ := 1
  type_str _ := "char"

/-- CTypedef represents a C typedef definition. -/
inductive CTypedef (α:Type u) [IsCType α]   where
  | mk : CTypedef α
instance (α : Type u) [IsCType α] : IsCFuncArgType (CTypedef α) where
  isCFuncArgType := True


inductive CIntType (sz : CIntSize) (sgn : Bool) (β  : Type) [CTypeQualification β] where
| mk : CIntType sz sgn β
instance (sz : CIntSize) (sgn : Bool) {β  :Type} [CTypeQualification β] : IsCFuncArgType (CIntType sz sgn β) where
  isCFuncArgType := True

inductive CUInt8Type where
| mk : CUInt8Type
instance : IsCType CUInt8Type where
  isCType := True
instance : IsCFuncArgType CUInt8Type where
  isCFuncArgType := True
instance : CSizeOf CUInt8Type where
  size_of := 1

inductive CUInt16Type where
| mk : CUInt16Type
instance : IsCType CUInt16Type where
  isCType := True
instance : IsCFuncArgType CUInt16Type where
  isCFuncArgType := True
instance : CSizeOf CUInt16Type where
  size_of := 2

inductive CUInt32Type where
| mk : CUInt32Type
instance : IsCType CUInt32Type where
  isCType := True
instance : IsCFuncArgType CUInt32Type where
  isCFuncArgType := True
instance : CSizeOf CUInt32Type where
  size_of := 4

inductive CUInt64Type where
| mk : CUInt64Type
instance : IsCType CUInt64Type where
  isCType := True
instance : IsCFuncArgType CUInt64Type where
  isCFuncArgType := True
instance : CSizeOf CUInt64Type where
  size_of := 8


inductive CInt8Type where
| mk : CInt8Type
instance : IsCType CInt8Type where
  isCType := True
instance : IsCFuncArgType CInt8Type where
  isCFuncArgType := True
instance : CSizeOf CInt8Type where
  size_of := 1

inductive CInt16Type where
| mk : CInt16Type
instance : IsCType CInt16Type where
  isCType := True
instance : IsCFuncArgType CInt16Type where
  isCFuncArgType := True
instance : CSizeOf CUInt16Type where
  size_of := 2

inductive CInt32Type where
| mk : CInt32Type
instance : IsCType CInt32Type where
  isCType := True
instance : IsCFuncArgType CInt32Type where
  isCFuncArgType := True
instance : CSizeOf CUInt32Type where
  size_of := 4

inductive CInt64Type where
| mk : CInt64Type
instance : IsCType CInt64Type where
  isCType := True
instance : IsCFuncArgType CInt64Type where
  isCFuncArgType := True
instance : CSizeOf CUInt64Type where
  size_of := 8


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


/-- we need to build scope together with the type
since for the struct to be usefull we need to be able to acces it
By the contract the first item in the scope is a type that can produce all identifiers in this scope.

The first list is a list of identifier producers for argument types having identifiers in it, like CStruct_t
The rest is just CTypes.
-/

def CTrivialLabelScopeProducer := Unit
instance : IdentifierProducer CTrivialLabelScopeProducer Nat where
  produce_identifier val stream := stream.putStr s!"id_{val}"
  can_produce _ := True
  are_identifiers_unique _ := True

inductive CStructScope : (List (Type u)) -> Type (u+1) where
  | nil : CStructScope []
  | cons {τs : List (Type u)} (τ : Type u ) [IsCType τ] (_ :CStructScope τs)  : CStructScope (τ :: τs)
  | bitfield {τs : List (Type u)} (storage_type : Type u) [IsIntegerType storage_type] [CSizeOf storage_type] (size : Nat) (_:size <= (CSizeOf.size_of storage_type)) (_ :CStructScope τs)  : CStructScope (storage_type :: τs)


inductive CStructType (β:Type) (α:Type) [IdentifierProducer β  α ] (γ:CVarScope δ) where
  | mk : CStructType β α γ
instance (β:Type) (α:Type) [IdentifierProducer β  α ] : IsCType (CStructType β α γ) where
  isCType := True
instance (β:Type) (α:Type) [IdentifierProducer β  α ] : IsCFuncArgType (CStructType β α γ) where
  isCFuncArgType := True

inductive CUnionType (β:Type) (α:Type) [IdentifierProducer β  α ] where
  | mk : CVarScope δ → CUnionType β α
instance (β:Type) (α:Type) [IdentifierProducer β  α ] : IsCType (CUnionType β α) where
  isCType := True
instance (β:Type) (α:Type) [IdentifierProducer β  α ] : IsCFuncArgType (CUnionType β α) where
  isCFuncArgType := True


example : CStructType CTrivialLabelScopeProducer Nat (CVarScope.cons (CUInt32Type) (CVarScope.cons (CVoidType ) CVarScope.nil) ) := CStructType.mk
example: Prop :=
  let st := CStructType CTrivialLabelScopeProducer Nat (CVarScope.cons (CUInt32Type) (CVarScope.cons (CVoidType ) CVarScope.nil) )
  IsCFuncArgType.isCFuncArgType st

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
