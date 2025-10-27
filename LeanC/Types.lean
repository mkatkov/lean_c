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

- we are missing structure bits
-/

namespace LeanC

universe u

/-- IsCType is a type class for distinguishing C types from other types. -/
class IsCType (α : Type u) where
  /-- Proof that the type α is a C type -/
  isCType : Prop
class IsCFuncArgType (α : Type u) where
  /-- Proof that the type α is a C type -/
  isCType : Prop

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

/-- Integer sizes -/
inductive CIntSize where
  | i8
  | i16
  | i32
  | i64
  | i128

instance : CTypeSize CIntSize where
  sizeOf
    | CIntSize.i8   => 1
    | CIntSize.i16  => 2
    | CIntSize.i32  => 4
    | CIntSize.i64  => 8
    | CIntSize.i128 => 16
  alignOf
    | CIntSize.i8   => 1
    | CIntSize.i16  => 2
    | CIntSize.i32  => 4
    | CIntSize.i64  => 8
    | CIntSize.i128 => 16
  type_str
    | .i8 => "int8_t"
    | .i16 => "int16_t"
    | .i32 => "int32_t"
    | .i64 => "int64_t"
    | .i128 => "int128_t"

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


inductive CTypeQualificationNone where
| mk : CTypeQualificationNone
instance : CTypeQualification CTypeQualificationNone where
  isCTypeQualification := True

inductive CTypeQualificationConst where
  | Const
instance : CTypeQualification CTypeQualificationConst where
  isCTypeQualification := True

inductive CTypeQualificationVolatile where
  | Volatile
instance : CTypeQualification CTypeQualificationVolatile where
  isCTypeQualification := True

inductive CTypeQualificationRestrict where
  | Restrict
instance : CTypeQualification CTypeQualificationRestrict where
  isCTypeQualification := True


/-- Void type -/
inductive CVoidType (α : Type)  where
  | mk : CVoidType α
instance {α :Type} [CTypeQualification α] : IsCType (CVoidType α) where
  isCType := True

/-- Void type (α - type qualification) -/
inductive CCharType (α : Type) [CTypeQualification α]  where
  | mk : CCharType α
instance {α :Type} [CTypeQualification α] : IsCType (CCharType α) where
  isCType := True
instance {α :Type} [CTypeQualification α] : IsCFuncArgType (CCharType α) where
  isCType := True
instance {α :Type} [CTypeQualification α] : CTypeSize (CCharType α) where
  sizeOf _ := 1
  alignOf _ := 1
  type_str _ := "char"

/-- CTypedef represents a C typedef definition. -/
inductive CTypedef (α:Type u) [IsCType α] (β  : Type) [CTypeQualification β]   where
  | mk : CTypedef α β
instance (α : Type u) [IsCType α] {β  :Type} [CTypeQualification β]  : IsCType (CTypedef α β) where
  isCType := True
instance (α : Type u) [IsCType α] {β  :Type} [CTypeQualification β] : IsCFuncArgType (CTypedef α β) where
  isCType := True


inductive CIntType (sz : CIntSize) (sgn : Bool) (β  : Type) [CTypeQualification β] where
| mk : CIntType sz sgn β
instance (sz : CIntSize) (sgn : Bool) {β  :Type} [CTypeQualification β] : IsCType (CIntType sz sgn β) where
  isCType := True
instance (sz : CIntSize) (sgn : Bool) {β  :Type} [CTypeQualification β] : IsCFuncArgType (CIntType sz sgn β) where
  isCType := True

inductive CFloatType (sz : CFloatSize) (β  : Type) [CTypeQualification β] where
| mk : CFloatType sz β
instance (sz : CFloatSize) {β  :Type} [CTypeQualification β] : IsCType (CFloatType sz β) where
  isCType := True
instance (sz : CFloatSize) {β  :Type} [CTypeQualification β] : IsCFuncArgType (CFloatType sz β) where
  isCType := True

/-- CPointer_t represents a C pointer type.
There is no restriction to what pointer type can point to.
-/
inductive CPointer_t (α : Type u) [IsCType α] (β  : Type) [CTypeQualification β] where
  | mk : CPointer_t α β
instance (α : Type u) [IsCType α] {β  :Type} [CTypeQualification β] : IsCType (CPointer_t α β) where
  isCType := True

instance (α : Type u) [IsCType α] {β  :Type} [CTypeQualification β] : IsCFuncArgType (CPointer_t α β) where
  isCType := True


/-- CArray_t represents a C array type.

CArray has element type and size. It is usually stack allocated, when declared.
However we can define a pointer to an array type as well. This can be in heap, but we indicate that
each array has fixed size.
-/
inductive CArray_t (α : Type u) [IsCType α] (β  : Type) [CTypeQualification β] where
  | mk : Nat → CArray_t α β
instance (α : Type u) [IsCType α] {β  :Type} [CTypeQualification β] : IsCType (CArray_t α β) where
  isCType := True
instance (α : Type u) [IsCType α] {β  :Type} [CTypeQualification β] : IsCFuncArgType (CArray_t α β) where
  isCType := True


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

inductive CStructType (β:Type) (α:Type) [IdentifierProducer β  α ] (γ : Type) [CTypeQualification γ] where
  | mk  (τ : CVarScope δ) : CStructType β α γ
instance (β:Type) (α:Type) [IdentifierProducer β  α ] (γ : Type) [CTypeQualification γ] : IsCType (CStructType β α γ) where
  isCType := True
instance (β:Type) (α:Type) [IdentifierProducer β  α ] (γ : Type) [CTypeQualification γ] : IsCFuncArgType (CStructType β α γ) where
  isCType := True

inductive CUnionType (β:Type) (α:Type) [IdentifierProducer β  α ]  (γ : Type) [CTypeQualification γ] where
  | mk : CVarScope δ → CUnionType β α γ
instance (β:Type) (α:Type) [IdentifierProducer β  α ] (γ : Type) [CTypeQualification γ] : IsCType (CUnionType β α γ) where
  isCType := True
instance (β:Type) (α:Type) [IdentifierProducer β  α ] (γ : Type) [CTypeQualification γ] : IsCFuncArgType (CUnionType β α γ) where
  isCType := True


example : CStructType CTrivialLabelScopeProducer Nat CTypeQualificationNone := CStructType.mk (CVarScope.cons (CIntType .i32 true CTypeQualificationNone) (CVarScope.cons (CVoidType CTypeQualificationNone) CVarScope.nil) )


/-- CFunction represents a C function type.

arguments → (does it have variable arguments? : Bool) → return type
TODO: one have to take care of void ctype would not be a part of argument types
-/

inductive CFunctionType (arg_types : CFuncArgVarScope γ) (elipses : Bool) (ret_type : Type u) [IsCType ret_type] where
  | mk : CFunctionType arg_types elipses ret_type
instance (arg_types : CFuncArgVarScope γ) (elipses : Bool) (ret_type : Type u) [IsCType ret_type] :
    IsCType (CFunctionType arg_types elipses ret_type) where
  isCType := True

example : CFunctionType (CFuncArgVarScope.cons (CIntType .i32 true CTypeQualificationNone) (CFuncArgVarScope.cons (CFloatType .F64 CTypeQualificationNone) CFuncArgVarScope.nil)) false (CVoidType CTypeQualificationNone) :=
  CFunctionType.mk

example :=
  let args := CFuncArgVarScope.cons (CIntType .i32 true CTypeQualificationNone)
              (CFuncArgVarScope.cons (CFloatType .F64 CTypeQualificationNone)
              CFuncArgVarScope.nil)
  let ret := CVoidType CTypeQualificationNone
  (CFunctionType.mk : CFunctionType args false ret )


/- we need type production:

There are two options here:
1. we have declaration -- "type identifier;"
2. we have casting -- "(type)"
-/


end LeanC
