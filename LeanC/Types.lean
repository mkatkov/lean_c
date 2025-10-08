
/-!
Basic C type representations for lean_c - small, valid stub used by the roadmap.
This file intentionally keeps cstruct simple (identified by name). Field
descriptions and layout logic will live in a separate module to avoid nested
inductive definitions during initial prototyping.
-/

namespace LeanC

/- we want flexible size types, so we defne it as class -/

class CTypeSize (α : Type) where
  sizeOf : α -> Nat
  alignOf : α -> Nat

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

/--
Identifier, each identifier has it unique reference
-/
inductive Identifier where
  | id : Nat -> Identifier

class CType (α : Type) where
  toCType : α -> CType α

inductive CSimpleType where
  | cvoid  : CSimpleType
  | cint   : CIntSize -> Bool -> CSimpleType -- bitwidth, unsigned?
  | cfloat : CFloatSize -> CSimpleType

inductive CPointerType α where
  | mk : CType α -> CPointerType α
inductive CArrayType α where
  | mk : CType α -> Nat -> CArrayType α

inductive CInStructureField α where
  | simple : Identifier -> CSimpleType -> CInStructureField α
  | ptr    : Identifier -> CPointerType α -> CInStructureField α
  | array  : Identifier -> CArrayType α -> CInStructureField α

universe u

def HList (τs : List (Type u)) : Type u := τs.foldr Prod PUnit

@[match_pattern] def HList.nil : HList [] := PUnit.unit

@[match_pattern] def HList.cons {τ : Type u} {τs : List (Type u)} (x : τ) (xs : HList τs) :
    HList (τ :: τs) := (x, xs)

def HList.rec {motive : (τs : List (Type u)) → HList τs → Sort u}
    (nil : motive [] HList.nil)
    (cons : {τ : Type u} → {τs : List (Type u)} → (x : τ) → (xs : HList τs) →
              motive τs xs → motive (τ :: τs) (HList.cons x xs))
    {τs : List (Type u)} (xs : HList τs) : motive τs xs :=
  match τs, xs with
  | [], PUnit.unit => nil
  | _ :: _, (x, xs) => cons x xs (HList.rec nil cons xs)

instance : CType CSimpleType where
  toCType
  | x => CType x

inductive CType where
  | cvoid  : CType
  | cint   : IntSize -> Bool -> CType -- bitwidth, unsigned?
  | cfloat : FloatSize -> CType
  | cptr   : CType -> CType
  | carray : CType -> Nat -> CType
  | cstruct : FieldList -> CType -- struct identified by name; fields handled elsewhere
  | cunion : FieldList -> CType



end LeanC
