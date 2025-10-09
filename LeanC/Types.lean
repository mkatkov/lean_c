
/-!
Basic C type representations for lean_c - small, valid stub used by the roadmap.
This file intentionally keeps cstruct simple (identified by name). Field
descriptions and layout logic will live in a separate module to avoid nested
inductive definitions during initial prototyping.
-/

namespace LeanC

universe u
/- we want flexible size types, so we defne it as class -/

class CTypeSize (α : Type u) where
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
  | mk : Nat -> Identifier

inductive CSimpleType where
  | cvoid  : CSimpleType
  | cint   : CIntSize -> Bool -> CSimpleType -- bitwidth, unsigned?
  | cfloat : CFloatSize -> CSimpleType

mutual
inductive CType (α : Type) (β : Type) where
| void : CType α β
| int : CIntSize -> Bool -> CType α β
| float : CFloatSize -> CType α β
| ptr : CPointerType α β -> CType α β
| arr : CArrayType α β -> CType α β
| union :  CStruct α β -> CType α β
| struct : CStruct α β -> CType α β

inductive CPointerType (α : Type) (β : Type) where
  | mk : CType α β -> CPointerType α β
inductive CArrayType (α : Type) (β : Type) where
  | mk : CType α β -> Nat -> CArrayType α β

inductive CInStructureField (α : Type) (β : Type) where
  | simple : Identifier -> CSimpleType -> CInStructureField α β
  | ptr    : Identifier -> CPointerType α β -> CInStructureField α β
  | array  : Identifier -> CArrayType α β -> CInStructureField α β

inductive CStruct α β where
| intro (x: CInStructureField α β) : CStruct α β
| cons (x : CInStructureField α β) (xs : CStruct α β ) : CStruct α β

end

end LeanC
