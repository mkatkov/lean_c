
/-!
Basic C type representations for lean_c - small, valid stub used by the roadmap.
This file intentionally keeps cstruct simple (identified by name). Field
descriptions and layout logic will live in a separate module to avoid nested
inductive definitions during initial prototyping.
-/

namespace LeanC

inductive IntSize where
  | i8
  | i16
  | i32
  | i64

inductive FloatSize where
  | F16
  | F32
  | F64


/--
Identifier, each identifier has it unique reference
-/
inductive Identifier where
  | id : Nat -> Identifier

inductive FieldEntry where
  | fld : Identifier -> CType -> FieldEntry

inductive FieldList where
  | fields : List FieldEntry -> FieldList

inductive CType where
  | cvoid  : CType
  | cint   : IntSize -> Bool -> CType -- bitwidth, unsigned?
  | cfloat : FloatSize -> CType
  | cptr   : CType -> CType
  | carray : CType -> Nat -> CType
  | cstruct : FieldList -> CType -- struct identified by name; fields handled elsewhere
  | cunion : FieldList -> CType

end LeanC
