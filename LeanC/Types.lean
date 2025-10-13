import LeanC.Prod

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

/--
Identifier, each identifier has it unique reference
-/
inductive Identifier where
  | mk : Nat -> Identifier

/--
This is trivial identifier resolver
-/
instance : CResolver Unit Identifier String where
resolve := fun _ value => match value with
  | .mk n =>  "id_" ++ toString n

mutual
inductive CType (α : Type) (β : Type) where
| void : CType α β
| int : CIntSize -> Bool -> CType α β
| float : CFloatSize -> CType α β
| ptr : CPointerType α β -> CType α β
| arr : CArrayType α β -> CType α β
| union :  CStruct α β -> CType α β
| struct : CStruct α β -> CType α β
| func : CFunction α β → CType α β

inductive CFunction (α : Type) (β : Type) where
| mk : Identifier → (args: CStruct α β) → (ellipses : Bool) → CType α β -> CFunction α β
inductive CPointerType (α : Type) (β : Type) where
  | mk : CType α β -> CPointerType α β
inductive CArrayType (α : Type) (β : Type) where
  | mk : CType α β -> Nat -> CArrayType α β

inductive CInStructureField (α : Type) (β : Type) where
  | int : Identifier → CIntSize -> Bool -> CInStructureField α β
  | float : Identifier → CFloatSize -> CInStructureField α β
  | simple : Identifier -> CSimpleType -> CInStructureField α β
  | ptr    : Identifier -> CPointerType α β -> CInStructureField α β
  | array  : Identifier -> CArrayType α β -> CInStructureField α β

inductive CStruct α β where
| intro (x: CInStructureField α β) : CStruct α β
| cons (x : CInStructureField α β) (xs : CStruct α β ) : CStruct α β

end

/--
For code production we need to know whether we have declaration or casting request
-/
inductive CTypeProd where
| decl
| cast

/--
For type production we need to specify names of variables in declaration
this is not required, though, for casting
-/
class CType_CIdentifier_Resolver (α : Type u ) ( β : Type v) (γ :Type w) where
 type_prod : CTypeProd
 identifier_resolver :  ( context : α ) -> (value: β) → γ

/--
Pointer production depends on few factors:
1. whether it is declaration or casting
2. on pointer type, for instance function pointer is different than int pointer.

conceptually we should use resolver to get correct information
-/
instance : CProduction (context_t :Type) ( CPointerType (α : Type) ( β : Type)  ) ( resolver_result_t   )  where
 prod ( pt : CPointerType α β ) stream resolver context :=
  let prod_type := resolver.type_prod
  match pt with
    | .mk (t: CType α β ) => match t with
     | .func (_fa : CFunction α β ) => do
        stream.putStr "("
        stream.putStr ")"
        throw (.userError "no pure function type declaration even ffor pointers")
     | _ => stream.putStr "_"



instance : CProduction context (CType α β ) context where
 prod
 | .void => fun stream _ _ => stream.putStr "void"
 | .int sz sgn => fun stream _ _ => match sgn with
   | false =>  stream.putStr ("u" ++ CTypeSize.type_str sz)
   | true => stream.putStr ( CTypeSize.type_str sz )
 | .float sz => fun stream _ _ => stream.putStr (CTypeSize.type_str sz)
 | .ptr pt => fun stream r c => CProduction.prod pt stream r c
 | .arr _arr => fun _stream r c => CProduction.prod _arr stream r c
 | .union _s => fun stream r c => stream.putStr "union"
 | .struct _s => fun stream r c => stream.putStr "struct"
 | .func _f => fun _stream r c => throw (.userError "no pure function type declaration")


instance : CProduction (CTypeProd α β)  where
 prod
  | .decl a =>  CProduction.prod a
  | .cast a => CProduction.prod a

end LeanC
