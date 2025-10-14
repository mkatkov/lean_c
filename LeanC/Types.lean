import LeanC.Prod

/-!
# C Type System for lean_c

This module defines the core C type system including:
- Basic types (integers, floats, void)
- Composite types (pointers, arrays, structs, unions, functions)
- Type size and alignment calculations
- Type resolution and code generation framework

The implementation uses mutual inductive definitions to handle recursive type
dependencies (e.g., structs containing pointers to other structs).
Type production and identifier resolution are handled through type classes
to support different code generation contexts.

we still miss typedef production for readability
-/

namespace LeanC

universe u
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

/--
Identifier, each identifier has it unique reference that can be used to resolve to the name in production
-/
inductive Identifier where
  | mk : Nat -> Identifier

/--
This is trivial identifier resolver it resolve identifier to string by prefixing
it with "id_" in empty context. this is good enough if one can prove that all identifiers
are unique in the given context.

Since this resolver cannot be proven to be correct in all contexts, it should not be used,
and is shown as an example how resolver can be defined.
On the other hand, this resolver can be used for internal debugging.
-/
instance {α : Type}: CValueResolver α Empty Identifier String where
get_value := fun _ _ value => match value with
  | .mk n =>  "id_" ++ toString n


mutual

/-- CType represent all C types. -/
inductive CType where
| void : CType
| int : CIntSize -> Bool -> CType
| float : CFloatSize -> CType
| ptr : CPointerType  -> CType
| arr : CArrayType  -> CType
| union :  CStruct  -> CType
| struct : CStruct  -> CType
| func : CFunction  → CType


/-- CFunction represents a C function type.

identifier → arguments → (does it have variable arguments? : Bool) → return type -/
inductive CFunction  where
| mk : Identifier → (args: CStruct ) → (ellipses : Bool) → CType  -> CFunction

/-- CPointerType represents a C pointer type.

There is no restriction to what pointer type can point to.
-/
inductive CPointerType  where
  | mk : CType  -> CPointerType

/-- CArrayType represents a C array type.

CArray has element type and size. It is usually stack allocated, when declared.
However we can define a pointer to an array type as well. This can be in heap, but we indicate that
each array has fixed size.
-/
inductive CArrayType  where
  | mk : CType  -> Nat -> CArrayType

inductive CStruct where
| nil : CStruct
| cons (x : CType) (id: Identifier) (xs : CStruct  ) : CStruct

end

/-- uint32_t -/
def _example1 : CType :=
  .int .i32 true

/-- uint32_t* -/
def _example2 : CType  :=
  .ptr (.mk (.int .i32 true))
/-- uint32_t[10] -/
def _example3 : CType  :=
  .arr (.mk (.int .i32 true) 10)
/-- struct { uint32_t; float64_t; } -/
def _example4 : CType  :=
  .struct (.cons (.int .i32 true) (.mk 0) (.cons (.float .F64) (.mk 1) .nil))
/-- void function_with_identifier_1 (uint32_t) -/
def _example5 : CType  :=
  .func (.mk (Identifier.mk 1) (.cons (.int .i32 true) (.mk 0) .nil) false (.void))

/--
For code production we need to know whether we have a declaration or a casting request
-/
inductive CTypeDeclarationProd where
| mk : CType → Identifier → CTypeDeclarationProd

inductive CTypeCastProd where
| mk : CType -> CTypeCastProd

mutual
def produce_field_types (stream : IO.FS.Stream) (sep:String) (s : CStruct)  : EIO IO.Error Unit := match s with
  | .nil => pure ()
  | .cons t _id ts => do
    produce_type_signature stream t
    stream.putStr sep
    produce_field_types stream sep ts

def produce_type_signature (stream : IO.FS.Stream) (t : CType)  : EIO IO.Error Unit := match t with
  | .void => stream.putStr "void"
  | .int sz sgn  => match sgn with
    | false =>  stream.putStr ("u" ++ CTypeSize.type_str sz )
    | true => stream.putStr (CTypeSize.type_str sz )
  | .float sz => stream.putStr (CTypeSize.type_str sz )
  | .ptr pt => match pt with /- function pointer is special and cannot be casted actually -/
    | .mk (t: CType) => do
      stream.putStr "("
      produce_type_signature stream t
      stream.putStr "*)"
  | .arr arr => match arr with
    | .mk (t: CType) n => do
      produce_type_signature stream t
      stream.putStr ("[" ++ toString n ++ "]")
  | .union s  => do
      stream.putStr "union{"
      produce_field_types stream "; " s
      stream.putStr "}"
  | .struct s => do
      stream.putStr "struct{"
      produce_field_types stream "; " s
      stream.putStr "}"
  | .func f => match f with
      | .mk _id args ellipses ret => do
        produce_type_signature stream ret
        stream.putStr " ("
        produce_field_types stream ", " args
        if ellipses then
          match args with
          | .nil => stream.putStr "..."
          | _ => stream.putStr ", ..."
        stream.putStr ")"

end
set_option diagnostics true

instance :
    CProduction context_t CTypeCastProd resolver_t  where
 prod stream _context ct _resolver := do match ct with
  | .mk t => do
    stream.putStr "("
    produce_type_signature stream t
    stream.putStr ")"



end LeanC
