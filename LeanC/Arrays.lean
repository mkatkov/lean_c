import LeanC.Types
/-!
# Array Types for lean_c

This module defines core RAM related types:

CMemoryBlock -- represents pointer to void, but with associated
allocated size to guarantee within bounds operations.

CArray -- MemoryBlock with Associated Type

CSlice -- a subarray (Rust inspired name)

In general we need a proof system that the block of memory is allocated.
The guarantee can be obtained by generating allocation call as a result of the
construction operator, but we need to take into account that allocation can fail.
Conceptually, both branches -- successive allocation and failed allocation should be
processed.

we can have either smart or private constructors.
-/

namespace LeanC
universe u

/-- For fixed size memory blocks we know the allocated size
so we can supply it with the parameter
-/
inductive CFixedSizeMemoryBlock (n:Nat) where
| mk

/-- For dynamically allocated memory block we need a variable that will store
the allocated size, so we need a reference to the variable.
Roughly speaking this only possible after we define Variables, and references to variables.
-/
inductive CDynamicMemoryBlock where
| mk


/-- CMemoryBlock represents allocated memory block
It is technically equivalent to (void *) type with allocated size information.
It represents succesifully allocated block of RAM
Technically Mempory block can be an argument of f(void *)

There are 2 way to allocate memory:
- we can have a fixed size allocation known at compile time
- dynamic allocations, we need a variable storing actual value
-/
inductive CMemoryBlock (α:Type u) where
| mk

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


end LeanC
