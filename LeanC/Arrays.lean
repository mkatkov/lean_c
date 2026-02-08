import LeanC.Types
/-!
# Array Types for lean_c

This module defines core RAM related types:

CMemoryBlock -- represents pointer to void, but with associated
allocated size to guarantee within bounds operations.

CArray -- MemoryBlock with Associated Type

CSlice -- a subarray (Rust inspired name)

In general we need a proof system that the block of memory is allocated.
This splits into 2 cases: stack allocation and RAM allocation.
Stack allocations are complicated, since we need to know the size of the stack and have
a proof that current stack allocation does not break stack allocation. Since the size of the stack
can sometimes be defined in the compiler options, that requires the interaction with compliter.
There are also static allocations that are allocated as a part of program. There is guarantee that
if program is loaded this part of memory is allocated.

For array reasoning we also need to know the size of allocated block
in bytes? and in type units. This can be done in 2 ways.

1. the allocation size is known during compilation time and does not change (like incase of static allocations).
2. the allocation size is dynamically changing, like when arrays are growing and one have to
   reallocate memory.

Fixed allocations can have allocation size as Nat parameter.
Dynamic allocations should have a variable of integer type that will track the allocation size
For this we need to define a variable reference type. The difficulty is that the variable can be defined
very deep inside the function tree, and inside the structures within these functions, and then
transfered to another function. Rust, c++ solves this by associating size variable with pointed in the
structure, but that not the only way. For instance it is possible that there are sinchronized arrays
that have the same size, so we need to keep only one size value and reallocate all arrays at once.
(for example by joining bitfields into one large bitfield array, and saving memory of the structure containing it).

The pointer of the array and size variables can be defined in the allocated memories (say an array
referencing another arrays). So we need to be sure that at the time of the access to the array variables these pointers are stil allocated.

We need to define CMemoryAllocation operation that have 2 branches: succesfull allocation and failure.
Two branches have two different types of variables

we can have either smart or private constructors in lean to prevent unauthorized modifications.

Anfortunately at this level of description we can have only `sorry` proof for memory allocations.
The real proof would require a system allocation core to be implemented within this system.
-/

namespace LeanC
universe u

inductive CStaticMemoryBlock (n:Nat) where
| private mk

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

end LeanC
