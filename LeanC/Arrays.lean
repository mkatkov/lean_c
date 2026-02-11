import LeanC.Types
import LeanC.TypeClasses
import LeanC.Values

/-!
# Array Types for lean_c

This module defines core RAM related types:

CMemoryBlock -- represents pointer to void, but with associated
allocated size to guarantee within bounds operations.

In general we need a proof system that the block of memory is allocated and contains specific number of items.

This splits into 3 cases: stack allocation, RAM allocation and within code allocation (asm data blocks).

- Stack allocations are complicated, since we need to know the size of the stack and have
  a proof that current allocation does not exeed stack size. Since the size of the stack
  can sometimes be defined in the compiler options, that requires the interaction with compliter.
  We will not use this part now untill we can interoperate with toolchain.

- There are also global static allocations that are allocated as a part of a program (asm data block).
  There is guarantee that if program is loaded this part of memory is allocated.
  We would assume that global static arrays are allocated in data segment.
  Otherwise, there is no good way to define even "Hello world!" string.

- Dynamically allocaed arrays should have a dedicated pointer variable. It can have 2 states:
  1. Allocated - with the proof it is allocated
  2. NotAllocated - not yet initialized or allocation failed.


For array reasoning we also need to know the size of allocated block in bytes? and in type units.
 This can be done in 2 ways.

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

So ingeneral, CArray class should have an information about pointer, allocation state and allocated size.
It should support a proof that when element of an array is accessed it is in allocated array, and
the element is entierly within allocated region.

We need to define CMemoryAllocation operation that have 2 branches: succesfull allocation and failure.
Two branches have two different types of variables

we can have either smart or private constructors in lean to prevent unauthorized modifications.

Anfortunately at this level of description we can have only `sorry` proof for memory allocations.
The real proof would require a system allocation core to be implemented within this system.


Please note that we are slowily building variable properties.
There are 2 ways to do it.
1. Some concrete properties, like allocated array or size of allocation can be defined
  via classes and class implementation, since these are basic properties defined for
  all arrays, and hat is essential for programming we can keep it this way.

2. For each variable we can have its type and list of properties. Than we need a way to reflect
  which property we need at a particular moment for the proof.

For instance we now have a problem of making sure that the index is within a bound.
For concreteness a[i]. One way to get the proof that i is within the bound is to
have ```C
if( i< len(a) ) b= a[i];
```
meaning that if have 2 branches with one branch having the proof that i < len(a)
and another branch having a proof of negation of the proposition, so the variable i can
be assotiated with corresponding proposition in each branch, that will support the proof
of withing bounds. Suppose that we have ```C
if( i< len(a) ){
 int j = i/2;
b= a[j];
}
``` Nw we have 2 propositions (i< len(a) and j= i/2) how do we select the correct proposition in the
list of propositions? There are maybe other propositions related to different aspects of the code, not related to indexing.
-/

namespace LeanC
universe u

/-- we need to denote an index of an array.
  index can be a Natural value, or it can be a variable with non-negative value.
  We cannot access the region before the allocated region.


  value is a temporal solution
  we need to check if this will work later when we do array operations.
 -/
class CIndex (α : Type u) where
  isIndex : Prop
  value : Nat

/-- CArray defines a set of propositions to ensure arrays is well behaving -/
class CArray (α:Type u) where
  /-- when accessing, reallocating and freeing array we need to be sure it is allocated.
    when allocating array we need to be sure it is not yet allocated, otherwise we get a memory leak.
  -/
  isAllocated : Prop
  /-- for free and reallocation operation we need to be sure the array is still allocated
    and it was dynamically allocated -/
  isDynamicallyAllocated : Prop
  /-- for accessing elements of an array we need to be sure the array is allocated and
    the indexed element is within allocated region. -/
  within_bounds (β  : Type ) [CIndex β ] : Prop

instance {α : Type u}  [IsPointedCType α] : CArray (CPointerType α ) where
  isAllocated := False
  isDynamicallyAllocated := False
  within_bounds (β  : Type ) [CIndex β] := False


/-- an implementation of constant index as a natural number -/
inductive CConstIndex (n:Nat) : Type (u+1) where
| mk
instance {n:Nat} : CIndex (CConstIndex n) where
  isIndex := True
  value := n

/-- an implementation of fixed-size dynamic index (to distinguish from global static) -/
inductive CFixedIndex (n:Nat) : Type (u+1) where
| mk
instance {n:Nat} : CIndex (CFixedIndex n) where
  isIndex := True
  value := n

/-- An implementation of index variable, conceptually this can be stored in the
  CVarScope and behave as any other C variable, but with the rules assigned to it.
  α is the underlying type of index holding variable. It should be of integer type,
  but we do not require it to be strictly positive number, since for example we an have a situation
  where negative number means no value. Therefore, we need a proof that the index is nonegative.

  This will support the scenario when somwone would have an integer variable, but in certain situation
  can make an index out of integer, for example by checking the the value is non-negative, like
  if(i>=0) b= a[i];

  uints have the proof by construction that they are nonegative.
-/
inductive CVarIndex (α : Type u) [IsIntegerType α] [IsIntegerNonnegative α]
  (non_negative: IsIntegerNonnegative.isNonnegative α ) : Type (u+1) where
| mk : CVarIndex α non_negative
instance (α : Type u) [IsIntegerType α] [IsIntegerNonnegative α]
  (p: IsIntegerNonnegative.isNonnegative α ) :
    CIndex (CVarIndex α p ) where
  isIndex := True
  value := 0 -- TODO: reference the underlying integer value or provide appropriate extraction/proof
  -- essentially we need some proofs related to value, so do not need Nat.
instance (α : Type u) [IsIntegerType α] [IsIntegerNonnegative α]
  (p: IsIntegerNonnegative.isNonnegative α ) :
    IsCType (CVarIndex α p ) where
  isCType := True

example := CVarIndex CUInt64Type (by simp [c_uint_nonnegative])

/-- Global static memory block (should be inimialized outside any function)
We are assuming that compiler put it in the data/bss segment of program, and it is
loaded together with the program.
 -/
inductive CGlobalStaticMemoryBlock (α : Type u) [IsPointedCType α] (n:Nat) where
| mk
instance (α : Type u) [IsPointedCType α] (n:Nat) :
  IsCType (CGlobalStaticMemoryBlock α n) where
  isCType := True
instance {α : Type u}  [IsPointedCType α] {n:Nat} : CArray (CGlobalStaticMemoryBlock α n) where
  isAllocated := True
  isDynamicallyAllocated := False
  within_bounds (β  : Type ) [CIndex β] := CIndex.value β < n

/-- For fixed size memory blocks we know the allocated size
so we can supply it with the parameter. This is dynamically allocated
block of memory, so introduction of this variable cannot be made public.
For instance the variable should be allocated and can fail
so the programm should expose 2 branches one containing
CFixedSizeMemoryBlock and another one containig failed allocation.

Thechnically this is a pointer with allocated size information, so it can be added to CVarScore
as a type.
-/
inductive CFixedSizeMemoryBlock (α : Type u) [IsPointedCType α] (n:Nat) where
| private mk : CFixedSizeMemoryBlock α n
instance (α : Type u) [IsPointedCType α] (n:Nat) : IsCType (CFixedSizeMemoryBlock α n) where
  isCType := True
  /-- CFixedSizeMemoryBlock is a specialized pointer -> Coercion -/
instance {α:Type u} [IsPointedCType α] (_n:Nat)  : CoeOut (CFixedSizeMemoryBlock α _n) (CPointerType α) where
  coe := fun _ => CPointerType.mk
instance {α : Type u}  [IsPointedCType α] {n:Nat} : CArray (CFixedSizeMemoryBlock α n) where
  isAllocated := True
  isDynamicallyAllocated := True
  within_bounds (β  : Type ) [CIndex β] := CIndex.value β < n

/-- For dynamically allocated memory block we need a variable that will store
the allocated size, so we need a reference to the variable.
Roughly speaking this only possible after we define Variables, and references to variables.
-/
inductive CDynamicMemoryBlock
  (α : Type u) [IsPointedCType α]
  (β  : Type u) [IsIntegerType β ] [IsIntegerNonnegative β  ]
  (non_negative: IsIntegerNonnegative.isNonnegative β  )
  where
| mk : CDynamicMemoryBlock α β non_negative
instance {α:Type u} [IsPointedCType α]
  {β  : Type u} [IsIntegerType β ] [IsIntegerNonnegative β  ]
  {non_negative: IsIntegerNonnegative.isNonnegative β }  : CoeOut (CDynamicMemoryBlock α β non_negative ) (CPointerType α) where
  coe := fun _ => CPointerType.mk
instance  {α:Type u} [IsPointedCType α]
  {β  : Type u} [IsIntegerType β ] [IsIntegerNonnegative β  ]
  {non_negative: IsIntegerNonnegative.isNonnegative β } :
    CArray (CDynamicMemoryBlock α β non_negative) where
  isAllocated := True
  isDynamicallyAllocated := True
  within_bounds (γ   : Type ) [CIndex γ ] := CIndex.value γ  < CIndex.value (CVarIndex β non_negative)

example := CDynamicMemoryBlock CInt8Type CUInt64Type (by simp [c_uint_nonnegative])


/-- This is alternative formulation for allocaed blocks in a basic variant, since they all return the same
type of information. See examples for the definition of types.

Since we do not know what would work better we will keep both before decision.
-/

inductive CArrayType (α : Type u) [IsPointedCType α] : (n : Type (u+1)) -> Type (u+1) where
| global_static (i : Nat) : CArrayType α (CConstIndex i)
| private fixed_size_dynamic (i:Nat) : CArrayType α (CFixedIndex i)
| private dynamic (β  : Type u) [IsIntegerType β ] [IsIntegerNonnegative β  ]
  (non_negative: IsIntegerNonnegative.isNonnegative β  ) : CArrayType α (CVarIndex β non_negative)

example := CArrayType.global_static (α := CPointerType CVoidType) 5
example := CArrayType.fixed_size_dynamic (α := CPointerType CUInt16Type ) 15
example := CArrayType.dynamic (α := CPointerType CUInt16Type) (β := CUInt8Type) (by simp [c_uint_nonnegative])

instance {α : Type u} [IsPointedCType α] {n:Type (u+1)} : IsCType (CArrayType α n) where
  isCType := True
  /-- CFixedSizeMemoryBlock is a specialized pointer -> Coercion -/
instance {α:Type u} [IsPointedCType α] {n:Type (u+1)}: CoeOut (CArrayType α n) (CPointerType α) where
  coe := fun _ => CPointerType.mk

instance {α : Type u} [IsPointedCType α] {i:Nat} : CArray (CArrayType α (CConstIndex i)) where
  isAllocated := True
  isDynamicallyAllocated := False
  within_bounds (γ : Type) [CIndex γ] := CIndex.value γ < i

instance {α : Type u} [IsPointedCType α] {i:Nat} : CArray (CArrayType α (CFixedIndex i)) where
  isAllocated := True
  isDynamicallyAllocated := True
  within_bounds (γ : Type) [CIndex γ] := CIndex.value γ < i

instance {α : Type u} [IsPointedCType α] {β : Type u} [IsIntegerType β ] [IsIntegerNonnegative β ]
  (non_negative : IsIntegerNonnegative.isNonnegative β) :
    CArray (CArrayType α (CVarIndex β non_negative)) where
  isAllocated := True
  isDynamicallyAllocated := True
  within_bounds (γ : Type) [CIndex γ] :=
    CIndex.value γ < CIndex.value (CVarIndex β non_negative)


end LeanC
