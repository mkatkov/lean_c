namespace LeanC

/--
Production class for generating code
At some points we need to have context (e.g. for resolving identifiers)

The main question here is to whether we need a resolver type class for prod function?
The point is that we can define a resolver type for a (production type, context type, and a value type)
-/
class CProduction (α : Type ) ( β : Type) where
  /-- prod will produce output to the given stream for x in context -/
  prod : IO.FS.Stream → ( x: α  ) →  (context :β  ) →  EIO IO.Error Unit
  correctness_proof : ( x: α ) → (context :β ) → Prop

end LeanC


inductive Capacity where
| c1 : Capacity
| c2 : Capacity

class CapacityValue (a:Type) where
  capacity_of : Nat

inductive WithinCapacity (c:Type) (n:Nat) where
 | within_capacity [CapacityValue c] ( _ : n<= CapacityValue.capacity_of c) : WithinCapacity c n

instance : CapacityValue Capacity where
  capacity_of := 4 -- say 4 bytes

example : WithinCapacity Capacity 2 :=
  WithinCapacity.within_capacity (by decide)
