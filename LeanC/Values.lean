import LeanC.Types
/-!
Literal AST and serializers for lean_c - stub.
-/


universe u

namespace LeanC

class IsIntegerNonnegative (α : Type u) [IsIntegerType α] where
 isNonnegative : Prop

instance {sz : CIntSize} : IsIntegerNonnegative (CIntType sz false) where
 isNonnegative := True

example := IsIntegerNonnegative.isNonnegative (CIntType CIntSize.I8 false)

theorem c_uint_nonnegative {sz : CIntSize} :
  IsIntegerNonnegative.isNonnegative (CIntType sz false) := by
  simp [IsIntegerNonnegative.isNonnegative]

-- example :=
--   let t := (CIntType CIntSize.I8 false )
--   let ( p : IsIntegerNonnegative.isNonnegative t)


/-- TypedValue is a value of type β that can be casted (stored) in var of type α -/
inductive TypedValue (α : Type) [IsCType α] (β :Type) ( v : β) ( can_cast : β -> α -> Prop )
| mk : TypedValue α β v can_cast

instance {α : Type} [IsCType α] {β :Type} {v : β} ( can_cast : β -> α -> Prop ) : IsCType (TypedValue α β v can_cast) where
  isCType := True

end LeanC
