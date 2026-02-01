import LeanC.Types
/-!
Literal AST and serializers for lean_c - stub.
-/


namespace LeanC

/-- TypedValue is a value of type β that can be casted (stored) in var of type α -/
inductive TypedValue (α : Type) [IsCType α] (β :Type) ( v : β) ( can_cast : β -> α -> Prop )
| mk : TypedValue α β v can_cast

instance {α : Type} [IsCType α] {β :Type} {v : β} ( can_cast : β -> α -> Prop ) : IsCType (TypedValue α β v can_cast) where
  isCType := True

end LeanC
