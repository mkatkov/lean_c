/-!
Literal AST and serializers for lean_c - stub.
-/
namespace LeanC

/-- Integer literal with optional suffix info. -/
structure IntLit where
  value : Int
  width : Option Nat -- bitwidth hint

def IntLit.default : IntLit := { value := 0, width := none }

end LeanC
