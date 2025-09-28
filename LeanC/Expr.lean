/-! Expression AST stubs for lean_c. -/
namespace LeanC

inductive Expr where
  | litInt : Int -> Expr
  | var : Name -> Expr
  | add : Expr -> Expr -> Expr

end LeanC
