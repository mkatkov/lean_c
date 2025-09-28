/- A minimal scaffold for a Lean-driven bootstrap probe emitter.

   The intent: produce a tiny C probe (or JSON manifest) from Lean and provide
   a small proof that the emitted probe is syntactically limited to a
   project-local whitelist of statements. This file is intentionally small and
   self-contained so it can be used as a starting point for the full
   `LeanC.Bootstrap` implementation.
-/

namespace LeanC.Bootstrap

/- Define a tiny AST for the small subset we need in the probe. Keep it
   intentionally minimal: includes, calls to `printf` with a single string,
   and a `return` statement. Additional constructors can be added later.
-/

mutual
  inductive ProbeTop : Type
  | include : String → ProbeTop
  | function : String → List String → List ProbeStmt → ProbeTop

  inductive ProbeStmt : Type
  | call_printf : String → ProbeStmt
  | expr_stmt : String → ProbeStmt -- a catch-all for simple expressions
  | ret : Int → ProbeStmt
end

open ProbeTop ProbeStmt

/- A small whitelist predicate expressed as Bool: this makes it easy to prove
   equality against `true` for a concrete, computed probe. -/
def whitelist_bool : ProbeTop → Bool
| ProbeTop.include _ => true
| ProbeTop.function _ _ body =>
   let stmt_whitelist := fun (s : ProbeStmt) =>
      match s with
      | ProbeStmt.call_printf _ => true
      | ProbeStmt.expr_stmt _ => true
      | ProbeStmt.ret _ => true
   body.foldl (fun acc s => acc && stmt_whitelist s) true

/- A tiny emitter that turns the minimal AST into strings. In the full
   implementation this would perform escaping and target-specific formatting.
   Keep this simple for now. -/
def emit_stmt : ProbeStmt → String
| ProbeStmt.call_printf s => "printf(" ++ s ++ ");"
| ProbeStmt.expr_stmt s => s ++ ";"
| ProbeStmt.ret n => "return " ++ toString n ++ ";"

def emit_top : ProbeTop → String
| ProbeTop.include hdr => "#include " ++ hdr ++ "\n"
| ProbeTop.function name _ body =>
   let header := "int " ++ name ++ "() {\n"
   let body_str := String.intercalate "\n" (body.map emit_stmt)
   header ++ body_str ++ "\n}\n"

/- Example generator that produces a probe which prints sizeof/alignof results.
   This is intentionally concrete so we can state and prove properties about
   it. -/
def example_probe : List ProbeTop :=
   [ ProbeTop.include "<stdio.h>",
      ProbeTop.function "main" [] [ ProbeStmt.call_printf "\"{\\n  \\\"pointer_width\\\": %zu,\\n\"", ProbeStmt.ret 0 ]]

/- Simple Bool-valued check for the example probe being whitelisted. -/
def example_probe_whitelisted_bool : Bool :=
   (whitelist_bool (ProbeTop.include "<stdio.h>")) &&
   (whitelist_bool (ProbeTop.function "main" [] [ ProbeStmt.call_printf "\"{\\n  \\\"pointer_width\\\": %zu,\\n\"", ProbeStmt.ret 0 ]))

end LeanC.Bootstrap
