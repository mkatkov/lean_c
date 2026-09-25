import LeanC.Context

/-!
# Functions — signature stub (T5)

`call ↔ Func` cycle is broken by name (`CExpr.call` takes `fname`,
never a `Func` body; `Expr` never imports `Func`).

Costs (if any) are assigned per resource type via `CContext.setResource`
+ `CResource` instances in the caller's file.
-/

namespace LeanC

/-- WHAT a function is (draft): name only. `isFuncSound := True` is the
placeholder. -/
structure CFunc where
  (fname : String)
  (isFuncSound : Prop := True)

/-- C sketch: `void fname(void) { body }` (bodies are statement
sketches from `Stmt`, passed in as pre-rendered text for the draft). -/
def emitFunc (f : CFunc) (bodyStr : String) : String :=
  "void " ++ f.fname ++ "(void) { " ++ bodyStr ++ " }"

end LeanC
