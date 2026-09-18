import LeanC.Context
import LeanC.Complexity

/-!
# Functions — signature + body bound + `declaredCost` check (T5 stub)

`call ↔ Func` cycle is broken by `declaredCost` (D4): `CExpr.call` takes
`fname + declaredCost`, never a `Func` body. `Func` later discharges
`bodyBound ≤ declaredCost` per func (`funcSound : BigO bodyBound …` —
per-func `example` in `Tests`, suffices for draft).
-/

namespace LeanC

/-- WHAT a function is (draft): name + worst body bound + advertised cost.
`isFuncSound := True` is the placeholder; the real obligation per func is
`BigO bodyBound.timeRep declaredCost.timeRep` (see `Tests`). -/
structure CFunc where
  (fname : String)
  (bodyBound : ResourceBound)
  (declaredCost : ResourceBound)
  (isFuncSound : Prop := True)

/-- C sketch: `void fname(void) { body }` (bodies are statement
sketches from `Stmt`, passed in as pre-rendered text for the draft). -/
def emitFunc (f : CFunc) (bodyStr : String) : String :=
  "void " ++ f.fname ++ "(void) { " ++ bodyStr ++ " }"

end LeanC
