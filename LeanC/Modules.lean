import LeanC.Context
import LeanC.Func

/-!
# Modules — translation units: funcs + literal pool (T5 stub)
-/

namespace LeanC

/-- WHAT a module is (draft): a translation unit — function list plus the
live global-statics pool (`LiteralPoolEntry`, see `Context`). -/
structure CModule where
  (funcs : List CFunc)
  (pool : List LiteralPoolEntry)
  (isModSound : Prop := True)

end LeanC
