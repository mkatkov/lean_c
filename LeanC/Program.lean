import LeanC.Context
import LeanC.Modules

/-!
# Programs — top level: modules + `main` (T5 stub)
-/

namespace LeanC

/-- WHAT a program is (draft): modules + designated `main` + pool. -/
structure CProgram where
  (mods : List CModule)
  (main : String)
  (pool : List LiteralPoolEntry)
  (isProgSound : Prop := True)

end LeanC
