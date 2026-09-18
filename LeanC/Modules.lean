import LeanC.Context
import LeanC.Complexity
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

/-- Fold `bodyBound`s by sequencing (time adds, memory maxes); empty
module costs zero. Uses `seqBound`'s rule inline (same leaf reason as
`Context`: `Modules → Func`, and `seqBound` lives in `Processes` — import
it here is fine, no cycle: `Processes` never imports `Modules`). -/
def modBound (m : CModule) : ResourceBound :=
  m.funcs.foldl (fun acc f =>
    { timeRep := fun n => acc.timeRep n + f.bodyBound.timeRep n,
      memRep := fun n => Nat.max (acc.memRep n) (f.bodyBound.memRep n) })
    { timeRep := gZero, memRep := gZero }

end LeanC
