import LeanC.Context
import LeanC.Complexity
import LeanC.Modules

/-!
# Programs — top level: modules + `main` + worst bound (T5 stub)
-/

namespace LeanC

/-- WHAT a program is (draft): modules + designated `main` + pool +
precomputed `worst_bound` (fold of module bounds; computed by
`progBound`, stored for the C pipeline to consume). -/
structure CProgram where
  (mods : List CModule)
  (main : String)
  (pool : List LiteralPoolEntry)
  (worst_bound : ResourceBound)
  (isProgSound : Prop := True)

/-- Fold module bounds by sequencing; empty program costs zero. -/
def progBound (mods : List CModule) : ResourceBound :=
  mods.foldl (fun acc m =>
    let mb := modBound m
    { timeRep := fun n => acc.timeRep n + mb.timeRep n,
      memRep := fun n => Nat.max (acc.memRep n) (mb.memRep n) })
    { timeRep := gZero, memRep := gZero }

end LeanC
