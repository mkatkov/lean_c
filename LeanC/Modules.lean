import LeanC.Context
import LeanC.Func

/-!
# Modules — translation units: funcs + literal pool (T5 stub)
-/

namespace LeanC

/-- WHAT a module is: a translation unit — function list plus the
live global-statics pool (`LiteralPoolEntry`, see `Context`) plus its
worst-case envelope (folded `max` over member funcs, so the module is
a proof that every member fits the envelope).
P2 E4: vacuous True soundness field deleted; only real
`moduleAllBodiesFit` remains. -/
structure CModule where
  (funcs : List CFunc)
  (pool : List LiteralPoolEntry)

/-- Worst declared time over member funcs (module envelope, N1
parametric: pointwise `max` at input size `n`). -/
def modWorstTime (m : CModule) : Nat → Nat :=
  fun n => m.funcs.foldl (fun acc f => Nat.max acc (f.declaredTime n)) 0

/-- Worst declared mem over member funcs (N1 parametric). -/
def modWorstMem (m : CModule) : Nat → Nat :=
  fun n => m.funcs.foldl (fun acc f => Nat.max acc (f.declaredMem n)) 0

/-- Every member's body fits its declaration (lifts `CFunc.bodyLeDeclared`,
N1 parametric pointwise). -/
def moduleAllBodiesFit (m : CModule) : Prop :=
  ∀ f ∈ m.funcs, (∀ n, f.bodyTime n ≤ f.declaredTime n) ∧
    (∀ n, f.bodyMem n ≤ f.declaredMem n)

theorem moduleAllBodiesFit_of (m : CModule)
    (h : ∀ f ∈ m.funcs, (∀ n, f.bodyTime n ≤ f.declaredTime n) ∧
      (∀ n, f.bodyMem n ≤ f.declaredMem n)) :
    moduleAllBodiesFit m := h

/-- Lookup by name (call resolution within one module). -/
def moduleFindFunc (m : CModule) (fname : String) : Option CFunc :=
  m.funcs.find? (fun f => f.fname == fname)

end LeanC
