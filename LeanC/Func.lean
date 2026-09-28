import LeanC.Context
import LeanC.Expr
import LeanC.CostSpec

/-!
# Functions — signature stub (T5) + N1/N2 parametric + measured bodies

`call ↔ Func` cycle is broken by name (`CExpr.call` takes `fname`,
never a `Func` body; `Expr` never imports `Func`). Import direction
`Program → Func → Expr` preserved (`Func` may import `Expr`).

Costs are parametric (`Nat → Nat`, N1): `declaredTime/Mem` are what
callers assume in `CExpr.call` (as functions of input size `n`);
`bodyTime/Mem` are the measured body costs (as functions); `Program`
discharges the name to this spec.
-/

namespace LeanC

/-- WHAT a function is: a proof of its spec (Fix 1 + program-as-proof,
N1 parametric + N2 measured).

A `CFunc` is not just a name — it carries its declared envelope
(`declaredTime/Mem : Nat → Nat`, what callers assume in `CExpr.call`)
together with its measured body cost (`bodyTime/Mem : Nat → Nat`) and
the machine-checked proof `bodyLeDeclared` that the body fits the
declaration pointwise (`∀ n`). `call ↔ Func` cycle stays broken by name
(`CExpr.call` takes `fname`, never a `Func` body; `Expr` never imports
`Func`); `Program.callResolves` discharges the name to this spec.
`bodyLeDeclared` has NO default (a proof cannot be defaulted for
arbitrary bounds) — every `CFunc` literal provides it (usually
`⟨fun _ => Nat.le_refl _, fun _ => Nat.le_refl _⟩` for exact specs or
`⟨fun _ => by omega, fun _ => by omega⟩` for loose ones; leaf/external
`fun _ => K` specs use `Nat.le_refl`).

N2 tie (not free `Nat`s): non-external funcs MUST be built via
`mkFuncWithBody` below, which sets `bodyTime/Mem := exprTimeBound /
exprMemBound body` by construction (measured, not trusted) and takes
the `body ≤ declared` proof as the only obligation. Direct `CFunc`
literals remain for leaf/external specs (e.g. `puts`); `Program` still
checks them via `callResolves`. -/
structure CFunc where
  (fname : String)
  (declaredTime : Nat → Nat := fun _ => 0)
  (declaredMem : Nat → Nat := fun _ => 0)
  (bodyTime : Nat → Nat := fun _ => 0)
  (bodyMem : Nat → Nat := fun _ => 0)
  (bodyLeDeclared :
    (∀ n, bodyTime n ≤ declaredTime n) ∧ (∀ n, bodyMem n ≤ declaredMem n))
  (isFuncSound : Prop := True)

/-- N2 measured constructor: `bodyTime/Mem` by computation from `body`
(`exprTimeBound`/`exprMemBound`), not by trust. Only `declaredTime/Mem`
+ the `body ≤ declared` proof remain as obligations (discharged by
`Program.allBodiesFit`). -/
def mkFuncWithBody {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (fname : String) (body : CExpr Γ α)
    (declaredTime declaredMem : Nat → Nat)
    (hle : (∀ n, exprTimeBound body n ≤ declaredTime n) ∧
           (∀ n, exprMemBound body n ≤ declaredMem n)) : CFunc :=
  { fname := fname, declaredTime := declaredTime, declaredMem := declaredMem,
    bodyTime := exprTimeBound body, bodyMem := exprMemBound body,
    bodyLeDeclared := hle }

/-- A `call` site's stored spec functions match the registry entry (what
`Program` must discharge for every `CExpr.call`, N1 parametric:
equality of `Nat → Nat` specs, `rfl` for same `fun _ => K` literal). -/
def funcMatchesCall (f : CFunc) (fname : String)
    (dt dm : Nat → Nat) : Prop :=
  f.fname = fname ∧ f.declaredTime = dt ∧ f.declaredMem = dm

theorem funcMatchesCall_refl (f : CFunc) :
    funcMatchesCall f f.fname f.declaredTime f.declaredMem :=
  ⟨rfl, rfl, rfl⟩

/-- C sketch: `void fname(void) { body }` (bodies are statement
sketches from `Stmt`, passed in as pre-rendered text for the draft). -/
def emitFunc (f : CFunc) (bodyStr : String) : String :=
  "void " ++ f.fname ++ "(void) { " ++ bodyStr ++ " }"

end LeanC
