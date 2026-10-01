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
universe v

/-- P1-A3: call site lives in `Func` (D10), not `Program`.
`Expr` never imports `Func` (D8); `Program` reaches `CallSite` via
`Modules → Func`. One `CExpr.call` occurrence's stored spec — `fname` +
`declaredTime/Mem : Nat → Nat`. -/
structure CallSite where
  (fname : String)
  (declaredTime : Nat → Nat)
  (declaredMem : Nat → Nat)

/-- P1-A1 traversal collectors (home: `Func.lean`, agent's choice —
`Calls.lean` rejected to keep import closure small; never in `Expr.lean`
which would need `CallSite`, violating D8 if it stayed in `Program`).
Rules mirror `exprTimeFn`'s `match`-recursion, not `induction … with`
(GADT motives defeat the tactic, cf. `CostSpec`).

`exprCalls`: `.call fname … dt dm → [⟨fname, dt, dm⟩]`; all other ctors
recurse and append. `exprNested`: flatMap `rawCallFnames` over call args
where available — `CExpr.call` stores only erased `argStrs`
(`List String`), so the `.call` case is `[]` by construction; nested
`callRaw` in `mkCallWithRaw` args is forbidden by discipline
(`rawArgsFirstOrder`, see P1-A4 first-order-only choice in `Expr.lean`
+ F13). All other ctors recurse and append. -/
def exprCalls {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → List CallSite
  | .lit _ _ => []
  | .var _ => []
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => exprCalls e
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => exprCalls l ++ exprCalls r
  | @CExpr.cast _ _ _ _ _ _ e _ => exprCalls e
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => exprCalls e
  | @CExpr.addr _ _ _ _ _ _ _ _ => []
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => exprCalls base
  | @CExpr.field _ _ _ _ _ _ base _ _ => exprCalls base
  | .tern g t e => exprCalls g ++ exprCalls t ++ exprCalls e
  | .call fname _ _ _ dt dm => [{ fname := fname, declaredTime := dt, declaredMem := dm }]

/-- Nested name-obligations (D12 existence-only): `RawExpr.callRaw` carries
no specs, so inner fnames resolve by name-existence
(`ProgramNestedResolve`). Under the first-order-only restriction
(P1-A4) the `.call` case is `[]` (erased `argStrs` carry no hidden calls);
other ctors recurse. -/
def exprNested {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → List String
  | .lit _ _ => []
  | .var _ => []
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => exprNested e
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => exprNested l ++ exprNested r
  | @CExpr.cast _ _ _ _ _ _ e _ => exprNested e
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => exprNested e
  | @CExpr.addr _ _ _ _ _ _ _ _ => []
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => exprNested base
  | @CExpr.field _ _ _ _ _ _ base _ _ => exprNested base
  | .tern g t e => exprNested g ++ exprNested t ++ exprNested e
  | .call _ _ _ _ _ _ => []

/-- WHAT a function is: a proof of its spec (Fix 1 + program-as-proof,
N1 parametric + N2 measured + P1-A2 traversal-closed + P2 enforcing).

P2 E1: `CFunc` is an inductive with 2 constructors only. `calls`/`nested`/
`bodyTime`/`bodyMem`/`bodySrc`/`fname`/`declared` are `def`s by matching,
never structure fields with defaults a producer can forge. Forged
empty-call list is unrepresentable (no field to set). Only
`mkFuncWithBody` / `mkLeafFunc` are reachable (no structure-literal
syntax remains).

A `CFunc` carries its declared envelope (`declaredTime/Mem : Nat → Nat`,
what callers assume in `CExpr.call`) together with either a measured body
(`withBody`: body + `body ≤ declared` proof) or an external spec (`leaf`).
`call ↔ Func` cycle stays broken by name (`CExpr.call` takes `fname`,
never a `Func` body; `Expr` never imports `Func`); `Program.callResolves`
discharges the name to this spec. -/
inductive CFunc where
| withBody {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (fname : String) (body : CExpr Γ α)
    (declaredTime declaredMem : Nat → Nat)
    (hle : (∀ n, exprTimeBound body n ≤ declaredTime n) ∧
           (∀ n, exprMemBound body n ≤ declaredMem n)) : CFunc
| leaf (fname : String) (declaredTime declaredMem : Nat → Nat) : CFunc

/-- Computed projections (all `def`, no fields). -/
def CFunc.fname : CFunc → String
| @CFunc.withBody _ _ _ _ f _ _ _ _ => f
| .leaf f _ _ => f

def CFunc.declaredTime : CFunc → Nat → Nat
| @CFunc.withBody _ _ _ _ _ _ dt _ _ => dt
| .leaf _ dt _ => dt

def CFunc.declaredMem : CFunc → Nat → Nat
| @CFunc.withBody _ _ _ _ _ _ _ dm _ => dm
| .leaf _ _ dm => dm

def CFunc.bodyTime : CFunc → Nat → Nat
| @CFunc.withBody _ _ _ _ _ body _ _ _ => exprTimeBound body
| .leaf _ dt _ => dt

def CFunc.bodyMem : CFunc → Nat → Nat
| @CFunc.withBody _ _ _ _ _ body _ _ _ => exprMemBound body
| .leaf _ _ dm => dm

def CFunc.calls : CFunc → List CallSite
| @CFunc.withBody _ _ _ _ _ body _ _ _ => exprCalls body
| .leaf _ _ _ => []

def CFunc.nested : CFunc → List String
| @CFunc.withBody _ _ _ _ _ body _ _ _ => exprNested body
| .leaf _ _ _ => []

def CFunc.bodySrc : CFunc → String
| @CFunc.withBody _ _ _ _ _ body _ _ _ => emitExpr body ++ ";"
| .leaf _ _ _ => ""

def CFunc.bodyLeDeclared (f : CFunc) :
    (∀ n, f.bodyTime n ≤ f.declaredTime n) ∧
    (∀ n, f.bodyMem n ≤ f.declaredMem n) :=
  match f with
  | @CFunc.withBody _ _ _ _ _ body _ _ hle => hle
  | .leaf _ _ _ => ⟨fun _ => Nat.le_refl _, fun _ => Nat.le_refl _⟩

/-- N2 measured constructor (+ P1-A2 computed calls, P2 E1 thin wrapper):
`bodyTime/Mem` by computation from `body` (`exprTimeBound`/`exprMemBound`),
`calls`/`nested` by traversal (`exprCalls`/`exprNested`), `bodySrc` by
emission (`emitExpr body ++ ";"`), not by trust. Only `declaredTime/Mem` +
the `body ≤ declared` proof remain as obligations (discharged by
`Program.allBodiesFit`). -/
def mkFuncWithBody {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (fname : String) (body : CExpr Γ α)
    (declaredTime declaredMem : Nat → Nat)
    (hle : (∀ n, exprTimeBound body n ≤ declaredTime n) ∧
           (∀ n, exprMemBound body n ≤ declaredMem n)) : CFunc :=
  .withBody fname body declaredTime declaredMem hle

/-- P1-A2 leaf constructor for external specs (`puts`): empty calls/nested,
body equals declared (exact), empty body source (external — no
definition emitted). Migrate all direct leaf literals (`putsSpec`,
`add2Func`, `putsFunc`) to this. -/
def mkLeafFunc (fname : String) (dt dm : Nat → Nat) : CFunc :=
  .leaf fname dt dm

/-- A `call` site's stored spec functions match the registry entry (what
`Program` must discharge for every `CExpr.call`, N1 parametric:
equality of `Nat → Nat` specs, `rfl` for same `fun _ => K` literal). -/
def funcMatchesCall (f : CFunc) (fname : String)
    (dt dm : Nat → Nat) : Prop :=
  f.fname = fname ∧ f.declaredTime = dt ∧ f.declaredMem = dm

theorem funcMatchesCall_refl (f : CFunc) :
    funcMatchesCall f f.fname f.declaredTime f.declaredMem :=
  ⟨rfl, rfl, rfl⟩

end LeanC
