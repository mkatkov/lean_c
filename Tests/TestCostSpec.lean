import LeanC.CostSpec
import LeanC.Loop
import LeanC.Context
import LeanC.Literals
import LeanC.Variables
import LeanC.Ops
import LeanC.Expr
import LeanC.Func
import LeanC.Program
import Examples.ComplexityLinear

open Lean IO

/-!
# Tests for unified cost specs (CostSpec + syntax measurement + budget)

Compile-time `example`s lock the unification (exact + `O1` in one
value, statements preserve `O1`); the `IO` runner replays the closed
bounds budgets actually check (`add2` time `1` / mem `0`, `"hello"`
mem `5`, tiny-device accept/reject).
-/

namespace TestCostSpec

open LeanC

/-- `1 + 2` over signed I32 (same shape as the stdlib pilot). -/
def add2 : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.binop CAddOp.add
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 1 rfl) rfl)
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 2 rfl) rfl)

/-- `"hello"` literal (5 cells). -/
def hello : CLiteral (CGlobalStaticMemoryBlock (CCharType Unit) 5) :=
  CLiteral.strLit "hello" rfl

/-- ATtiny-class envelope: 2KB SRAM, 100k cycles, 512 stack, 32KB flash. -/
def attiny : DeviceSpec :=
  { flash := 32768, sram := 2048, stack := 512, maxCycles := 100000 }

/-- Zero-cycle device: rejects anything non-empty (negative control). -/
def noCycles : DeviceSpec :=
  { flash := 32768, sram := 2048, stack := 512, maxCycles := 0 }

/-! ## Compile-time: exact + asymptotic in one value -/

example : costInClass (axis := .time) StepCost TimeComplexity_O1
    (litTimeCost (.intLit .I32 true 42 rfl)) :=
  litTime_in_o1 _

example : costInClass (axis := .memory) CellCost MemoryComplexity_O1
    (litMemCost hello) :=
  litMem_in_o1 _

example : costInClass (axis := .time) StepCost TimeComplexity_O1
    (exprTimeCost add2) :=
  exprTime_in_o1 _ ⟨trivial, trivial⟩

example : costInClass (axis := .memory) CellCost MemoryComplexity_O1
    (exprMemCost add2) :=
  exprMem_in_o1 _ ⟨trivial, trivial⟩

example : costInClass (axis := .time) StepCost TimeComplexity_O1
      (assignTime (exprTimeCost add2)) ∧
    costInClass (axis := .memory) CellCost MemoryComplexity_O1
      (assignMem (exprMemCost add2)) :=
  assignExpr_in_o1 _ ⟨trivial, trivial⟩ ⟨trivial, trivial⟩

/-- Parametric bounds behind the `O1` claims (what budgets evaluate at
`n`; N1 `Nat → Nat`). -/
example (n : Nat) : exprTimeBound add2 n = 1 := rfl
example (n : Nat) : exprMemBound add2 n = 0 := rfl
example : litMemBound hello = 5 := rfl
example : litTimeBound hello = 1 := rfl

/-- Budget soundness: accept gives all four inequalities (Fix 2:
`flashCells`/`stackCells` explicit — `add2` uses `0`/`0`). -/
example : fitsExprAt attiny add2 0 0 0 = true := rfl
example : fitsExprAt noCycles add2 0 0 0 = false := rfl

example (h : fitsExprAt attiny add2 0 0 0 = true) :
    exprTimeFn add2 0 ≤ attiny.maxCycles ∧ exprMemFn add2 0 ≤ attiny.sram ∧
      (0 : Nat) ≤ attiny.flash ∧ (0 : Nat) ≤ attiny.stack :=
  fitsExprAt_true h

/-! ## N1 parametric call + N3 mem fix: `call` threads `n`, checked via `RawExpr` -/

/-- `puts("hi")` via checked path: arg sums by computation (N1
`declared : Nat → Nat`, leaf `fun _ => K`). -/
def putsCall : CExpr DraftCtx (CIntType .I32 true) :=
  mkCallWithRaw "puts" [.strLit "hi"] (by simp [rawArgsFirstOrder, rawCallFnames]) (fun _ => 10) (fun _ => 0)

example (n : Nat) : exprTimeBound putsCall n = rawArgsTime [.strLit "hi"] + 10 + 1 := rfl
example (n : Nat) : exprMemBound putsCall n = Nat.max (rawArgsMem [.strLit "hi"]) 0 := rfl
example : rawTimeBound (.strLit "hi") = 1 := by simp [rawTimeBound]
example : rawTimeBound (.intLit 5) = 0 := by simp [rawTimeBound]
example : rawMemBound (.intLit 5) = 0 := by simp [rawMemBound]
example : rawArgsTime [.intLit 5] = 0 := by simp [rawArgsTime, rawTimeBound]
example : rawArgsMem [.intLit 5] = 0 := by simp [rawArgsMem, rawMemBound]
-- `strLit` mem is `s.length` (kernel-opaque `String.length`, so no
-- closed numeric `example` — runtime `test` below evaluates it to `2`
-- via compiled code; see `putsCall` mem `2` check). -/

/-- N1 gating for `putsCall`: arg fns const `O1` + declared const `O1`. -/
def putsCallO1Time : exprCallsO1Time putsCall :=
  ⟨rawArgsTimeFn_in_o1 _, BigO.const_le_one _ (fun _ => Nat.le_refl _)⟩

def putsCallO1Mem : exprCallsO1Mem putsCall :=
  ⟨rawArgsMemFn_in_o1 _, BigO.const_le_one _ (fun _ => Nat.le_refl _)⟩

example : costInClass (axis := .time) StepCost TimeComplexity_O1
    (exprTimeCost putsCall) :=
  exprTime_in_o1 _ putsCallO1Time

example : costInClass (axis := .memory) CellCost MemoryComplexity_O1
    (exprMemCost putsCall) :=
  exprMem_in_o1 _ putsCallO1Mem

/-- `puts` registry entry matching the call above (N1 `fun _ => K`,
discharges N1 gating + N2 `callResolves`). P1-A2: leaf via `mkLeafFunc`
(empty calls/nested, empty body source, external). -/
def putsSpec : CFunc :=
  mkLeafFunc "puts" (fun _ => 10) (fun _ => 0)

/-- P1-A3 caller whose body calls `puts` (empty args for closed numeric
proofs): `exprTimeBound = 0 + 10 + 1 = 11`, `exprMemBound = 0`.
`mkFuncWithBody` computes calls by traversal. -/
def putsCallerBody : CExpr DraftCtx (CIntType .I32 true) :=
  mkCallWithRaw "puts" [] rfl (fun _ => 10) (fun _ => 0)

def putsCaller : CFunc :=
  mkFuncWithBody "puts_caller" putsCallerBody (fun _ => 11) (fun _ => 0)
    ⟨fun _ => Nat.le_refl _, fun _ => Nat.le_refl _⟩

def progPuts : CProgram :=
  { mods := [{ funcs := [putsSpec, putsCaller], pool := [] }],
    main := "puts", pool := [] }

example : callResolves progPuts "puts" (fun _ => 10) (fun _ => 0) :=
  ⟨putsSpec, by simp [programFuncs, progPuts], rfl, rfl, rfl⟩

example : programCallsComputed progPuts =
    [⟨"puts", (fun _ => 10), (fun _ => 0)⟩] := by
  simp [programCallsComputed, programFuncs, progPuts, putsSpec, putsCaller,
    putsCallerBody, mkCallWithRaw, mkFuncWithBody, mkLeafFunc, exprCalls,
    CFunc.calls, CFunc.fname, CFunc.declaredTime, CFunc.declaredMem]

example : programNestedComputed progPuts = [] := by
  simp [programNestedComputed, programFuncs, progPuts, putsSpec, putsCaller,
    putsCallerBody, mkCallWithRaw, mkFuncWithBody, mkLeafFunc, exprNested,
    CFunc.nested, CFunc.fname]

example : ProgramCallsResolve progPuts := by
  intro s hs
  have hlist : programCallsComputed progPuts =
      [⟨"puts", (fun _ => 10), (fun _ => 0)⟩] := by
    simp [programCallsComputed, programFuncs, progPuts, putsSpec, putsCaller,
      putsCallerBody, mkCallWithRaw, mkFuncWithBody, mkLeafFunc, exprCalls,
      CFunc.calls, CFunc.fname, CFunc.declaredTime, CFunc.declaredMem]
  rw [hlist] at hs
  cases hs with
  | head _ => exact ⟨putsSpec, by simp [programFuncs, progPuts], rfl, rfl, rfl⟩
  | tail _ h => cases h

example : ProgramNestedResolve progPuts := by
  intro fname hf
  have hempty : programNestedComputed progPuts = [] := by
    simp [programNestedComputed, programFuncs, progPuts, putsSpec, putsCaller,
      putsCallerBody, mkCallWithRaw, mkFuncWithBody, mkLeafFunc, exprNested,
      CFunc.nested, CFunc.fname]
  rw [hempty] at hf
  cases hf

example (n : Nat) : programWorstTime progPuts n = 11 := by
  simp [programWorstTime, programFuncs, progPuts, putsSpec, putsCaller,
    mkLeafFunc, mkFuncWithBody, CFunc.declaredTime, CFunc.declaredMem, CFunc.fname]
example : programPoolCells progPuts = 0 := rfl
example : programFitsDevice progPuts attiny 0 0 = true := rfl

example : ProgramMeetsSpec progPuts :=
  mkProgramMeetsSpec progPuts
    ⟨putsSpec, by simp [programFuncs, progPuts], rfl⟩
    (by intro f hf
        simp only [programFuncs, progPuts] at hf
        cases hf with
        | head _ =>
          -- putsSpec is leaf: body = declared by construction
          exact putsSpec.bodyLeDeclared
        | tail _ h =>
          cases h with
          | head _ =>
            exact putsCaller.bodyLeDeclared
          | tail _ h => cases h)
    (by intro s hs
        have hlist : programCallsComputed progPuts =
            [⟨"puts", (fun _ => 10), (fun _ => 0)⟩] := by
          simp [programCallsComputed, programFuncs, progPuts, putsSpec, putsCaller,
            putsCallerBody, mkCallWithRaw, mkFuncWithBody, mkLeafFunc, exprCalls,
            CFunc.calls, CFunc.fname, CFunc.declaredTime, CFunc.declaredMem]
        rw [hlist] at hs
        cases hs with
        | head _ => exact ⟨putsSpec, by simp [programFuncs, progPuts], rfl, rfl, rfl⟩
        | tail _ h => cases h)
    (by intro fname hf
        have hempty : programNestedComputed progPuts = [] := by
          simp [programNestedComputed, programFuncs, progPuts, putsSpec, putsCaller,
            putsCallerBody, mkCallWithRaw, mkFuncWithBody, mkLeafFunc, exprNested,
            CFunc.nested, CFunc.fname]
        rw [hempty] at hf
        cases hf)

/-! ## Fix 2: pool + width + full budget -/

example : poolCells [] = 0 := rfl
example : tableMemCells 1 4 = 4 := rfl
example : tableMemCells 4 4 = 16 := rfl

/-- `hello` pool (5 cells) fits `attiny.flash`, rejects tiny flash. -/
def tinyFlash : DeviceSpec :=
  { flash := 4, sram := 2048, stack := 512, maxCycles := 100000 }

example : poolCells [{ ty := CGlobalStaticMemoryBlock (CCharType Unit) 5, cells := 5 }] = 5 := rfl
example : fitsBudget attiny 1 0 5 0 = true := rfl
example : fitsBudget tinyFlash 1 0 5 0 = false := rfl

example (h : fitsBudget attiny 1 0 5 0 = true) :
    (1 : Nat) ≤ attiny.maxCycles ∧ (0 : Nat) ≤ attiny.sram ∧
      (5 : Nat) ≤ attiny.flash ∧ (0 : Nat) ≤ attiny.stack :=
  fitsBudget_true h

/-! ## N1: parametric call varies with `n` (fails before N1) -/

/-- Linear callee spec (`fun n => n`, not `O1`): caller threads `n`. -/
def linearCall : CExpr DraftCtx (CIntType .I32 true) :=
  mkCallWithRaw "f" [] rfl (fun n => n) (fun _ => 0)

example : exprTimeFn linearCall 5 = 0 + 5 + 1 := rfl
example : exprTimeFn linearCall 10 = 0 + 10 + 1 := rfl
example : exprTimeFn linearCall 5 ≠ exprTimeFn linearCall 10 := by decide

/-- Leaf `puts` stays `fun _ => K` (N1 migration). -/
example (n : Nat) : exprTimeFn putsCall n = 1 + 10 + 1 := by
  simp [putsCall, mkCallWithRaw, exprTimeFn, rawArgsTimeFn, rawArgsTime, rawTimeBound]

/-! ## N3: nested-call mem = max, not `0` -/

/-- N3 regression: `rawMemBound (callRaw f [strLit …])` keeps inner
high-water (equals inner `strLit` mem), not `0` (before N3 it was `0`). -/
example : rawMemBound (.callRaw "f" [.strLit "hi"]) =
    rawMemBound (.strLit "hi") := by
  simp [rawMemBound]

example : rawMemBound (.callRaw "f" [.strLit "hi"]) = ("hi".length) := by
  simp [rawMemBound, Nat.max_eq_right, Nat.zero_le]

/-- Nested `puts(f(x))` via `RawExpr`: inner high-water kept. -/
def nestedRaw : RawExpr := .callRaw "puts" [.callRaw "f" [.strLit "hi"]]

example : rawMemBound nestedRaw = ("hi".length) := by
  simp [nestedRaw, rawMemBound, Nat.max_eq_right, Nat.zero_le]

example : rawArgsMem [.callRaw "f" [.strLit "hi"]] = ("hi".length) := by
  simp [rawArgsMem, rawMemBound, Nat.max_eq_right, Nat.zero_le]

/-! ## P1-A3/A4: bogus-in-body — omission impossible + first-order nested rule -/

/-- Bogus body: `call "bogus"` built IN A BODY via `mkCallWithRaw`
(empty args for closed proofs), not via a listed site. Registry
(`putsSpec` + this caller) lacks `"bogus"`. There is no list to omit
from — `programCallsComputed` traverses stored bodies. -/
def bogusBody : CExpr DraftCtx (CIntType .I32 true) :=
  mkCallWithRaw "bogus" [] rfl (fun _ => 1) (fun _ => 0)

def bogusCaller : CFunc :=
  mkFuncWithBody "bogus_caller" bogusBody (fun _ => 2) (fun _ => 0)
    ⟨fun _ => Nat.le_refl _, fun _ => Nat.le_refl _⟩

/-- Bogus program: registry has `puts` + caller, but caller traverses to
`bogus` which is unregistered.
P2 E1: no open-constructor forgery remains — there is no structure-literal
syntax for `CFunc`/`CModule`/`CProgram` outside smart constructors
(`mkFuncWithBody`/`mkLeafFunc`); omission trick impossible (checked by rg,
acceptance §6.2). -/
def progBogus : CProgram :=
  { mods := [{ funcs := [putsSpec, bogusCaller], pool := [] }],
    main := "puts", pool := [] }

example : programCallsComputed progBogus =
    [⟨"bogus", (fun _ => 1), (fun _ => 0)⟩] := by
  simp [programCallsComputed, programFuncs, progBogus, putsSpec, bogusCaller,
    bogusBody, mkCallWithRaw, mkFuncWithBody, mkLeafFunc, exprCalls,
    CFunc.calls, CFunc.fname, CFunc.declaredTime, CFunc.declaredMem]

example : ¬ ProgramCallsResolve progBogus := by
  intro h
  have hlist : programCallsComputed progBogus =
      [⟨"bogus", (fun _ => 1), (fun _ => 0)⟩] := by
    simp [programCallsComputed, programFuncs, progBogus, putsSpec, bogusCaller,
      bogusBody, mkCallWithRaw, mkFuncWithBody, mkLeafFunc, exprCalls,
      CFunc.calls, CFunc.fname, CFunc.declaredTime, CFunc.declaredMem]
  have hmem : (⟨"bogus", (fun _ => 1), (fun _ => 0)⟩ : CallSite) ∈ programCallsComputed progBogus := by
    rw [hlist]
    exact List.Mem.head _
  have hsite := h _ hmem
  simp only [callResolves, programFuncs, progBogus] at hsite
  obtain ⟨f, hf, hfname, _, _⟩ := hsite
  cases hf with
  | head _ =>
    simp [putsSpec, mkLeafFunc, CFunc.fname, CFunc.declaredTime, CFunc.declaredMem] at hfname
  | tail _ h =>
    cases h with
    | head _ =>
      simp [bogusCaller, mkFuncWithBody, bogusBody, mkCallWithRaw, CFunc.fname] at hfname
    | tail _ h => cases h

example : ¬ ProgramMeetsSpec progBogus := by
  intro h
  have hc := h.callsResolve
  have hlist : programCallsComputed progBogus =
      [⟨"bogus", (fun _ => 1), (fun _ => 0)⟩] := by
    simp [programCallsComputed, programFuncs, progBogus, putsSpec, bogusCaller,
      bogusBody, mkCallWithRaw, mkFuncWithBody, mkLeafFunc, exprCalls,
      CFunc.calls, CFunc.fname, CFunc.declaredTime, CFunc.declaredMem]
  have hmem : (⟨"bogus", (fun _ => 1), (fun _ => 0)⟩ : CallSite) ∈ programCallsComputed progBogus := by
    rw [hlist]
    exact List.Mem.head _
  have hsite := hc _ hmem
  simp only [callResolves, programFuncs, progBogus] at hsite
  obtain ⟨f, hf, hfname, _, _⟩ := hsite
  cases hf with
  | head _ =>
    simp [putsSpec, mkLeafFunc, CFunc.fname, CFunc.declaredTime, CFunc.declaredMem] at hfname
  | tail _ h =>
    cases h with
    | head _ =>
      simp [bogusCaller, mkFuncWithBody, bogusBody, mkCallWithRaw, CFunc.fname] at hfname
    | tail _ h => cases h

/-- P1-A4 nested rule (first-order-only choice, recorded) + P2 E2 gate:
`nestedRaw` stays a `rawMemBound`-only example, never lifted to `CExpr`.
`rawCallFnames nestedRaw = ["puts", "f"]` (outer + inner), but
`exprNested` of any `mkCallWithRaw`-built `CExpr` is `[]` (erased
`argStrs`); lifting nested args would require `ProgramNestedResolve`
which fails without `"f"` registered. P2 E2: lifting this arg list is
ill-typed by the gate (`mkCallWithRaw` requires `rawArgsFirstOrder`);
the `¬ rawArgsFirstOrder […]` below is the proof that lifting it is
ill-typed. -/
example : rawCallFnames nestedRaw = ["puts", "f"] := by
  simp [nestedRaw, rawCallFnames]

example : rawIsFirstOrder (.strLit "hi") := by
  simp [rawIsFirstOrder, rawCallFnames]

example : ¬ rawArgsFirstOrder [.callRaw "f" [.strLit "hi"]] := by
  simp [rawArgsFirstOrder, rawCallFnames]

example : exprNested putsCallerBody = [] := by
  simp [putsCallerBody, mkCallWithRaw, exprNested]

example : exprNested bogusBody = [] := by
  simp [bogusBody, mkCallWithRaw, exprNested]

/-- N2 measured body: `add2` func built via `mkFuncWithBody` (not free `Nat`s). -/
def add2Measured : CFunc :=
  mkFuncWithBody "add2" add2 (fun _ => 1) (fun _ => 0)
    ⟨fun _ => Nat.le_refl _, fun _ => Nat.le_refl _⟩

example : add2Measured.bodyTime = exprTimeBound add2 := rfl
example : add2Measured.bodyMem = exprMemBound add2 := rfl

/-! ## N4: bounded loop — first `linear` inhabitant + `0`-iter `O1` -/

/-- `0` iters is `Zero`-exact/`O1` (N4 `O1` case). -/
example : costInZero StepCost (forNTime 0 (constStepBody 1)) :=
  forNTime_zero_in_zero _
example : costInClass (axis := .time) StepCost TimeComplexity_O1
    (forNTime 0 (constStepBody 1)) :=
  forNTime_zero_in_o1 _

/-- Core linear inhabitant: `n` iters of uniform-`O1` body is `poly 1`. -/
example : costInClass (axis := .time) StepCost (TimeComplexity_poly 1)
    (forNDiagTime (constStepBody 1)) :=
  forNDiag_const_linear 1

/-- `linear` (`Examples`) inhabitant via same uniform bound (`glinear`). -/
theorem forNDiag_linear_glinear :
    costInClass (axis := .time) StepCost TimeComplexity_linear
      (forNDiagTime (constStepBody 1)) := by
  show BigO (HasCost.cost (forNDiagTime (constStepBody 1))) glinear
  refine ⟨1, 0, fun n _ => ?_⟩
  show forNTimeAux n (constStepBody 1) n ≤ 1 * Nat.max (glinear n) 1
  have hsum : forNTimeAux n (constStepBody 1) n ≤ n * 1 :=
    forNTimeAux_le (constStepBody_uniform 1) n n
  have h1 : n * 1 = n := Nat.mul_one n
  rw [h1] at hsum
  show forNTimeAux n (constStepBody 1) n ≤ 1 * Nat.max (glinear n) 1
  simp only [glinear, Nat.one_mul]
  exact Nat.le_trans hsum (Nat.le_max_left _ _)

example : costInClass (axis := .time) StepCost TimeComplexity_linear
    (forNDiagTime (constStepBody 1)) :=
  forNDiag_linear_glinear

/-- Diagonal mem stays `O1`. -/
example : costInClass (axis := .memory) CellCost MemoryComplexity_O1
    (forNDiagMem countdownMemBody) :=
  forNDiagMem_in_o1 countdownMem_uniform

/-! ## N5: unbounded marker compiles, cites `no_divergent_cost` -/

example (c : Nat → Nat) : ¬ IsDivergentCost c := whileTrue_no_divergent c
example : ComplexityLE TimeComplexity_UNBOUND TimeComplexity_UNDECIDABLE :=
  complexityLE_unbound_undecidable

def assertEq (expected actual : String) : IO Bool :=
  if expected == actual then
    IO.println s!"OK: {actual}" *> pure true
  else
    IO.eprintln s!"FAIL: expected {expected} but got {actual}" *> pure false

def test : IO UInt32 := do
  let mut ok := true
  -- parametric exact bounds at input size 0 (not asymptotics; N1 `Nat → Nat`)
  ok := (← assertEq "1" (toString (exprTimeBound add2 0))) && ok
  ok := (← assertEq "0" (toString (exprMemBound add2 0))) && ok
  ok := (← assertEq "5" (toString (litMemBound hello))) && ok
  ok := (← assertEq "1" (toString (litTimeBound hello))) && ok
  -- tiny-device accept / reject at input size 0 (Fix 2: flash/stack explicit)
  ok := (← assertEq "true" (toString (fitsExprAt attiny add2 0 0 0))) && ok
  ok := (← assertEq "false" (toString (fitsExprAt noCycles add2 0 0 0))) && ok
  ok := (← assertEq "true" (toString (fitsBudget attiny 1 0 0 0))) && ok
  ok := (← assertEq "false" (toString (fitsBudget noCycles 1 0 0 0))) && ok
  -- N1 checked call bounds at 0 (1 + 10 + 1 = 12 time, max 2 0 = 2 mem)
  ok := (← assertEq "12" (toString (exprTimeBound putsCall 0))) && ok
  ok := (← assertEq "2" (toString (exprMemBound putsCall 0))) && ok
  ok := (← assertEq "11" (toString (programWorstTime progPuts 0))) && ok
  ok := (← assertEq "true" (toString (programFitsDevice progPuts attiny 0 0))) && ok
  -- Fix 2: pool + width + full budget
  ok := (← assertEq "5" (toString (poolCells [{ ty := CGlobalStaticMemoryBlock (CCharType Unit) 5, cells := 5 }]))) && ok
  ok := (← assertEq "16" (toString (tableMemCells 4 4))) && ok
  ok := (← assertEq "true" (toString (fitsBudget attiny 1 0 5 0))) && ok
  ok := (← assertEq "false" (toString (fitsBudget tinyFlash 1 0 5 0))) && ok
  -- N1: parametric call varies with n (6 vs 11)
  ok := (← assertEq "6" (toString (exprTimeFn linearCall 5))) && ok
  ok := (← assertEq "11" (toString (exprTimeFn linearCall 10))) && ok
  -- N3: nested-call mem = max (2), not 0
  ok := (← assertEq "2" (toString (rawMemBound (.callRaw "f" [.strLit "hi"])))) && ok
  ok := (← assertEq "2" (toString (rawMemBound nestedRaw))) && ok
  -- N4: bounded loop computes (0 iters → 0; n=5 iters of K=1 → 5)
  ok := (← assertEq "0" (toString ((forNTime 0 (constStepBody 1)).val 7))) && ok
  ok := (← assertEq "5" (toString ((forNDiagTime (constStepBody 1)).val 5))) && ok
  -- unified specs compiled (examples above); runtime confirms numbers
  IO.println "OK: CostSpec exact+O1 (lit/expr/assign), closed bounds, tiny budget accept/reject"
  IO.println "OK: N1 parametric call (varies with n; leaf fun _ => K) + N3 nested-mem max"
  IO.println "OK: N2 program cert (bogus rejected, measured body) + N4 linear (poly1/linear) + N5 marker"
  IO.println "OK: Fix1 call spec (RawExpr sums, registry resolve, program worst+fit)"
  IO.println "OK: Fix2 pool/width/full-budget (all four limits checked)"
  if ok then
    IO.println "All costspec tests passed." *> pure 0
  else
    pure 1

end TestCostSpec