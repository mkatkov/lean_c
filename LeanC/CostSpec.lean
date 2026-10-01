import LeanC.Context
import LeanC.TypeClasses
import LeanC.Complexity.BigO
import LeanC.Complexity.Growth
import LeanC.Complexity.Classes
import LeanC.Complexity.Quant
import LeanC.Complexity.Bridge
import LeanC.Literals
import LeanC.Variables
import LeanC.Expr
import LeanC.Stmt

/-!
# Unified cost spec — exact resource + asymptotic class + device budget

Unifies the two previously detached layers:

* asymptotic (`Complexity`: `BigO`, `HasQuantRep`, `costInClass`), which
  classified synthetic values but never programs, and
* syntax (`Literals`/`Expr`/`Stmt`), which carried no bounds.

One value now satisfies both: `CostSpec` stores the exact closed-form
cost function plus the `BigO` membership proof for the same function.
Tiny-device budgeting (`DeviceSpec`/`fitsBudget`) evaluates the exact
side at a concrete input size; aggressive optimization (inlining,
unrolling, `O(n²)` vs `O(n log n)` crossover at small `n`) reads the
hidden Landau constants out of the same proof instead of trusting `=O`.

## Layering

Core canonical resources live here (`StepCost`/`CellCost`) so syntax
measurement does not import `Examples/` (which would invert the
core→example direction). `Examples/Resources.lean` keeps its own
`TimeCost`/`MemCost` as demos; the composition pattern (`BigO.add` for
time, `BigO.max_bound` for memory) is reused here, not moved.

## Cost model (frozen D1 + stage rules)

* Literals: pure (`int`/`char`/`float`) cost `0` time / `0` mem;
  allocating (`str`/`table`) cost `1` time + `len` cells mem.
  Both fold via `BigO.const_le_one` (see `F2`: `cells = len` assumes
  width 1 until `cellSize` is threaded).
* `var`/`addr`: `1` time / `0` mem (load / lea is `O1`, allocates nothing).
* `unop`/`binop`/`cast`/`deref`/`index`/`field`: time adds `+1`,
  memory takes `max` (high-water mark).
* `tern g t e`: time `g + max t e + 1` (guard sequenced before the
  branch join, matching `TimeCost.branchCombine = max + 1`);
  memory `max g t e`.
* `call fname argStrs argTime argMem declaredTime declaredMem`:
  time `argTime n + declaredTime n + 1` (summed arg costs + callee spec +
  dispatch, threaded through `n`), memory
  `max (argMem n) (declaredMem n)` (high-water mark). All four are
  `Nat → Nat` (N1 parametric). Typed args arrive via `RawExpr`
  (`mkCallWithRaw` computes `argTime/argMem` as const fns by recursion,
  not by trust); `declaredTime/Mem` are the callee's `CFunc` spec
  functions and MUST resolve in the program registry
  (`Program.callResolves` — a program is a proof of specs: an unresolved
  `call` fails the program proof, so its local cost below never becomes
  a closed-program claim). The `O1` lemmas below are LOCAL costs
  conditional on the declared bounds (`exprCallsO1Time/Mem` hypothesis);
  they do NOT certify unknown callees. Never use `exprTime_in_o1` on a
  `call` whose `declaredTime/Mem` do not match a registered `CFunc`
  (and are not `O1`).
* Statements (`CAssign`/`CDecl`/`CReturn` are payload-free markers, see
  `F8`): costs are parallel value-level combinators over resource
  values (`assignTime rhs`, …), not type-level fields.

Expression costs are `O1` iff their call specs are `O1` (leaf/external
`fun _ => K` via `const_le_one`); parametric `n` threads through calls
so a linear callee gives a linear caller (first linear inhabitant via
loops in N4, calls preserve it). The exact `K`/`n` matters now for
budgets (`fitsExprAt` evaluates at `n`).
-/

namespace LeanC

/-- Canonical core time resource: steps as `Nat → Nat`.
Mirrors `Examples.TimeCost` so core syntax never imports `Examples/`. -/
structure StepCost where
  val : Nat → Nat

instance : CResourceType StepCost where
  isResourceType := True

instance : CResource StepCost where
  zero := ⟨fun _ => 0⟩
  seqCombine a b := ⟨fun n => a.val n + b.val n⟩
  branchCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n) + 1⟩

instance : HasCost StepCost where
  cost := (·.val)

/-- Canonical core memory resource: peak live cells as `Nat → Nat`.
Sequencing takes the high-water `max` (slots reused); branching maxes. -/
structure CellCost where
  val : Nat → Nat

instance : CResourceType CellCost where
  isResourceType := True

instance : CResource CellCost where
  zero := ⟨fun _ => 0⟩
  seqCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n)⟩
  branchCombine a b := ⟨fun n => Nat.max (a.val n) (b.val n)⟩

instance : HasCost CellCost where
  cost := (·.val)

/-- Unified spec: the exact closed-form cost function PLUS the `BigO`
membership for the same function. `exact_eq` ties the resource value
to `exact`, so `asymp` (a `BigO` about `exact`) transports to
`costInClass` about the value. Lives in `Type` (not `Prop`) because
`exact` is data — the concrete bound tiny budgets evaluate. -/
structure CostSpec (R : Type 0) [HasCost R] (axis : ResourceAxis)
    (C : Type) [HasQuantRep axis C] (v : R) : Type where
  exact : Nat → Nat
  exact_eq : HasCost.cost v = exact
  asymp : BigO exact (HasQuantRep.rep (axis := axis) (α := C))

/-- The asymptotic half of a `CostSpec` as a `costInClass` claim. -/
theorem CostSpec.toCostInClass {R : Type 0} [HasCost R]
    {axis : ResourceAxis} {C : Type} [HasQuantRep axis C] {v : R}
    (s : CostSpec R axis C v) : costInClass (axis := axis) R C v := by
  show BigO (HasCost.cost v) _
  rw [s.exact_eq]
  exact s.asymp

/-- The concrete half: unfold `asymp` to the explicit `c, N₀` inequality
optimizers evaluate at small `n` (where hidden constants decide
`O(n²)` vs `O(n log n)`). -/
theorem CostSpec.bound {R : Type 0} [HasCost R]
    {axis : ResourceAxis} {C : Type} [HasQuantRep axis C] {v : R}
    (s : CostSpec R axis C v) :
    ∃ c N₀, ∀ n ≥ N₀, s.exact n ≤ c * Nat.max ((HasQuantRep.rep (axis := axis) (α := C)) n) 1 :=
  s.asymp

/-- Promotion to the finite ceiling reuses the bridge (existential
forgetting via the class rep as witness). -/
theorem CostSpec.toHalts {R : Type 0} [HasCost R]
    {C : Type} [HasQuantRep .time C] {v : R}
    (s : CostSpec R .time C v) : costInHalts R v :=
  costInClass_to_halts s.toCostInClass

theorem CostSpec.toBounded {R : Type 0} [HasCost R]
    {C : Type} [HasQuantRep .memory C] {v : R}
    (s : CostSpec R .memory C v) : costInBounded R v :=
  costInClass_to_bounded s.toCostInClass

/-- Tiny-device envelope: all limits are closed `Nat`s, ALL CHECKED
(Fix 2 — no dead fields). `flash` counts live static cells
(`poolCells` below: `sum LiteralPoolEntry.cells` + any extra static
`flashCells` at the check site); `sram` is the runtime high-water
check; `stack` counts fixed-block frames (currently `0` for the
loop-free fragment — no fixed-block model yet, but the field is
checked, not ignored: callers pass `0` explicitly); `maxCycles` is
the time check. -/
structure DeviceSpec where
  flash : Nat
  sram : Nat
  stack : Nat
  maxCycles : Nat
deriving DecidableEq, Repr

/-- Static pool footprint: `sum cells` over live global-statics.
This IS `poolBound` (the name the old plan used for the hardwired
pair): in the generic resource model it is a plain `Nat` fold, not a
`ResourceBound` field — costs flow per-resource via `HasCost`, the
pool sum flows here for the `flash` check. -/
def poolCells (pool : List LiteralPoolEntry) : Nat :=
  (pool.map (fun e => e.cells)).sum

theorem poolCells_nil : poolCells [] = 0 := rfl

theorem poolCells_cons (e : LiteralPoolEntry) (es : List LiteralPoolEntry) :
    poolCells (e :: es) = e.cells + poolCells es := by
  simp [poolCells]

/-- Table footprint with explicit element width (Fix 2, F2 closed):
`cellSize * n`. `CTypeSize.size_of` needs a *value*; `tableLit` only
carries types + lengths, so the width is an explicit parameter the
producer states (char tables: `1`). The old `cells = n` below is the
`cellSize = 1` specialization, now named as such. -/
def tableMemCells (cellSize n : Nat) : Nat :=
  cellSize * n

theorem tableMemCells_width1 (n : Nat) : tableMemCells 1 n = n := by
  simp [tableMemCells]

/-- Decidable budget check at one concrete point — checks ALL FOUR
limits (Fix 2). `flashCells` is typically `poolCells pool + extra`;
`stackCells` is `0` until the fixed-block model lands (pass `0`
explicitly — the check is not skipped). -/
def fitsBudget (d : DeviceSpec) (timeCycles memCells flashCells stackCells : Nat) : Bool :=
  decide (timeCycles ≤ d.maxCycles ∧ memCells ≤ d.sram ∧
    flashCells ≤ d.flash ∧ stackCells ≤ d.stack)

/-- Evaluate a time/mem function pair at input size `n`
(`flashCells`/`stackCells` are size-independent: pool + frames). -/
def fitsCostFns (d : DeviceSpec) (t m : Nat → Nat) (n : Nat)
    (flashCells stackCells : Nat) : Bool :=
  fitsBudget d (t n) (m n) flashCells stackCells

/-- Evaluate a unified time+memory spec pair at `n`. -/
def fitsSpecsAt (d : DeviceSpec) (p : StepCost × CellCost) (n : Nat)
    (flashCells stackCells : Nat) : Bool :=
  fitsCostFns d (HasCost.cost p.1) (HasCost.cost p.2) n flashCells stackCells

/-- Closed-program budget check: worst time/mem at `n` + static pool +
stack frames, all against `d`. This is what `ProgramMeetsSpec` +
budgets trust (program-as-proof: the pool sum is computed, not asserted). -/
def fitsProgramAt (d : DeviceSpec) (worstTime worstMem : Nat)
    (pool : List LiteralPoolEntry) (stackCells n : Nat)
    (t m : Nat → Nat) : Bool :=
  fitsBudget d (Nat.max worstTime (t n)) (Nat.max worstMem (m n))
    (poolCells pool) stackCells

/-- Monotonicity of `max`: the workhorse for memory high-water proofs.
Core-only (`Nat.max_le.mpr` + transitivity), no Mathlib needed. -/
theorem max_mono {a b c d : Nat} (h1 : a ≤ b) (h2 : c ≤ d) :
    Nat.max a c ≤ Nat.max b d := by
  apply Nat.max_le.mpr
  constructor
  · exact Nat.le_trans h1 (Nat.le_max_left _ _)
  · exact Nat.le_trans h2 (Nat.le_max_right _ _)

/-- Correctness of `fitsBudget = true`: all four inequalities hold. -/
theorem fitsBudget_true {d : DeviceSpec} {t m f s : Nat}
    (h : fitsBudget d t m f s = true) :
    t ≤ d.maxCycles ∧ m ≤ d.sram ∧ f ≤ d.flash ∧ s ≤ d.stack := by
  simp only [fitsBudget] at h
  have h' := of_decide_eq_true h
  exact ⟨h'.1, h'.2.1, h'.2.2.1, h'.2.2.2⟩

theorem fitsProgramAt_true {d : DeviceSpec} {wt wm : Nat}
    {pool : List LiteralPoolEntry} {sc n : Nat} {t m : Nat → Nat}
    (h : fitsProgramAt d wt wm pool sc n t m = true) :
    Nat.max wt (t n) ≤ d.maxCycles ∧ Nat.max wm (m n) ≤ d.sram ∧
      poolCells pool ≤ d.flash ∧ sc ≤ d.stack := by
  simp only [fitsProgramAt] at h
  exact fitsBudget_true h

/-! ## Literal measurement (D1) -/

/-- Exact time shape of a literal: pure `0`, allocating `1`. -/
def litTimeFn {α : Type} [IsCType α] : CLiteral α → Nat → Nat
  | .intLit _ _ _ _ => fun _ => 0
  | .charLit _ _ => fun _ => 0
  | .floatLit _ _ _ => fun _ => 0
  | .strLit _ _ => fun _ => 1
  | @CLiteral.tableLit _ _ _ _ _ _ _ => fun _ => 1

/-- Exact memory shape: pure `0`, `str` = byte length, `table` = `n`
cells at width 1 (Fix 2: this IS `litMemFnWithWidth 1` below; pass an
explicit `cellSize` when the element type is wider — see
`tableMemCells` and F2). -/
def litMemFn {α : Type} [IsCType α] : CLiteral α → Nat → Nat
  | .intLit _ _ _ _ => fun _ => 0
  | .charLit _ _ => fun _ => 0
  | .floatLit _ _ _ => fun _ => 0
  | .strLit s _ => fun _ => s.length
  | @CLiteral.tableLit _ _ _ _ n _ _ => fun _ => n

/-- Width-parameterized memory shape (Fix 2): `table` = `cellSize * n`,
`str` unchanged (char width 1). -/
def litMemFnWithWidth {α : Type} [IsCType α] (cellSize : Nat) : CLiteral α → Nat → Nat
  | .intLit _ _ _ _, _ => 0
  | .charLit _ _, _ => 0
  | .floatLit _ _ _, _ => 0
  | .strLit s _, _ => s.length
  | @CLiteral.tableLit _ _ _ _ n _ _, _ => tableMemCells cellSize n

theorem litMemFn_width1 {α : Type} [IsCType α] (l : CLiteral α) (n : Nat) :
    litMemFnWithWidth 1 l n = litMemFn l n := by
  match l with
  | .intLit _ _ _ _ => rfl
  | .charLit _ _ => rfl
  | .floatLit _ _ _ => rfl
  | .strLit _ _ => rfl
  | @CLiteral.tableLit _ _ _ _ _ _ _ => simp [litMemFnWithWidth, litMemFn, tableMemCells]

def litTimeCost {α : Type} [IsCType α] (l : CLiteral α) : StepCost :=
  ⟨litTimeFn l⟩

def litMemCost {α : Type} [IsCType α] (l : CLiteral α) : CellCost :=
  ⟨litMemFn l⟩

/-- Closed literal bounds (the numbers budgets check). Split out so
expression measurement never nested-matches a `CLiteral` GADT inside a
`CExpr` motive (cf. friction F6): `.lit l` forwards to these. -/
def litTimeBound {α : Type} [IsCType α] : CLiteral α → Nat
  | .intLit _ _ _ _ => 0
  | .charLit _ _ => 0
  | .floatLit _ _ _ => 0
  | .strLit _ _ => 1
  | @CLiteral.tableLit _ _ _ _ _ _ _ => 1

def litMemBound {α : Type} [IsCType α] : CLiteral α → Nat
  | .intLit _ _ _ _ => 0
  | .charLit _ _ => 0
  | .floatLit _ _ _ => 0
  | .strLit s _ => s.length
  | @CLiteral.tableLit _ _ _ _ n _ _ => n

theorem litTime_le_bound {α : Type} [IsCType α] (l : CLiteral α) (n : Nat) :
    litTimeFn l n ≤ litTimeBound l := by
  match l with
  | .intLit _ _ _ _ => simp [litTimeFn, litTimeBound]
  | .charLit _ _ => simp [litTimeFn, litTimeBound]
  | .floatLit _ _ _ => simp [litTimeFn, litTimeBound]
  | .strLit _ _ => simp [litTimeFn, litTimeBound]
  | @CLiteral.tableLit _ _ _ _ _ _ _ => simp [litTimeFn, litTimeBound]

theorem litMem_le_bound {α : Type} [IsCType α] (l : CLiteral α) (n : Nat) :
    litMemFn l n ≤ litMemBound l := by
  match l with
  | .intLit _ _ _ _ => simp [litMemFn, litMemBound]
  | .charLit _ _ => simp [litMemFn, litMemBound]
  | .floatLit _ _ _ => simp [litMemFn, litMemBound]
  | .strLit _ _ => simp [litMemFn, litMemBound]
  | @CLiteral.tableLit _ _ _ _ _ _ _ => simp [litMemFn, litMemBound]

theorem litTimeFn_eq {α : Type} [IsCType α] (l : CLiteral α) :
    HasCost.cost (litTimeCost l) = litTimeFn l := rfl

theorem litMemFn_eq {α : Type} [IsCType α] (l : CLiteral α) :
    HasCost.cost (litMemCost l) = litMemFn l := rfl

/-- Pure literals are `Zero`-exact. -/
theorem litTime_zero_int {sz : CIntSize} {sgn : Bool} {v : Int} :
    costInZero StepCost (litTimeCost (.intLit sz sgn v rfl)) := by
  show (litTimeFn (.intLit sz sgn v rfl)) = gZero
  funext n
  simp [litTimeFn, gZero]

theorem litMem_zero_int {sz : CIntSize} {sgn : Bool} {v : Int} :
    costInZero CellCost (litMemCost (.intLit sz sgn v rfl)) := by
  show (litMemFn (.intLit sz sgn v rfl)) = gZero
  funext n
  simp [litMemFn, gZero]

/-- Every literal time is `O1` (pure `0 =O 1`, allocating `1 =O 1`). -/
theorem litTime_in_o1 {α : Type} [IsCType α] (l : CLiteral α) :
    costInClass (axis := .time) StepCost TimeComplexity_O1 (litTimeCost l) := by
  show BigO (litTimeFn l) g1
  match l with
  | .intLit _ _ _ _ => exact bigO_zero_le_one
  | .charLit _ _ => exact bigO_zero_le_one
  | .floatLit _ _ _ => exact bigO_zero_le_one
  | .strLit _ _ => exact BigO.refl _
  | @CLiteral.tableLit _ _ _ _ _ _ _ => exact BigO.refl _

/-- Every literal memory is `O1` (`0` or a fixed `len`). -/
theorem litMem_in_o1 {α : Type} [IsCType α] (l : CLiteral α) :
    costInClass (axis := .memory) CellCost MemoryComplexity_O1 (litMemCost l) := by
  show BigO (litMemFn l) g1
  match l with
  | .intLit _ _ _ _ => exact bigO_zero_le_one
  | .charLit _ _ => exact bigO_zero_le_one
  | .floatLit _ _ _ => exact bigO_zero_le_one
  | .strLit s h =>
    exact BigO.const_le_one _ (fun _ => Nat.le_refl _)
  | @CLiteral.tableLit _ _ _ _ _ _ _ =>
    exact BigO.const_le_one _ (fun _ => Nat.le_refl _)

/-- Unified literal specs (exact + `O1` in one value). -/
def litTimeSpec {α : Type} [IsCType α] (l : CLiteral α) :
    CostSpec StepCost .time TimeComplexity_O1 (litTimeCost l) :=
  { exact := litTimeFn l, exact_eq := rfl, asymp := litTime_in_o1 l }

def litMemSpec {α : Type} [IsCType α] (l : CLiteral α) :
    CostSpec CellCost .memory MemoryComplexity_O1 (litMemCost l) :=
  { exact := litMemFn l, exact_eq := rfl, asymp := litMem_in_o1 l }

/-! ## Expression measurement (stages A–C) -/

/-- Exact time shape, by recursion: sums `+1` per node, `tern` threads
the guard through the branch join (`g + max t e + 1`), `call` threads
`n` (`argTime n + declaredTime n + 1`, N1 parametric). -/
def exprTimeFn {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → Nat → Nat
  | .lit l _ => litTimeFn l
  | .var _ => fun _ => 1
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => fun n => exprTimeFn e n + 1
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => fun n => exprTimeFn l n + exprTimeFn r n + 1
  | @CExpr.cast _ _ _ _ _ _ e _ => fun n => exprTimeFn e n + 1
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => fun n => exprTimeFn e n + 1
  | @CExpr.addr _ _ _ _ _ _ _ _ => fun _ => 1
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => fun n => exprTimeFn base n + 1
  | @CExpr.field _ _ _ _ _ _ base _ _ => fun n => exprTimeFn base n + 1
  | .tern g t e => fun n => exprTimeFn g n + Nat.max (exprTimeFn t n) (exprTimeFn e n) + 1
  | .call _ _ argTime _ declaredTime _ => fun n => argTime n + declaredTime n + 1

/-- Exact memory shape: `max` everywhere (high-water mark); leaves `0`
except allocating literals. `call` maxes summed arg mem with the
callee spec (`max (argMem n) (declaredMem n)`, N1 parametric). -/
def exprMemFn {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → Nat → Nat
  | .lit l _ => litMemFn l
  | .var _ => fun _ => 0
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => exprMemFn e
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => fun n => Nat.max (exprMemFn l n) (exprMemFn r n)
  | @CExpr.cast _ _ _ _ _ _ e _ => exprMemFn e
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => exprMemFn e
  | @CExpr.addr _ _ _ _ _ _ _ _ => fun _ => 0
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => exprMemFn base
  | @CExpr.field _ _ _ _ _ _ base _ _ => exprMemFn base
  | .tern g t e => fun n => Nat.max (exprMemFn g n) (Nat.max (exprMemFn t n) (exprMemFn e n))
  | .call _ _ _ argMem _ declaredMem => fun n => Nat.max (argMem n) (declaredMem n)

/-- Parametric upper bounds (the numbers budgets check at input size
`n`). Every `exprTimeFn e n ≤ exprTimeBound e n` for all `n`; same for
memory. Currently exact (same recursion as `exprTimeFn`/`exprMemFn` —
N1 threads `n` through `call`); looseness enters via `declaredTime/Mem`
upper-bounding callee bodies (discharged at `Program`). The `.lit` case
forwards to `litTimeFn`/`litMemFn` (no nested GADT match in an
expression motive). -/
def exprTimeBound {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → Nat → Nat
  | .lit l _ => litTimeFn l
  | .var _ => fun _ => 1
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => fun n => exprTimeBound e n + 1
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => fun n => exprTimeBound l n + exprTimeBound r n + 1
  | @CExpr.cast _ _ _ _ _ _ e _ => fun n => exprTimeBound e n + 1
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => fun n => exprTimeBound e n + 1
  | @CExpr.addr _ _ _ _ _ _ _ _ => fun _ => 1
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => fun n => exprTimeBound base n + 1
  | @CExpr.field _ _ _ _ _ _ base _ _ => fun n => exprTimeBound base n + 1
  | .tern g t e => fun n => exprTimeBound g n + Nat.max (exprTimeBound t n) (exprTimeBound e n) + 1
  | .call _ _ argTime _ declaredTime _ => fun n => argTime n + declaredTime n + 1

def exprMemBound {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → Nat → Nat
  | .lit l _ => litMemFn l
  | .var _ => fun _ => 0
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => exprMemBound e
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => fun n => Nat.max (exprMemBound l n) (exprMemBound r n)
  | @CExpr.cast _ _ _ _ _ _ e _ => exprMemBound e
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => exprMemBound e
  | @CExpr.addr _ _ _ _ _ _ _ _ => fun _ => 0
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => exprMemBound base
  | @CExpr.field _ _ _ _ _ _ base _ _ => exprMemBound base
  | .tern g t e => fun n => Nat.max (exprMemBound g n) (Nat.max (exprMemBound t n) (exprMemBound e n))
  | .call _ _ _ argMem _ declaredMem => fun n => Nat.max (argMem n) (declaredMem n)

def exprTimeCost {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) : StepCost :=
  ⟨exprTimeFn e⟩

def exprMemCost {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) : CellCost :=
  ⟨exprMemFn e⟩

/-- Time bound is sound: the exact function never exceeds the
parametric bound, at any input size. Proved by structural recursion
(`match`, not `induction ... with` — the GADT motives for
`deref`/`index` defeat the `induction` tactic; the equation compiler
handles the same split that `emitExpr` uses). Currently exact, so each
case is `≤` via IH + `omega`/`max_mono`. -/
theorem exprTime_le_bound {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) (n : Nat) : exprTimeFn e n ≤ exprTimeBound e n :=
  match e with
  | .lit _ _ => Nat.le_refl _
  | .var _ => Nat.le_refl _
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => by
    have ih := exprTime_le_bound e n
    simp only [exprTimeFn, exprTimeBound]
    omega
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => by
    have ihl := exprTime_le_bound l n
    have ihr := exprTime_le_bound r n
    simp only [exprTimeFn, exprTimeBound]
    omega
  | @CExpr.cast _ _ _ _ _ _ e _ => by
    have ih := exprTime_le_bound e n
    simp only [exprTimeFn, exprTimeBound]
    omega
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => by
    have ih := exprTime_le_bound e n
    simp only [exprTimeFn, exprTimeBound]
    omega
  | @CExpr.addr _ _ _ _ _ _ _ _ => Nat.le_refl _
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => by
    have ih := exprTime_le_bound base n
    simp only [exprTimeFn, exprTimeBound]
    omega
  | @CExpr.field _ _ _ _ _ _ base _ _ => by
    have ih := exprTime_le_bound base n
    simp only [exprTimeFn, exprTimeBound]
    omega
  | .tern g t e => by
    have ihg := exprTime_le_bound g n
    have iht := exprTime_le_bound t n
    have ihe := exprTime_le_bound e n
    have hmax : Nat.max (exprTimeFn t n) (exprTimeFn e n) ≤
        Nat.max (exprTimeBound t n) (exprTimeBound e n) :=
      max_mono iht ihe
    simp only [exprTimeFn, exprTimeBound]
    omega
  | .call _ _ _ _ _ _ => Nat.le_refl _

/-- Memory bound is sound (same `match`-recursion pattern). -/
theorem exprMem_le_bound {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) (n : Nat) : exprMemFn e n ≤ exprMemBound e n :=
  match e with
  | .lit _ _ => Nat.le_refl _
  | .var _ => Nat.le_refl _
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => exprMem_le_bound e n
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => max_mono (exprMem_le_bound l n) (exprMem_le_bound r n)
  | @CExpr.cast _ _ _ _ _ _ e _ => exprMem_le_bound e n
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => exprMem_le_bound e n
  | @CExpr.addr _ _ _ _ _ _ _ _ => Nat.le_refl _
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => exprMem_le_bound base n
  | @CExpr.field _ _ _ _ _ _ base _ _ => exprMem_le_bound base n
  | .tern g t e =>
    max_mono (exprMem_le_bound g n)
      (max_mono (exprMem_le_bound t n) (exprMem_le_bound e n))
  | .call _ _ _ _ _ _ => Nat.le_refl _

/-- N1 gating predicate (time): all `call` arg/callee specs in `e` are
`O1`. Leaves are `True`; composites conjoin; `call` demands
`BigO argTime g1 ∧ BigO declaredTime g1`. This is the LOCAL hypothesis
that makes `exprTime_in_o1` true; linear callees fail it (caller is
linear, proved via loops/N4 or `BigO` directly). -/
def exprCallsO1Time {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → Prop
  | .lit _ _ => True
  | .var _ => True
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => exprCallsO1Time e
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => exprCallsO1Time l ∧ exprCallsO1Time r
  | @CExpr.cast _ _ _ _ _ _ e _ => exprCallsO1Time e
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => exprCallsO1Time e
  | @CExpr.addr _ _ _ _ _ _ _ _ => True
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => exprCallsO1Time base
  | @CExpr.field _ _ _ _ _ _ base _ _ => exprCallsO1Time base
  | .tern g t e => exprCallsO1Time g ∧ exprCallsO1Time t ∧ exprCallsO1Time e
  | .call _ _ argTime _ declaredTime _ => BigO argTime g1 ∧ BigO declaredTime g1

/-- N1 gating predicate (memory): same shape with `argMem/declaredMem`. -/
def exprCallsO1Mem {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → Prop
  | .lit _ _ => True
  | .var _ => True
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e => exprCallsO1Mem e
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r => exprCallsO1Mem l ∧ exprCallsO1Mem r
  | @CExpr.cast _ _ _ _ _ _ e _ => exprCallsO1Mem e
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => exprCallsO1Mem e
  | @CExpr.addr _ _ _ _ _ _ _ _ => True
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ => exprCallsO1Mem base
  | @CExpr.field _ _ _ _ _ _ base _ _ => exprCallsO1Mem base
  | .tern g t e => exprCallsO1Mem g ∧ exprCallsO1Mem t ∧ exprCallsO1Mem e
  | .call _ _ _ argMem _ declaredMem => BigO argMem g1 ∧ BigO declaredMem g1

/-- `call` time `O1` from `O1` components (`BigO.add` + const folding).
Only `BigO` kit, no Mathlib. -/
theorem callTime_in_o1 {argT decT : Nat → Nat}
    (hat : BigO argT g1) (hdt : BigO decT g1) :
    BigO (fun n => argT n + decT n + 1) g1 := by
  have h1 : BigO (fun _ : Nat => 1) g1 := BigO.refl _
  have hadd : BigO (fun n => argT n + decT n) (fun n => g1 n + g1 n) :=
    BigO.add hat hdt
  have hadd1 : BigO (fun n => argT n + decT n + 1) (fun n => (g1 n + g1 n) + g1 n) :=
    BigO.add hadd h1
  have hfold : BigO (fun n => (g1 n + g1 n) + g1 n) g1 :=
    BigO.const_le_one _ (K := 3) (fun n => by simp [g1])
  exact BigO.trans hadd1 hfold

/-- `call` mem `O1` from `O1` components (`BigO.max_bound` + folding). -/
theorem callMem_in_o1 {am dm : Nat → Nat}
    (ham : BigO am g1) (hdm : BigO dm g1) :
    BigO (fun n => Nat.max (am n) (dm n)) g1 := by
  have hmax : BigO (fun n => Nat.max (am n) (dm n)) (fun n => Nat.max (g1 n) (g1 n)) :=
    BigO.max_bound ham hdm
  have hfold : BigO (fun n => Nat.max (g1 n) (g1 n)) g1 := by
    simpa [g1] using BigO.refl g1
  exact BigO.trans hmax hfold

/-- Unop `+1` preserves `O1` (shared by unop/cast/deref/index/field). -/
theorem o1_add_one {f : Nat → Nat} (h : BigO f g1) :
    BigO (fun n => f n + 1) g1 := by
  have h1 : BigO (fun _ : Nat => 1) g1 := BigO.refl _
  have hadd : BigO (fun n => f n + 1) (fun n => g1 n + g1 n) :=
    BigO.add h h1
  have hfold : BigO (fun n => g1 n + g1 n) g1 :=
    BigO.const_le_one _ (K := 2) (fun n => by simp [g1])
  exact BigO.trans hadd hfold

/-- Binop `l + r + 1` preserves `O1`. -/
theorem o1_binop_add {fl fr : Nat → Nat} (hl : BigO fl g1) (hr : BigO fr g1) :
    BigO (fun n => fl n + fr n + 1) g1 := by
  have hadd : BigO (fun n => fl n + fr n) (fun n => g1 n + g1 n) :=
    BigO.add hl hr
  have haddO1 : BigO (fun n => fl n + fr n) g1 := by
    have hfold : BigO (fun n => g1 n + g1 n) g1 :=
      BigO.const_le_one _ (K := 2) (fun n => by simp [g1])
    exact BigO.trans hadd hfold
  exact o1_add_one haddO1

/-- Tern `g + max t e + 1` preserves `O1`. -/
theorem o1_tern_add {fg ft fe : Nat → Nat}
    (hg : BigO fg g1) (ht : BigO ft g1) (he : BigO fe g1) :
    BigO (fun n => fg n + Nat.max (ft n) (fe n) + 1) g1 := by
  have hmax : BigO (fun n => Nat.max (ft n) (fe n)) (fun n => Nat.max (g1 n) (g1 n)) :=
    BigO.max_bound ht he
  have hmax1 : BigO (fun n => Nat.max (ft n) (fe n)) g1 := by
    have hfold : BigO (fun n => Nat.max (g1 n) (g1 n)) g1 := by
      simpa [g1] using BigO.refl g1
    exact BigO.trans hmax hfold
  have hadd : BigO (fun n => fg n + Nat.max (ft n) (fe n)) (fun n => g1 n + g1 n) :=
    BigO.add hg hmax1
  have hadd' : BigO (fun n => fg n + Nat.max (ft n) (fe n)) g1 := by
    have hfold : BigO (fun n => g1 n + g1 n) g1 :=
      BigO.const_le_one _ (K := 2) (fun n => by simp [g1])
    exact BigO.trans hadd hfold
  exact o1_add_one hadd'

/-- Expression time is `O1` iff its call specs are `O1` (N1 conditional;
loop-free fragment with leaf `fun _ => K` calls).

GATING (Fix 1 + N1): for `call`, this is a LOCAL cost conditional on the
stored `argTime/declaredTime` (`exprCallsO1Time` hypothesis). It becomes
a closed-program claim only when `Program.callResolves` discharges that
the stored `declaredTime/Mem` equal the registered callee's spec (a
program is a proof of specs — unresolved calls fail the program proof).
Linear callees fail the hypothesis (caller is linear, not `O1`). -/
theorem exprTime_in_o1 {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) (h : exprCallsO1Time e) :
    costInClass (axis := .time) StepCost TimeComplexity_O1 (exprTimeCost e) := by
  show BigO (exprTimeFn e) g1
  match e with
  | .lit l _ => exact litTime_in_o1 l
  | .var _ => exact BigO.refl _
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e =>
    simp only [exprCallsO1Time] at h
    simp only [exprTimeFn] at *
    exact o1_add_one (exprTime_in_o1 e h)
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r =>
    simp only [exprCallsO1Time] at h
    simp only [exprTimeFn] at *
    exact o1_binop_add (exprTime_in_o1 l h.1) (exprTime_in_o1 r h.2)
  | @CExpr.cast _ _ _ _ _ _ e _ =>
    simp only [exprCallsO1Time] at h
    simp only [exprTimeFn] at *
    exact o1_add_one (exprTime_in_o1 e h)
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ =>
    simp only [exprCallsO1Time] at h
    simp only [exprTimeFn] at *
    exact o1_add_one (exprTime_in_o1 e h)
  | @CExpr.addr _ _ _ _ _ _ _ _ =>
    simp only [exprTimeFn]
    exact BigO.refl _
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ =>
    simp only [exprCallsO1Time] at h
    simp only [exprTimeFn] at *
    exact o1_add_one (exprTime_in_o1 base h)
  | @CExpr.field _ _ _ _ _ _ base _ _ =>
    simp only [exprCallsO1Time] at h
    simp only [exprTimeFn] at *
    exact o1_add_one (exprTime_in_o1 base h)
  | .tern g t e =>
    simp only [exprCallsO1Time] at h
    simp only [exprTimeFn] at *
    exact o1_tern_add (exprTime_in_o1 g h.1) (exprTime_in_o1 t h.2.1) (exprTime_in_o1 e h.2.2)
  | .call _ _ argT _ decT _ =>
    simp only [exprCallsO1Time] at h
    simp only [exprTimeFn] at *
    exact callTime_in_o1 h.1 h.2

/-- Expression memory is `O1` iff its call specs are `O1` (same gating
as time: `call` mem is local `max (argMem n) (declaredMem n)`,
discharged at `Program`). -/
theorem exprMem_in_o1 {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) (h : exprCallsO1Mem e) :
    costInClass (axis := .memory) CellCost MemoryComplexity_O1 (exprMemCost e) := by
  show BigO (exprMemFn e) g1
  match e with
  | .lit l _ => exact litMem_in_o1 l
  | .var _ => exact bigO_zero_le_one
  | @CExpr.unop _ _ _ _ _ _ _ _ _ _ e =>
    simp only [exprCallsO1Mem] at h
    simp only [exprMemFn] at *
    exact exprMem_in_o1 e h
  | @CExpr.binop _ _ _ _ _ _ _ _ _ _ l r =>
    simp only [exprCallsO1Mem] at h
    simp only [exprMemFn] at *
    exact BigO.max_bound (exprMem_in_o1 l h.1) (exprMem_in_o1 r h.2)
  | @CExpr.cast _ _ _ _ _ _ e _ =>
    simp only [exprCallsO1Mem] at h
    simp only [exprMemFn] at *
    exact exprMem_in_o1 e h
  | @CExpr.deref _ _ _ _ _ _ _ _ e _ _ =>
    simp only [exprCallsO1Mem] at h
    simp only [exprMemFn] at *
    exact exprMem_in_o1 e h
  | @CExpr.addr _ _ _ _ _ _ _ _ =>
    simp only [exprMemFn]
    exact bigO_zero_le_one
  | @CExpr.index _ _ _ _ _ _ _ _ _ base _ _ _ _ =>
    simp only [exprCallsO1Mem] at h
    simp only [exprMemFn] at *
    exact exprMem_in_o1 base h
  | @CExpr.field _ _ _ _ _ _ base _ _ =>
    simp only [exprCallsO1Mem] at h
    simp only [exprMemFn] at *
    exact exprMem_in_o1 base h
  | .tern g t e =>
    simp only [exprCallsO1Mem] at h
    simp only [exprMemFn] at *
    exact BigO.max_bound (exprMem_in_o1 g h.1)
      (BigO.max_bound (exprMem_in_o1 t h.2.1) (exprMem_in_o1 e h.2.2))
  | .call _ _ _ am _ dm =>
    simp only [exprCallsO1Mem] at h
    simp only [exprMemFn] at *
    exact callMem_in_o1 h.1 h.2

/-- `call` parametric bounds expose the declared spec functions
(what `Program` checks): `exprTimeBound (call …) n = argTime n +
declaredTime n + 1`, similarly mem. -/
theorem callTimeBound_eq {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    {fname : String} {argStrs : List String}
    {argTime argMem declaredTime declaredMem : Nat → Nat} (n : Nat) :
    exprTimeBound (Γ := Γ) (α := α)
      (CExpr.call fname argStrs argTime argMem declaredTime declaredMem) n =
      argTime n + declaredTime n + 1 := rfl

theorem callMemBound_eq {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    {fname : String} {argStrs : List String}
    {argTime argMem declaredTime declaredMem : Nat → Nat} (n : Nat) :
    exprMemBound (Γ := Γ) (α := α)
      (CExpr.call fname argStrs argTime argMem declaredTime declaredMem) n =
      Nat.max (argMem n) (declaredMem n) := rfl

/-- Checked-call costs agree with `RawExpr` sums by construction
(`mkCallWithRaw` cannot understate arg costs; N1 threads `n` through
`declaredTime/Mem`). -/
theorem mkCallWithRaw_time {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    {fname : String} {args : List RawExpr} {dt dm : Nat → Nat}
    (h : rawArgsFirstOrder args) (n : Nat) :
    exprTimeFn (Γ := Γ) (α := α) (mkCallWithRaw fname args h dt dm) n =
      rawArgsTime args + dt n + 1 := rfl

theorem mkCallWithRaw_mem {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    {fname : String} {args : List RawExpr} {dt dm : Nat → Nat}
    (h : rawArgsFirstOrder args) (n : Nat) :
    exprMemFn (Γ := Γ) (α := α) (mkCallWithRaw fname args h dt dm) n =
      Nat.max (rawArgsMem args) (dm n) := rfl

/-- `mkCallWithRaw` arg fns are `O1` (closed sums lifted to const fns). -/
theorem rawArgsTimeFn_in_o1 (args : List RawExpr) : BigO (rawArgsTimeFn args) g1 :=
  BigO.const_le_one _ (fun _ => Nat.le_refl _)

theorem rawArgsMemFn_in_o1 (args : List RawExpr) : BigO (rawArgsMemFn args) g1 :=
  BigO.const_le_one _ (fun _ => Nat.le_refl _)

/-- Checked calls with `O1` callee are `O1` (leaf `fun _ => K` case). -/
theorem mkCallWithRaw_in_o1_time {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    {fname : String} {args : List RawExpr} {dt : Nat → Nat}
    (h : rawArgsFirstOrder args)
    (hdt : BigO dt g1) :
    BigO (exprTimeFn (Γ := Γ) (α := α) (mkCallWithRaw fname args h dt (fun _ => 0))) g1 := by
  simpa [mkCallWithRaw, exprTimeFn] using
    callTime_in_o1 (rawArgsTimeFn_in_o1 args) hdt

/-- Unified expression specs (conditional on call `O1` hypotheses). -/
def exprTimeSpec {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) (h : exprCallsO1Time e) :
    CostSpec StepCost .time TimeComplexity_O1 (exprTimeCost e) :=
  { exact := exprTimeFn e, exact_eq := rfl, asymp := exprTime_in_o1 e h }

def exprMemSpec {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) (h : exprCallsO1Mem e) :
    CostSpec CellCost .memory MemoryComplexity_O1 (exprMemCost e) :=
  { exact := exprMemFn e, exact_eq := rfl, asymp := exprMem_in_o1 e h }

/-- Budget check for one expression at input size `n` (Fix 2: takes
`flashCells`/`stackCells` explicitly — no silent `0`). -/
def fitsExprAt {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (d : DeviceSpec) (e : CExpr Γ α) (n : Nat)
    (flashCells stackCells : Nat) : Bool :=
  fitsCostFns d (exprTimeFn e) (exprMemFn e) n flashCells stackCells

/-- Soundness: `fitsExprAt = true` gives all four closed inequalities. -/
theorem fitsExprAt_true {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    {d : DeviceSpec} {e : CExpr Γ α} {n flashCells stackCells : Nat}
    (h : fitsExprAt d e n flashCells stackCells = true) :
    exprTimeFn e n ≤ d.maxCycles ∧ exprMemFn e n ≤ d.sram ∧
      flashCells ≤ d.flash ∧ stackCells ≤ d.stack := by
  simp only [fitsExprAt, fitsCostFns] at h
  exact fitsBudget_true h

/-! ## Statement measurement (value-level combinators, F8)

`CAssign`/`CDecl`/`CReturn` are payload-free markers, so a type-level
`stmtBound` field cannot depend on the RHS value. Costs are parallel
combinators over resource values: sequencing adds time (`BigO.add`
shape) and maxes memory; `return`/`decl` add the `O1` store/dispatch.
-/

/-- `x = rhs`: sequence `rhs` then the `O1` store. -/
def assignTime (rhs : StepCost) : StepCost :=
  CResource.seqCombine rhs ⟨fun _ => 1⟩

def assignMem (rhs : CellCost) : CellCost :=
  CResource.seqCombine rhs ⟨fun _ => 0⟩

/-- `auto x = init` / `auto x;`: same shape as assign (dispatch `O1`). -/
def declTime (init : StepCost) : StepCost :=
  CResource.seqCombine init ⟨fun _ => 1⟩

def declMem (init : CellCost) : CellCost :=
  CResource.seqCombine init ⟨fun _ => 0⟩

/-- `return e`: sequence `e` then the `O1` return dispatch. -/
def returnTime (e : StepCost) : StepCost :=
  CResource.seqCombine e ⟨fun _ => 1⟩

def returnMem (e : CellCost) : CellCost :=
  CResource.seqCombine e ⟨fun _ => 0⟩

/-- Preservation: `O1` RHS stays `O1` through assign/decl/return
(const folding `O1 + O1 = O1`). Each is one `BigO.add` with the `O1`
guard, i.e. the per-resource composition rule. -/
theorem assignTime_in_o1 {rhs : StepCost}
    (h : costInClass (axis := .time) StepCost TimeComplexity_O1 rhs) :
    costInClass (axis := .time) StepCost TimeComplexity_O1 (assignTime rhs) := by
  show BigO (fun n => rhs.val n + 1) g1
  have h1 : BigO (fun _ : Nat => 1) g1 := BigO.refl _
  have hadd : BigO (fun n => rhs.val n + 1) (fun n => g1 n + g1 n) :=
    BigO.add h h1
  have hfold : BigO (fun n => g1 n + g1 n) g1 :=
    BigO.const_le_one _ (K := 2) (fun n => by simp [g1])
  exact BigO.trans hadd hfold

theorem assignMem_in_o1 {rhs : CellCost}
    (h : costInClass (axis := .memory) CellCost MemoryComplexity_O1 rhs) :
    costInClass (axis := .memory) CellCost MemoryComplexity_O1 (assignMem rhs) := by
  show BigO (fun n => Nat.max (rhs.val n) 0) g1
  have h0 : BigO (fun _ : Nat => 0) g1 := bigO_zero_le_one
  have hmax : BigO (fun n => Nat.max (rhs.val n) 0) (fun n => Nat.max (g1 n) (g1 n)) :=
    BigO.max_bound h h0
  have hfold : BigO (fun n => Nat.max (g1 n) (g1 n)) g1 := by
    simpa [g1] using BigO.refl g1
  exact BigO.trans hmax hfold

theorem declTime_in_o1 {init : StepCost}
    (h : costInClass (axis := .time) StepCost TimeComplexity_O1 init) :
    costInClass (axis := .time) StepCost TimeComplexity_O1 (declTime init) :=
  assignTime_in_o1 h

theorem declMem_in_o1 {init : CellCost}
    (h : costInClass (axis := .memory) CellCost MemoryComplexity_O1 init) :
    costInClass (axis := .memory) CellCost MemoryComplexity_O1 (declMem init) :=
  assignMem_in_o1 h

theorem returnTime_in_o1 {e : StepCost}
    (h : costInClass (axis := .time) StepCost TimeComplexity_O1 e) :
    costInClass (axis := .time) StepCost TimeComplexity_O1 (returnTime e) :=
  assignTime_in_o1 h

theorem returnMem_in_o1 {e : CellCost}
    (h : costInClass (axis := .memory) CellCost MemoryComplexity_O1 e) :
    costInClass (axis := .memory) CellCost MemoryComplexity_O1 (returnMem e) :=
  assignMem_in_o1 h

/-- End-to-end: assigning an `O1`-gated expression result stays `O1`
on both axes (N1 conditional). -/
theorem assignExpr_in_o1 {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (e : CExpr Γ α) (ht : exprCallsO1Time e) (hm : exprCallsO1Mem e) :
    costInClass (axis := .time) StepCost TimeComplexity_O1
      (assignTime (exprTimeCost e)) ∧
    costInClass (axis := .memory) CellCost MemoryComplexity_O1
      (assignMem (exprMemCost e)) :=
  ⟨assignTime_in_o1 (exprTime_in_o1 e ht), assignMem_in_o1 (exprMem_in_o1 e hm)⟩

/-- Unified statement specs for the assign case (decl/return identical). -/
def assignTimeSpec {rhs : StepCost}
    (h : costInClass (axis := .time) StepCost TimeComplexity_O1 rhs) :
    CostSpec StepCost .time TimeComplexity_O1 (assignTime rhs) :=
  { exact := HasCost.cost (assignTime rhs), exact_eq := rfl,
    asymp := assignTime_in_o1 h }

def assignMemSpec {rhs : CellCost}
    (h : costInClass (axis := .memory) CellCost MemoryComplexity_O1 rhs) :
    CostSpec CellCost .memory MemoryComplexity_O1 (assignMem rhs) :=
  { exact := HasCost.cost (assignMem rhs), exact_eq := rfl,
    asymp := assignMem_in_o1 h }

end LeanC
