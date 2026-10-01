import LeanC.Context
import LeanC.Func
import LeanC.Modules
import LeanC.CostSpec

/-!
# Programs — top level: modules + `main` + pool, AS PROOF OF SPECS

A `CProgram` value alone is data. The claim "this program meets its
spec" is the SEPARATE `ProgramMeetsSpec p` proof below: main resolves,
every func body fits its declaration (N1 parametric pointwise), every
`call` site's stored `declaredTime/Mem` functions match the registry
(P1-A3 `ProgramCallsResolve` over the COMPUTED `programCallsComputed`
list — `flatMap` over stored bodies, never producer-listed), every nested
`RawExpr` fname exists (`ProgramNestedResolve`), and the worst envelope +
pool are explicit (`worstTime/worstMem/poolCells` as `Nat → Nat` at input
size `n`). Build a `CProgram`, then prove `ProgramMeetsSpec` — that proof
IS the spec certificate budgets and emitters trust, never the bare value.
Unresolved call ⇒ no proof: the `callsResolve` field demands
`callResolves` per computed site, so a body containing `call "bogus" …`
has no `ProgramMeetsSpec` proof even if the producer "forgets" to list it
(omission impossible — there is no list to omit from, D9).
-/

namespace LeanC

/-- WHAT a program is: modules + designated `main` + pool (P1-A3: NO
stored site list — call-site lists are computed `flatMap` defs below,
D9. P2 E4: vacuous True soundness field deleted). -/
structure CProgram where
  (mods : List CModule)
  (main : String)
  (pool : List LiteralPoolEntry)

/-- All funcs in all modules (flat registry for call resolution). -/
def programFuncs (p : CProgram) : List CFunc :=
  p.mods.flatMap (fun m => m.funcs)

/-- Lookup by name across modules. -/
def programFindFunc (p : CProgram) (fname : String) : Option CFunc :=
  (programFuncs p).find? (fun f => f.fname == fname)

/-- Worst declared time across the whole program (N1 parametric:
pointwise `max` at input size `n`). -/
def programWorstTime (p : CProgram) : Nat → Nat :=
  fun n => (programFuncs p).foldl (fun acc f => Nat.max acc (f.declaredTime n)) 0

/-- Worst declared mem across the whole program (N1 parametric). -/
def programWorstMem (p : CProgram) : Nat → Nat :=
  fun n => (programFuncs p).foldl (fun acc f => Nat.max acc (f.declaredMem n)) 0

/-- Static pool footprint (`sum cells`, see Fix 2 `poolCells`). -/
def programPoolCells (p : CProgram) : Nat :=
  (p.pool.map (fun e => e.cells)).sum

/-- A `call` site resolves: some registered func has this name AND the
stored `declaredTime/Mem` functions equal its spec (N1 parametric
equality, `rfl` for same `fun _ => K` literal). This is what discharges
the `CExpr.call` local-cost assumption (Fix 1 + N1). -/
def callResolves (p : CProgram) (fname : String)
    (dt dm : Nat → Nat) : Prop :=
  ∃ f ∈ programFuncs p, f.fname = fname ∧ f.declaredTime = dt ∧ f.declaredMem = dm

/-- P1-A3: program call sites COMPUTED, never stored (D9): `flatMap`
over stored `CFunc.calls` (themselves computed by `exprCalls` traversal
at `mkFuncWithBody`). Omission impossible by construction.
Transitivity note (P2 E4): `flatMap` over ALL funcs already covers
multi-hop bogus (B's direct `calls` include `bogus` even if A calls B) —
no fixpoint needed. -/
def programCallsComputed (p : CProgram) : List CallSite :=
  (programFuncs p).flatMap (fun f => f.calls)

/-- P1-A3 nested obligations computed (D12 existence-only): `flatMap`
over stored `CFunc.nested` (`exprNested` traversal). -/
def programNestedComputed (p : CProgram) : List String :=
  (programFuncs p).flatMap (fun f => f.nested)

/-- P1-A3 program-wide call resolution over the COMPUTED list: every
traversed call site resolves. Unresolved call ⇒ no proof (existential
fails for bogus `fname` or mismatched spec fns). A body containing
`call "bogus" …` yields no `ProgramMeetsSpec` even if the producer
"forgets" — there is no list to forget from. -/
def ProgramCallsResolve (p : CProgram) : Prop :=
  ∀ s ∈ programCallsComputed p, callResolves p s.fname s.declaredTime s.declaredMem

/-- P1-A3/D12 nested resolution (existence only): every nested `RawExpr`
`fname` names a registered func (`== true`). Under the first-order-only
restriction (P1-A4) computed lists are `[]` and this holds vacuously;
it still closes the hole if traversal ever populates `nested`. -/
def ProgramNestedResolve (p : CProgram) : Prop :=
  ∀ fname ∈ programNestedComputed p, ∃ f ∈ programFuncs p, (f.fname == fname) = true

/-- THE program spec certificate: main resolves + every body fits its
declaration (pointwise `∀ n`) + every COMPUTED call site resolves (P1-A3)
+ every nested fname exists + (extensionally) the worst envelope covers
every func pointwise. Carrying `callResolves` per call site is via
`ProgramCallsResolve` over `programCallsComputed`; this bundles the
program-wide obligations. -/
structure ProgramMeetsSpec (p : CProgram) : Prop where
  mainExists : ∃ f ∈ programFuncs p, f.fname = p.main
  allBodiesFit : ∀ f ∈ programFuncs p,
    (∀ n, f.bodyTime n ≤ f.declaredTime n) ∧ (∀ n, f.bodyMem n ≤ f.declaredMem n)
  callsResolve : ProgramCallsResolve p
  nestedResolve : ProgramNestedResolve p
  worstCovers : ∀ f ∈ programFuncs p, ∀ n,
    f.declaredTime n ≤ programWorstTime p n ∧ f.declaredMem n ≤ programWorstMem p n

/-- `foldl max` only grows: starting `acc` is below the fold
(N1 parametric at input size `n`). -/
theorem foldl_max_ge_acc_time (l : List CFunc) (acc n : Nat) :
    acc ≤ l.foldl (fun a g => Nat.max a (g.declaredTime n)) acc := by
  induction l generalizing acc with
  | nil => exact Nat.le_refl _
  | cons g gs ih =>
    simp only [List.foldl_cons]
    exact Nat.le_trans (Nat.le_max_left _ _) (ih _)

/-- Every member is below the `foldl max` envelope (pointwise at `n`). -/
theorem mem_le_foldl_max_time {f : CFunc} {l : List CFunc} {acc n : Nat}
    (h : f ∈ l) : f.declaredTime n ≤ l.foldl (fun a g => Nat.max a (g.declaredTime n)) acc := by
  induction l generalizing acc with
  | nil => simp at h
  | cons g gs ih =>
    simp only [List.mem_cons] at h
    simp only [List.foldl_cons]
    cases h with
    | inl heq =>
      subst heq
      exact Nat.le_trans (Nat.le_max_right _ _) (foldl_max_ge_acc_time _ _ _)
    | inr hmem => exact ih hmem

theorem foldl_max_ge_acc_mem (l : List CFunc) (acc n : Nat) :
    acc ≤ l.foldl (fun a g => Nat.max a (g.declaredMem n)) acc := by
  induction l generalizing acc with
  | nil => exact Nat.le_refl _
  | cons g gs ih =>
    simp only [List.foldl_cons]
    exact Nat.le_trans (Nat.le_max_left _ _) (ih _)

theorem mem_le_foldl_max_mem {f : CFunc} {l : List CFunc} {acc n : Nat}
    (h : f ∈ l) : f.declaredMem n ≤ l.foldl (fun a g => Nat.max a (g.declaredMem n)) acc := by
  induction l generalizing acc with
  | nil => simp at h
  | cons g gs ih =>
    simp only [List.mem_cons] at h
    simp only [List.foldl_cons]
    cases h with
    | inl heq =>
      subst heq
      exact Nat.le_trans (Nat.le_max_right _ _) (foldl_max_ge_acc_mem _ _ _)
    | inr hmem => exact ih hmem

/-- Worst envelope covers every func by construction (`foldl max`,
pointwise at `n`). -/
theorem programWorstCovers (p : CProgram) :
    ∀ f ∈ programFuncs p, ∀ n,
      f.declaredTime n ≤ programWorstTime p n ∧ f.declaredMem n ≤ programWorstMem p n := by
  intro f hf n
  constructor
  · show f.declaredTime n ≤ (programFuncs p).foldl _ 0
    exact mem_le_foldl_max_time hf
  · show f.declaredMem n ≤ (programFuncs p).foldl _ 0
    exact mem_le_foldl_max_mem hf

/-- Closed-program budget certificate (Fix 2 + program-as-proof + N1
parametric): worst envelope at `n` + pool + stack all fit `d`.
`stackCells` is `0` for the loop-free fragment (checked, not ignored).
Combine with `ProgramMeetsSpec` for the full certificate: specs hold
AND the device accepts. -/
def programFitsDevice (p : CProgram) (d : DeviceSpec) (n stackCells : Nat) : Bool :=
  fitsBudget d (programWorstTime p n) (programWorstMem p n)
    (programPoolCells p) stackCells

theorem programFitsDevice_true {p : CProgram} {d : DeviceSpec} {n sc : Nat}
    (h : programFitsDevice p d n sc = true) :
    programWorstTime p n ≤ d.maxCycles ∧ programWorstMem p n ≤ d.sram ∧
      programPoolCells p ≤ d.flash ∧ sc ≤ d.stack := by
  simp only [programFitsDevice] at h
  exact fitsBudget_true h

/-- Build the full certificate from its parts: worst-cover + bodies +
calls + nested + main. The `worstCovers` field is always
`programWorstCovers p`, so a program proof is four obligations
(main + bodies + calls + nested), never five. Unresolved call ⇒ no
`callsResolve` proof ⇒ no `ProgramMeetsSpec` (see bogus-in-body
negative test). -/
theorem mkProgramMeetsSpec (p : CProgram)
    (hmain : ∃ f ∈ programFuncs p, f.fname = p.main)
    (hbodies : ∀ f ∈ programFuncs p,
      (∀ n, f.bodyTime n ≤ f.declaredTime n) ∧ (∀ n, f.bodyMem n ≤ f.declaredMem n))
    (hcalls : ProgramCallsResolve p)
    (hnested : ProgramNestedResolve p) :
    ProgramMeetsSpec p :=
  { mainExists := hmain, allBodiesFit := hbodies, callsResolve := hcalls,
    nestedResolve := hnested, worstCovers := programWorstCovers p }

end LeanC
