import LeanC.Types
import LeanC.TypeClasses
import LeanC.Arrays
import LeanC.Context
import LeanC.Complexity
import LeanC.Literals
import LeanC.Variables
import LeanC.Expr
import LeanC.Stmt
import LeanC.Func
import LeanC.Modules
import LeanC.Program

open Lean IO

/-!
# Tests + stdlib pilot for literals + expressions (T6+T7)

Location choice (T6 agent's choice, one place): `Tests/` (this file) —
no new top-level `Stdlib/` dir to keep the draft's import closure small.
See friction F7.

Pilot (friction discovery, not coverage):
- `add2`: pure `binop` (`1 + 2`), `(O1,O1)`.
- `hello`: `strLit` in pool + `puts` call, `(O1,O1)`.
- `aget`: guarded `index` with `within_bounds` proof (`CNatIndex`, not
  `CConstIndex` — universe `Type (u+1)` vs `Type 0`, see friction F4).
  No proof = no term (documented below; Lean rejects the missing-arg
  application at elaboration).
- `map_step`: single-iteration body `y = x + 1` via `assignBound`.
Each exposes `worst_bound` + `BigO` membership.
-/

namespace TestLiteralsExpr

open LeanC

/-- Dummy scope context is `DraftCtx` (empty). -/
def ctx0 : DraftCtx := emptyDraft

/-! ## T7 checks: pure vs allocating literals -/

/-- Pure `intLit` costs `(Zero,Zero)` definitionally. -/
example : (litBound (α := CIntType .I32 true) (.intLit .I32 true 42 rfl)).timeRep = gZero := rfl
example : (litBound (α := CIntType .I32 true) (.intLit .I32 true 42 rfl)).memRep = gZero := rfl

/-- `strLit` costs `(O1,O1)` with `mem = const len`. -/
example : BigO
    (litBound (α := CGlobalStaticMemoryBlock (CCharType Unit) 5)
      (.strLit "hello" rfl)).timeRep g1 :=
  BigO.refl _
example : BigO
    (litBound (α := CGlobalStaticMemoryBlock (CCharType Unit) 5)
      (.strLit "hello" rfl)).memRep g1 :=
  BigO.const_le_one _ (fun _ => Nat.le_refl _)

/-! ## Pilot: add2 (binop) -/

/-- `add2` expr: `1 + 2` (signed I32). -/
def add2Expr : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.binop .add
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 1 rfl))
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 2 rfl))

/-- `add2` worst bound. -/
def add2Bound : ResourceBound := exprBound add2Expr

example : BigO add2Bound.timeRep g1 := by
  apply BigO.const_le_one (K := 1)
  intro n
  exact Nat.le_refl _

example : BigO add2Bound.memRep g1 := by
  apply BigO.const_le_one (K := 0)
  intro n
  exact Nat.zero_le _

/-- `add2` as a func: body `O(1)` discharged against `O(1)` cost. -/
def add2Func : CFunc :=
  { fname := "add2", bodyBound := add2Bound,
    declaredCost := { timeRep := g1, memRep := g1 } }

example : BigO add2Func.bodyBound.timeRep add2Func.declaredCost.timeRep := by
  apply BigO.const_le_one (K := 1)
  intro n
  exact Nat.le_refl _

/-- `funcSound`: body `≤` declared (per-func discharge for `call`). -/
example : BigO add2Func.bodyBound.timeRep add2Func.declaredCost.timeRep := by
  apply BigO.const_le_one (K := 1)
  intro n
  exact Nat.le_refl _

/-! ## Pilot: hello (str literal in pool + puts call) -/

/-- `hello` literal: `"hello"` (5 chars). -/
def helloLit : CLiteral (CGlobalStaticMemoryBlock (CCharType Unit) 5) :=
  CLiteral.strLit "hello" rfl

/-- `hello` pool entry: 5 cells. -/
def helloEntry : LiteralPoolEntry :=
  { ty := CGlobalStaticMemoryBlock (CCharType Unit) 5, cells := 5 }

/-- `hello` pool has exactly 1 entry. -/
def helloPool : List LiteralPoolEntry := [helloEntry]

example : helloPool.length = 1 := rfl

example : BigO (poolBound helloPool).timeRep g1 :=
  bigO_zero_le_one

example : BigO (poolBound helloPool).memRep g1 := by
  apply BigO.const_le_one (K := 5)
  intro n
  exact Nat.le_refl _

/-- `hello` call: `puts("hello")` returning I32. `argsBound` is the folded
bound of the real arg (`litBound helloLit`), computed beforehand — the
link is by construction (see `Expr` F5). -/
def helloArgsBound : ResourceBound := litBound helloLit

def helloCall : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.call "puts" ["\"hello\""] helloArgsBound
    { timeRep := g1, memRep := g1 }

def helloBound : ResourceBound := exprBound helloCall

example : BigO helloBound.timeRep g1 := by
  apply BigO.const_le_one (K := 3)
  intro n
  exact Nat.le_refl _

example : BigO helloBound.memRep g1 := by
  apply BigO.const_le_one (K := 5)
  intro n
  exact Nat.le_refl _

/-! ## Pilot: aget (guarded index, proof-required) -/

/-- Array base: `{10,20,30,40}` as static table (4 × I32). -/
def agetBase : CExpr DraftCtx
    (CGlobalStaticMemoryBlock (CIntType .I32 true) 4) :=
  CExpr.lit (α := CGlobalStaticMemoryBlock (CIntType .I32 true) 4)
    (@CLiteral.tableLit _ _ (CIntType .I32 true) _ 4
      ⟨#[10, 20, 30, 40], by decide⟩ rfl)

/-- Index proof: `2 < 4` via `CNatIndex` (Type 0). NOTE: spec says
`CConstIndex`, but `CConstIndex : Type (u+1)` cannot inhabit
`within_bounds (β : Type 0)` — see friction F4. `CNatIndex` is the
Type-0 replacement; the proof obligation (no proof = no term) is
unchanged: omitting the last arg fails to elaborate (documented, not
compiled — Lean rejects `CExpr.index base rfl (CNatIndex 2)`). -/
def agetProof :
    CArray.within_bounds
      (CGlobalStaticMemoryBlock (CIntType .I32 true) 4) (CNatIndex 2) := by
  show (2 < 4)
  decide

/-- `aget` expr: `base[2]`. -/
def agetExpr : CExpr DraftCtx (CIntType .I32 true) :=
  @CExpr.index DraftCtx _ _ _ _ _ _ _ _
    agetBase rfl (CNatIndex 2) _ agetProof

def agetBound : ResourceBound := exprBound agetExpr

example : BigO agetBound.timeRep g1 := by
  apply BigO.const_le_one (K := 2)
  intro n
  exact Nat.le_refl _

example : BigO agetBound.memRep g1 := by
  apply BigO.const_le_one (K := 4)
  intro n
  exact Nat.le_refl _

/-! ## Pilot: map_step (single-iteration body `y = x + 1`) -/

def mapX : CVarRef DraftCtx (CIntType .I32 true) := CVarRef.mk 0
def mapY : CVarRef DraftCtx (CIntType .I32 true) := CVarRef.mk 1

/-- `x + 1` expr. -/
def mapIncr : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.binop .add (CExpr.var mapX)
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 1 rfl))

/-- `map_step` bound: `assignBound y (x+1)` = `seqBound (exprBound …) varBound`. -/
def mapStepBound : ResourceBound := assignBound mapY mapIncr

example : BigO mapStepBound.timeRep g1 := by
  apply BigO.const_le_one (K := 3)
  intro n
  exact Nat.le_refl _

example : BigO mapStepBound.memRep g1 := by
  apply BigO.const_le_one (K := 0)
  intro n
  exact Nat.zero_le _

def mapStepStmt : String := emitAssign mapY mapIncr

/-! ## T7 checks: tern + call + pool + emit -/

/-- Tern guard `max+1 =O 1`: all-`O(1)` leaves fold to const `3`. -/
def ternGuard : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 1 rfl)
def ternThen : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 2 rfl)
def ternElse : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 3 rfl)
def ternEx : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.tern ternGuard ternThen ternElse

example : BigO (exprBound ternEx).timeRep g1 :=
  tern_guard_still_O1 ternGuard ternThen ternElse
    (fun _ => Nat.zero_le _) (fun _ => Nat.zero_le _) (fun _ => Nat.zero_le _)

/-- Call sums `declaredCost`: `time = args + declared + 1` by `call_time_eq`. -/
example : (exprBound (Γ := DraftCtx)
    (CExpr.call (α := CIntType .I32 true) "f" ["x0"]
      { timeRep := g1, memRep := gZero } { timeRep := g1, memRep := gZero })).timeRep =
    fun n => g1 n + g1 n + 1 := by
  simp only [exprBound]

def assertEq (expected actual : String) : IO Bool :=
  if expected == actual then
    IO.println s!"OK: {actual}" *> pure true
  else
    IO.eprintln s!"FAIL: expected {expected} but got {actual}" *> pure false

def test : IO UInt32 := do
  let mut ok := true
  -- literal emitters
  ok := (← assertEq "42" (emitLit (α := CIntType .I32 true) (.intLit .I32 true 42 rfl))) && ok
  ok := (← assertEq "42u" (emitLit (α := CIntType .I32 false) (.intLit .I32 false 42 rfl))) && ok
  ok := (← assertEq "\"hello\"" (emitLit helloLit)) && ok
  -- expr emitters (Stage A→C sketches)
  ok := (← assertEq "(1 + 2)" (emitExpr add2Expr)) && ok
  ok := (← assertEq "puts(\"hello\")" (emitExpr helloCall)) && ok
  ok := (← assertEq "{10, 20, 30, 40}[2]" (emitExpr agetExpr)) && ok
  ok := (← assertEq "(1 ? 2 : 3)" (emitExpr ternEx)) && ok
  ok := (← assertEq "x1 = (x0 + 1);" mapStepStmt) && ok
  -- pool accounting
  ok := (← assertEq "1" (toString helloPool.length)) && ok
  ok := (← assertEq "5" (toString ((poolBound helloPool).memRep 0))) && ok
  -- bounds are constant-folded (computed witnesses behind the BigO proofs)
  ok := (← assertEq "1" (toString (add2Bound.timeRep 0))) && ok
  ok := (← assertEq "3" (toString (helloBound.timeRep 0))) && ok
  ok := (← assertEq "2" (toString (agetBound.timeRep 0))) && ok
  ok := (← assertEq "3" (toString (mapStepBound.timeRep 0))) && ok
  IO.println "OK: pure lit (0,0); str (O1,O1)"
  IO.println "OK: exprBound (1+2) =O 1 (const folding)"
  IO.println "OK: tern guard max+1 =O 1"
  IO.println "OK: call sums declaredCost"
  IO.println "OK: pool has 1 entry, mem const"
  IO.println "OK: aget with within_bounds proof (CNatIndex; CConstIndex unusable, see friction F4)"
  if ok then
    IO.println "All literals+expr tests passed." *> pure 0
  else
    pure 1

end TestLiteralsExpr
