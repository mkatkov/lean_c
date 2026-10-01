import LeanC.Types
import LeanC.TypeClasses
import LeanC.Arrays
import LeanC.Context
import LeanC.Literals
import LeanC.Variables
import LeanC.Ops
import LeanC.Expr
import LeanC.Stmt
import LeanC.Func

open Lean IO

/-!
# Tests + stdlib pilot for literals + expressions (T6+T7)

Location choice (T6 agent's choice, one place): `Tests/` (this file) —
no new top-level `Stdlib/` dir to keep the draft's import closure small.
See friction F7.

Pilot (friction discovery, not coverage):
- `add2`: pure `binop` (`1 + 2`).
- `hello`: `strLit` in pool + `puts` call.
- `aget`: guarded `index` with `within_bounds` proof (`CNatIndex`, not
  `CConstIndex` — universe `Type (u+1)` vs `Type 0`, see friction F4).
  No proof = no term (documented below; Lean rejects the missing-arg
  application at elaboration).
- `map_step`: single-iteration body `y = x + 1` via `emitAssign`.
- Per-op typing: unsigned `add` preserves, `lt` returns signed `I32`.

Costs (if any) are assigned per resource type via `CContext.setResource`
+ `CResource` instances (see `Examples/Resources.lean`) — no hardwired
bounds here; this file checks syntax + emission.
-/

namespace TestLiteralsExpr

open LeanC

/-- Dummy scope context is `DraftCtx` (empty). -/
def ctx0 : DraftCtx := emptyDraft

/-! ## T7 checks: pure vs allocating literals -/

/-- `intLit` fits check still holds. -/
example : litFitsType (α := CIntType .I32 true) (.intLit .I32 true 42 rfl) = true := by
  decide

/-! ## Pilot: add2 (binop) -/

/-- `add2` expr: `1 + 2` (signed I32). -/
def add2Expr : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.binop CAddOp.add
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 1 rfl) rfl)
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 2 rfl) rfl)

/-- `add2` as a func-proof: declared `(1, 0)` covers body `(1, 0)`
(N1 parametric `fun _ => K`; program-as-proof: the `bodyLeDeclared`
proof IS the spec certificate). P1-A2: built via `mkFuncWithBody`
(`calls/nested/bodySrc` computed, not listed). -/
def add2Func : CFunc :=
  mkFuncWithBody "add2" add2Expr (fun _ => 1) (fun _ => 0)
    ⟨fun _ => Nat.le_refl _, fun _ => Nat.le_refl _⟩

/-- `puts` external spec used by `helloCall` (`declared 10/0`, N1
`fun _ => K`). P1-A2: leaf via `mkLeafFunc`. -/
def putsFunc : CFunc :=
  mkLeafFunc "puts" (fun _ => 10) (fun _ => 0)

/-! ## Per-op typing: unsigned `add` preserves, `lt` returns signed `I32` -/

/-- Unsigned `1u + 2u`: `add` on `CUInt32Type` yields `CUInt32Type`. -/
def uaddExpr : CExpr DraftCtx CUInt32Type :=
  CExpr.binop CAddOp.add
    (CExpr.lit (α := CUInt32Type) (.intLit .I32 false 1 rfl) rfl)
    (CExpr.lit (α := CUInt32Type) (.intLit .I32 false 2 rfl) rfl)

/-- Comparing unsigned yields signed `I32`, not unsigned. -/
def ultExpr : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.binop CLtOp.lt
    (CExpr.lit (α := CUInt32Type) (.intLit .I32 false 1 rfl) rfl)
    (CExpr.lit (α := CUInt32Type) (.intLit .I32 false 2 rfl) rfl)

example : (emitExpr ultExpr) = "(1u < 2u)" := rfl

/-! ## P2 E3b/c hardening: valid casts + struct fields (enforcing, not culture) -/

/-- P2 E3b positive: `widen` I8→I16 typechecks; `cast e (fun _ _ => True)`
no longer elaborates (second arg must be `ValidCast`). -/
def i8One : CExpr DraftCtx (CIntType .I8 true) :=
  CExpr.lit (α := CIntType .I8 true) (.intLit .I8 true 1 rfl) rfl

def i16FromI8 : CExpr DraftCtx (CIntType .I16 true) :=
  CExpr.cast i8One .widen8_16

example : emitExpr i16FromI8 = "((cast)1)" := rfl

/-- P2 E3c minimal test struct (agent's choice, one place, recorded F18):
`PairI32` with field `0 : I32`. No other `HasStructField` instances exist
in P2, so `field` on any other type/idx is ill-typed. -/
inductive PairI32 where
| mk : PairI32
instance : IsCType PairI32 where isCType := True
instance : HasStructField PairI32 0 (CIntType .I32 true) where ok := trivial

def pairBase : CVarRef DraftCtx PairI32 := CVarRef.mk 0 (by decide)
def pairVar : CExpr DraftCtx PairI32 := CExpr.var pairBase
def pairField0 : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.field pairVar 0

example : emitExpr pairField0 = "x0.f0" := rfl

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

/-- `hello` call: `puts("hello")` returning I32.

N1 parametric: `call` stores `argTime/argMem` + `declaredTime/declaredMem`
as `Nat → Nat` (here const `fun _ => 1`/`5` for the single `"hello"` arg
+ external `puts` spec `fun _ => 10`/`fun _ => 0`, discharged by
`Program.callResolves`). Emission unchanged. Checked construction via
`mkCallWithRaw` computes `1`/`5` from `[.strLit "hello"]`; the explicit
form below is the leaf/external spelling (`fun _ => K`). -/
def helloCall : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.call "puts" ["\"hello\""] (fun _ => 1) (fun _ => 5) (fun _ => 10) (fun _ => 0)

/-- Same call via the checked `RawExpr` path (sums by construction, N1
`declared : Nat → Nat`, P2 E2 first-order proof `by simp`). -/
def helloCallChecked : CExpr DraftCtx (CIntType .I32 true) :=
  mkCallWithRaw "puts" [.strLit "hello"] (by simp [rawArgsFirstOrder, rawCallFnames]) (fun _ => 10) (fun _ => 0)

/-! ## Pilot: aget (guarded index, proof-required) -/

/-- Array base: `{10,20,30,40}` as static table (4 × I32). -/
def agetBase : CExpr DraftCtx
    (CGlobalStaticMemoryBlock (CIntType .I32 true) 4) :=
  CExpr.lit (α := CGlobalStaticMemoryBlock (CIntType .I32 true) 4)
    (@CLiteral.tableLit _ _ (CIntType .I32 true) _ 4
      ⟨#[10, 20, 30, 40], by decide⟩ rfl) rfl

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

/-! ## Pilot: map_step (single-iteration body `y = x + 1`) -/

-- P2 E3a: `CVarRef.mk` requires membership proof (`idx < 2` for `DraftCtx`);
-- `CVarRef.mk 999` without proof does not elaborate (see friction F15).
def mapX : CVarRef DraftCtx (CIntType .I32 true) := CVarRef.mk 0 (by decide)
def mapY : CVarRef DraftCtx (CIntType .I32 true) := CVarRef.mk 1 (by decide)

/-- `x + 1` expr. -/
def mapIncr : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.binop CAddOp.add (CExpr.var mapX)
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 1 rfl) rfl)

def mapStepStmt : String := emitAssign mapY mapIncr

/-! ## T7 checks: tern + call + pool + emit -/

def ternGuard : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 1 rfl) rfl
def ternThen : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 2 rfl) rfl
def ternElse : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 3 rfl) rfl
def ternEx : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.tern ternGuard ternThen ternElse

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
  ok := (← assertEq "puts(\"hello\")" (emitExpr helloCallChecked)) && ok
  ok := (← assertEq "{10, 20, 30, 40}[2]" (emitExpr agetExpr)) && ok
  ok := (← assertEq "(1 ? 2 : 3)" (emitExpr ternEx)) && ok
  ok := (← assertEq "x1 = (x0 + 1);" mapStepStmt) && ok
  -- pool accounting
  ok := (← assertEq "1" (toString helloPool.length)) && ok
  IO.println "OK: pure lit; str emit"
  IO.println "OK: expr emitters (binop/tern/call/index)"
  IO.println "OK: per-op typing (unsigned add preserves, lt returns I32)"
  IO.println "OK: pool has 1 entry"
  IO.println "OK: aget with within_bounds proof (CNatIndex; CConstIndex unusable, see friction F4)"
  if ok then
    IO.println "All literals+expr tests passed." *> pure 0
  else
    pure 1

end TestLiteralsExpr
