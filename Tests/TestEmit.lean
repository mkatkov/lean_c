import LeanC.Emit
import LeanC.Context
import LeanC.Literals
import LeanC.Variables
import LeanC.Ops
import LeanC.Expr
import LeanC.Func
import LeanC.Program

open Lean IO

/-!
# Thin emission slice test (P1-B)

Machinery validation, not coverage: one certified demo program
(`add2`-shaped, pure `binop`, no calls) emits a single compilable `.c`
file FROM its `ProgramMeetsSpec` proof (`emitProgram` demands the proof,
D11), the test compiles it with the system C compiler (`cc`, fallback
`gcc`) and runs the binary, checking exit `0`. Every pain point → F13+.
-/

namespace TestEmit

open LeanC

/-- Demo body: `1 + 2` (signed I32), same shape as `TestCostSpec.add2`. -/
def demoBody : CExpr DraftCtx (CIntType .I32 true) :=
  CExpr.binop CAddOp.add
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 1 rfl) rfl)
    (CExpr.lit (α := CIntType .I32 true) (.intLit .I32 true 2 rfl) rfl)

/-- Demo func via `mkFuncWithBody` (calls/nested/bodySrc computed). -/
def demoFunc : CFunc :=
  mkFuncWithBody "demo" demoBody (fun _ => 1) (fun _ => 0)
    ⟨fun _ => Nat.le_refl _, fun _ => Nat.le_refl _⟩

def demoProg : CProgram :=
  { mods := [{ funcs := [demoFunc], pool := [] }],
    main := "demo", pool := [] }

example : programCallsComputed demoProg = [] := by
  simp [programCallsComputed, programFuncs, demoProg, demoFunc,
    demoBody, mkFuncWithBody, exprCalls, CFunc.calls]

example : programNestedComputed demoProg = [] := by
  simp [programNestedComputed, programFuncs, demoProg, demoFunc,
    demoBody, mkFuncWithBody, exprNested, CFunc.nested]

/-- Certified demo program (the proof the emitter trusts). -/
def demoProof : ProgramMeetsSpec demoProg :=
  mkProgramMeetsSpec demoProg
    ⟨demoFunc, by simp [programFuncs, demoProg], rfl⟩
    (by intro f hf
        simp only [programFuncs, demoProg] at hf
        cases hf with
        | head _ => exact demoFunc.bodyLeDeclared
        | tail _ h => cases h)
    (by intro s hs
        have hempty : programCallsComputed demoProg = [] := by
          simp [programCallsComputed, programFuncs, demoProg, demoFunc,
            demoBody, mkFuncWithBody, exprCalls, CFunc.calls]
        rw [hempty] at hs
        cases hs)
    (by intro fname hf
        have hempty : programNestedComputed demoProg = [] := by
          simp [programNestedComputed, programFuncs, demoProg, demoFunc,
            demoBody, mkFuncWithBody, exprNested, CFunc.nested]
        rw [hempty] at hf
        cases hf)

/-- Emitted source (from the proof, D11). -/
def demoC : String := emitProgram demoProg demoProof

def cPath := "/tmp/lean_c_emit_test.c"
def binPath := "/tmp/lean_c_emit_test"

/-- Try `cc`, fallback `gcc` (probe at runtime, fail loudly if absent). -/
def compileC (cPath binPath : String) : IO Bool := do
  let args := #["-o", binPath, cPath]
  let ccOut ← try
    IO.Process.output { cmd := "cc", args := args }
  catch _ =>
    pure { exitCode := (1 : UInt32), stdout := "", stderr := "cc missing" }
  if ccOut.exitCode == 0 then
    pure true
  else do
    IO.println s!"cc failed ({ccOut.exitCode}): {ccOut.stderr}; trying gcc"
    let gccOut ← try
      IO.Process.output { cmd := "gcc", args := args }
    catch e =>
      throw (.userError s!"no C compiler (cc and gcc absent): {e}")
    if gccOut.exitCode == 0 then
      pure true
    else do
      IO.eprintln s!"FAIL: C compile failed: {gccOut.stderr}"
      pure false

def test : IO UInt32 := do
  IO.FS.writeFile cPath demoC
  IO.println s!"emitted {cPath}:"
  IO.println demoC
  let compiled ← compileC cPath binPath
  if !compiled then
    return 1
  let runOut ← try
    IO.Process.output { cmd := binPath, args := #[] }
  catch e =>
    IO.eprintln s!"FAIL: running {binPath}: {e}"
    pure { exitCode := (1 : UInt32), stdout := "", stderr := toString e }
  if runOut.exitCode == 0 then
    IO.println s!"OK: {binPath} exited 0" *> pure 0
  else
    IO.eprintln s!"FAIL: {binPath} exited {runOut.exitCode}: {runOut.stderr}" *> pure 1

end TestEmit
