import LeanC.Program

/-!
# Emission — proof-demanding C emitter (P1-B, thin slice)

Home: `LeanC/Emit.lean` (agent's choice — new file importing `Program`
only, never `Expr` directly; `CFunc.bodySrc` already computed via
`emitExpr` at `mkFuncWithBody`, so no new import direction).

D11: `emitProgram` takes the `ProgramMeetsSpec` proof — uncertified
programs are unemittable by type. Thin slice only: one file,
`#include`s + pool comment + func defs (multi-line `emitFuncDef`;
empty body means external — emit a comment, never a
redefinition of libc symbols) + `int main(void)` calling `p.main`.
Out of scope: struct layout, ABI header (`lean_c_target.h`),
multi-unit linking, Clight semantics.
-/

namespace LeanC

/-- Multi-line func definition from the `CFunc` inductive (P2 E4:
matches by construction, no empty-string test — `leaf`
is external by construction, `withBody` emits from computed body).
External (`leaf`, e.g. `mkLeafFunc "puts"`) emits a comment,
not a conflicting redefinition. -/
def emitFuncDef (f : CFunc) : String :=
  match f with
  | .leaf fname _ _ => "// external: " ++ fname ++ "\n"
  | @CFunc.withBody _ _ _ _ fname body _ _ _ =>
    "void " ++ fname ++ "(void) {\n  " ++ emitExpr body ++ ";\n}\n"

/-- Thin-slice program emitter (D11): demands the `ProgramMeetsSpec`
proof. One compilable `.c` file: stdio include (covers `puts`-shaped
demos; harmless for pure demos) + pool footprint comment + func defs +
`int main` calling `p.main` and returning `0` (so the binary exits `0`). -/
def emitProgram (p : CProgram) (_ : ProgramMeetsSpec p) : String :=
  let header := "#include <stdio.h>\n"
  let poolComment := "// pool cells: " ++ toString (programPoolCells p) ++ "\n"
  let funcs := String.intercalate "\n" ((programFuncs p).map emitFuncDef)
  let mainFn := "int main(void) {\n  " ++ p.main ++ "();\n  return 0;\n}\n"
  header ++ poolComment ++ funcs ++ "\n" ++ mainFn

end LeanC
