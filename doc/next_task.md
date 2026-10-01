# Next Task — P1 Traversal-closed certificate + thin emission slice

Status: P1 landed (2026-09-29): A1 collectors (rawCallFnames/exprCalls/exprNested in Func.lean), A2 CFunc stores calls/nested/bodySrc + mkLeafFunc, A3 program computes (no stored list) + ProgramNestedResolve, A4 first-order-only nested rule + bogus-in-body rejection, B emitProgram from proof + TestEmit cc-green — `lake build` + `./.lake/build/bin/test` green. P0 landed 2026-09-28 (see §8 archive).
Related: `doc/roadmap.md` §§2–3, §7, §9 M2; `doc/complexity_proposal.md`;
`LeanC/CostSpec.lean`, `LeanC/Expr.lean`, `LeanC/Func.lean`,
`LeanC/Program.lean`, `LeanC/Loop.lean`, `LeanC/Complexity/Bridge.lean`;
`doc/stdlib_friction.md` F1–F12.

## 0. Project intent (binding, corrects earlier drift)

A `CProgram` value alone is data. The claim "this program meets its spec"
is the separate `ProgramMeetsSpec p` proof. **The program IS the proof of
specs, and the proof is what gets emitted** — emitters and budgets trust
only `(p, ProgramMeetsSpec p)`, never a bare `CProgram`. Concretely:

- Every `CExpr.call` carries its assumed callee spec
  (`declaredTime/Mem : Nat → Nat`); that assumption is discharged only by
  `Program.callResolves` against the registry.
- Every function body is measured (`bodyTime/Mem = exprTimeBound/MemBound
  body` via `mkFuncWithBody`), not trusted.
- P1 closes the remaining gap (§2): call sites are **computed by traversal
  of stored bodies**, not producer-listed. Omission is then impossible by
  construction — there is no list to omit from.
- P1-B checks the loop is closed end to end: a certified program emits
  compilable C (via `emitProgram (p, _)`, which demands the proof), compiled
  with the system C compiler in `Tests/`.

Earlier docs described "unresolved call fails program proof" as if enforced;
it was discipline (producer-supplied `CProgram.callSites`, `Func` bodies not
stored). This file corrects that: from P1 on, enforcement is by traversal
(P1-A). Where older text conflicts with this section, this section wins.

## 1. Goal

Two tracks, in order:

- **P1-A (guarantee):** make omission impossible. `CFunc` stores its body's
  computed call sites; `CProgram`'s call-site list is a computed `flatMap`
  over the registry, not a field. `ProgramCallsResolve` quantifies over the
  computed list. A body containing `call "bogus" …` therefore yields a
  program with no `ProgramMeetsSpec` proof even if the producer "forgets" to
  list it. Nested `RawExpr` calls (`callRaw` inside `mkCallWithRaw` args)
  are collected as name-obligations with the same treatment.
- **P1-B (thin slice):** prove the proof-emission loop works. One certified
  demo program (`add2`-shaped or `puts`-shaped, agent's choice) emits a
  single compilable `.c` file from its `ProgramMeetsSpec` proof and the test
  compiles it with `cc`/`gcc` and runs it. Goal is machinery validation, not
  coverage — every pain point goes into `doc/stdlib_friction.md` as F13+.

## 2. Why now (the hole P1-A closes)

- `CProgram.callSites : List CallSite := []` (`LeanC/Program.lean:38`) is
  producer-supplied. `ProgramCallsResolve` (`Program.lean:73-74`) checks only
  listed sites. A body built with `mkCallWithRaw "bogus" …` but a `callSites`
  list omitting it still proves `ProgramMeetsSpec` via `mkProgramMeetsSpec`
  — the negative test (`progBogus` in `Tests/TestCostSpec.lean:216-243`)
  passes only because the fixture honestly lists the bogus site.
- `CFunc` (`LeanC/Func.lean:42-50`) stores no body and no calls:
  `mkFuncWithBody` measures `bodyTime/Mem` but discards the body and its
  `exprCalls`; direct `CFunc` literals (e.g. `putsSpec`) carry no call info
  at all. There is nothing to traverse.
- Nested hole: `mkCallWithRaw fname args …` (`LeanC/Expr.lean:173-177`)
  stringifies `args : List RawExpr` into `argStrs`; a nested
  `.callRaw "f" […]` inside `args` contributes costs via `rawTimeBound` /
  `rawMemBound` but its `fname` never appears as a `CallSite`, so registry
  resolution never sees it.

## 3. Frozen decisions (do not relitigate)

| # | Decision | Rationale |
|---|---|---|
| D1–D7 | Carried from P0 (intrinsically typed AST; global-static + pool memory; staged expr A→C; `call ↔ Func` name seam; `HALTS/BOUNDED = IsFiniteCost`, `UNBOUND/GROWING` empty under total costs; `O1 = ≤ K`, `Zero` pointwise) | See §8; unchanged |
| D8 | `call ↔ Func` stays name-broken (`fname` seam, `Expr` never imports `Func`) | Import direction `Emit → Program → Func → Expr` preserved |
| D9 | Call sites computed, never listed: `CFunc` stores `calls`/`nested` at construction; program lists are `flatMap` defs | Omission impossible by construction; no `τ ∉ τs`-style proof burden |
| D10 | `CallSite` moves down to `Func` (or `Expr`); traversal lives in `Func` (or new `LeanC/Calls.lean` importing `Expr` only) | `Expr` must not import `Func`/`Program` (D8); `Program` reaches `CallSite` via `Modules → Func` |
| D11 | Emitter takes the proof: `emitProgram (p : CProgram) (_ : ProgramMeetsSpec p)` (or `Sigma`) | Uncertified programs are unemittable by type; matches §0 intent |
| D12 | Nested `RawExpr` calls are existence-obligations, typed calls are spec-obligations | `RawExpr.callRaw` carries no specs, so inner fnames resolve by name-existence; outer `CExpr.call` resolves by spec-equality (existing `callResolves`) |

Conventions (binding): `Prop`-valued class fields; no
`sorry`/`axiom`/`admit` in new code; `BigO` kit only — no Mathlib dep.

## 4. Scope (P1 only: A1–A4, B1–B3)

- A1 traversal collectors: `rawCallFnames : RawExpr → List String`
  (`LeanC/Expr.lean`, recursion on `RawExpr`; `callRaw` conses its fname +
  flatMaps args); `exprCalls : CExpr Γ α → List CallSite` +
  `exprNested : CExpr Γ α → List String` (`LeanC/Func.lean` or new
  `LeanC/Calls.lean` importing `Expr` only — agent picks one place, records
  it). Rules: `.call fname _ _ _ dt dm` → `[⟨fname, dt, dm⟩]` plus
  `rawCallFnames` over its source `RawExpr` args where available (see A4);
  all other ctors recurse and append (`.binop l r` → `exprCalls l ++
  exprCalls r`, etc., mirroring `exprTimeFn`'s `match`-recursion, not
  `induction … with` — GADT motives defeat the tactic, cf. `CostSpec`).
  Files: `LeanC/Expr.lean`, `LeanC/Func.lean` (or `LeanC/Calls.lean`).
- A2 `CFunc` stores computed calls: add `calls : List CallSite := []` +
  `nested : List String := []` (+ optionally `bodySrc : String` for B2 —
  agent decides, records it). `mkFuncWithBody` sets `calls :=
  exprCalls body`, `nested := exprNested body` (by computation) alongside
  the existing `bodyTime/Mem := exprTimeBound/MemBound body`. Add
  `mkLeafFunc (fname) (dt dm) : CFunc` with `calls := []`, `nested := []`,
  `bodyTime := dt`, `bodyMem := dm` for external specs (`puts`); migrate
  existing direct literals (`putsSpec`, `add2Func`, `putsFunc`) to it.
  Direct non-leaf `CFunc` literals are then ill-discipline (documented;
  full opacity is future work). File: `LeanC/Func.lean`.
- A3 program computes, never stores: **remove** `CProgram.callSites` field;
  add `programCallsComputed (p) : List CallSite :=
  (programFuncs p).flatMap (·.calls)` + `programNestedComputed (p) : List
  String`; redefine `ProgramCallsResolve p := ∀ s ∈ programCallsComputed p,
  callResolves p …` + new `ProgramNestedResolve p := ∀ fname ∈
  programNestedComputed p, ∃ f ∈ programFuncs p, f.fname == fname = true`
  (existence only, D12); add `callsResolve` + `nestedResolve` fields to
  `ProgramMeetsSpec`; `mkProgramMeetsSpec` takes both proofs
  (`programWorstCovers` stays derived). Move `CallSite` to `Func` (D10) and
  fix imports (`Program` reaches it via `Modules → Func`; add direct
  `import LeanC.Func` if clearer — allowed direction). Files:
  `LeanC/Func.lean`, `LeanC/Modules.lean` (re-export if needed),
  `LeanC/Program.lean`.
- A4 nested-call rule (closes §2 third bullet): `exprCalls (.call …)` for a
  `mkCallWithRaw fname args …` term includes the outer singleton **plus**
  every `g ∈ (args.flatMap rawCallFnames)` as a nested obligation (via
  `exprNested`). `ProgramNestedResolve` discharges them. Alternative
  (agent's choice, record it): forbid nested `callRaw` in `mkCallWithRaw`
  args (`rawIsFirstOrder` hypothesis) and keep N3's `nestedRaw` as a
  `rawMemBound`-only example, never lifted to `CExpr`. Either way, a program
  whose transitive call closure mentions an unregistered `fname` has no
  `ProgramMeetsSpec`. `Tests/TestCostSpec.lean` migrates: `progPuts` drops
  `callSites` (computed `[puts]`), `progBogus` builds its bogus call **in a
  body** via `mkFuncWithBody`/`mkCallWithRaw` (not via a listed site) and
  proves `¬ ProgramCallsResolve` + `¬ ProgramMeetsSpec` over the computed
  lists.
- B1–B3 thin emission slice (machinery check, not coverage): new
  `LeanC/Emit.lean` (or `Program.lean` extension — agent picks, records it)
  with `emitProgram (p : CProgram) (_ : ProgramMeetsSpec p) : String`
  (D11): one file, `#include`s + pool globals + func decls (via
  `emitFunc` extended multi-line; `CFunc.bodySrc` if A2 added it, else
  pre-rendered text as today) + `int main(void) { … }` calling `p.main`.
  New `Tests/TestEmit.lean` (`def test : IO UInt32`): builds a certified
  demo program (reuse `add2`/`puts` shape), emits to
  `/tmp/lean_c_emit_test.c`, runs `cc -o …` via `IO.Process.spawn`,
  runs the binary, checks exit `0`; wire into `test.lean:runAll`.
  Out of scope: struct layout, ABI header (`lean_c_target.h`), multi-unit
  linking, Clight semantics.

Out of scope: resource/pool/budget unification (`StepCost/TimeCost`,
`poolCells/programPoolCells`); retiring weak `can_insert_quant_to_list`
alias; facade drift; `cellSize` threading; full Clight semantics;
benchmarks; `Stmt` payload syntax (bodies stay single-`CExpr` until the
statement task).

## 5. Frozen interfaces (agents build against these)

```lean
-- A1: collectors (Func.lean or Calls.lean importing Expr only; RawExpr part in Expr.lean)
-- def rawCallFnames : RawExpr → List String
--   | .callRaw f args => f :: (args.flatMap rawCallFnames) | _ => ...
-- def exprCalls {Γ α} : CExpr Γ α → List CallSite   -- .call → [⟨fname, dt, dm⟩]
-- def exprNested {Γ α} : CExpr Γ α → List String    -- flatMap rawCallFnames over call args

-- A2: func stores computed calls (Func.lean)
-- structure CFunc … (calls : List CallSite := []) (nested : List String := [])
-- def mkFuncWithBody {Γ} [CContext Γ] {α} [IsCType α] (fname) (body : CExpr Γ α)
--   (dt dm) (hle) : CFunc  -- calls := exprCalls body, nested := exprNested body
-- def mkLeafFunc (fname) (dt dm : Nat → Nat) : CFunc  -- calls/nested := []

-- A3: program computes (Program.lean; CallSite lives in Func.lean per D10)
-- def programCallsComputed (p : CProgram) : List CallSite
-- def programNestedComputed (p : CProgram) : List String
-- def ProgramCallsResolve (p) : Prop := ∀ s ∈ programCallsComputed p, callResolves p …
-- def ProgramNestedResolve (p) : Prop := ∀ fname ∈ programNestedComputed p, ∃ f ∈ programFuncs p, …
-- structure ProgramMeetsSpec (p) : Prop … (callsResolve) (nestedResolve) …  -- no p.callSites

-- B: emitter takes the proof (Emit.lean or Program.lean)
-- def emitProgram (p : CProgram) (_ : ProgramMeetsSpec p) : String
```

Bound rules unchanged: call time `argTime n + declaredTime n + 1`, mem
`max (argMem n) (declaredMem n)`; loop sums/maxes. Reuse
`BigO.add`/`max_bound`/`const_le_one`.

## 6. Build order

```text
A1 (collectors) ─▶ A2 (func stores) ─▶ A3 (program computes + fixtures migrate) ─▶ A4 (nested rule + bogus-in-body test) ─▶ B (emit + cc test) ─▶ docs
```

`Tests/TestCostSpec.lean` migrates (computed lists; bogus-in-body negative
test); `Tests/TestComplexity.lean` untouched; new `Tests/TestEmit.lean`.
`doc/stdlib_friction.md` appends F13+ (traversal friction, emit friction).

## 7. Acceptance criteria

1. `lake build` clean; `rg -n "sorry|axiom|admit" LeanC Tests` empty for new
   code; `./.lake/build/bin/test` exits `0`.
2. No `CProgram.callSites` field remains (`rg "callSites"` shows only
   `programCallsComputed`/`programNestedComputed` + computed defs); every
   `CFunc` in `Tests/` is built via `mkFuncWithBody`/`mkLeafFunc` (no bare
   non-leaf literals).
3. Bogus-in-body negative test: a func whose body contains
   `mkCallWithRaw "bogus" …` (registered registry lacks `"bogus"`) yields
   `¬ ProgramCallsResolve` and `¬ ProgramMeetsSpec` over the **computed**
   lists (omission trick impossible — there is no list to omit from).
4. Nested rule: a program lifting `nestedRaw`-shaped args to `CExpr` either
   fails `ProgramNestedResolve` without `"f"` registered, or the agent
   documents the first-order-only restriction and `nestedRaw` stays a
   `rawMemBound`-only example. Either way the choice is recorded in code +
   friction log.
5. Thin slice: `Tests/TestEmit.lean` emits from a `ProgramMeetsSpec` proof
   (emitter signature demands the proof, D11), `cc` compiles the file, the
   binary exits `0`.
6. This file + `doc/roadmap.md` §7/§9-M2 status agree (thin status line only).

## 8. Archive — finished work (context only, not executable)

P0 Cost/Call/Loop (landed 2026-09-28; executable record was §§1–7 above
before this rewrite): `CExpr.call : fname + argStrs + argTime/argMem +
declaredTime/declaredMem : Nat → Nat` + `RawExpr`/`mkCallWithRaw`
(`LeanC/Expr.lean`); `exprTimeFn/MemFn` thread `n`
(`argTime n + declaredTime n + 1`, `max (argMem n) (declaredMem n)`) +
`exprTime/MemBound : Nat → Nat` + conditional `O1`
(`exprCallsO1Time/Mem`, `callTime/Mem_in_o1`, `o1_add_one/binop/tern`)
(`LeanC/CostSpec.lean`); parametric `CFunc` (`Nat → Nat` ×4, pointwise
`bodyLeDeclared`) + `mkFuncWithBody` measured tie (`LeanC/Func.lean`);
pointwise program worst + `CallSite`/`callSites`/`ProgramCallsResolve` +
`ProgramMeetsSpec.callsResolve` + `programFitsDevice` at `n`
(`LeanC/Program.lean`, `LeanC/Modules.lean`); `rawMemBound (.callRaw _ args)`
maxes over args (N3); `LeanC/Loop.lean` `forNTime/Mem` (sums/maxes) +
`forNDiag` linear (`poly 1` core, `linear` in `Tests`) + `0`-iter
`Zero`/`O1`; `UnboundedLoop.whileTrue` marker (no inhabitant, cites
`no_divergent_cost`); `recWithFuel` (`= forN`, fuel example `countdown`)
(`Tests/TestCostSpec.lean`, `Tests/TestComplexity.lean` green).
Frictions F11 (call parametricity: `O1` iff call specs `O1`, `fun _ => K`
wrappers, `programWorstTime : Nat → Nat` evaluated at `n`) and F12 (loop:
fixed-`iters` is `O1`, linear needs diagonal `iters = input size`; uniform
`K` bound; `Nat`-recursion `Aux` over `List.range`; `gpoly 1` vs `glinear`
via `pow_one`; `0` iters `Zero`-exact by `rfl`) in `doc/stdlib_friction.md`.

Literals + Expressions (landed 2026-09-27, fixes 1–5): `CLiteral` +
`litFitsType`/`emitLit` (`LeanC/Literals.lean`); `CVarRef` + `CExpr` stages
A→C + `emitExpr` (`LeanC/Variables.lean`, `LeanC/Expr.lean` with `RawExpr`/
`mkCallWithRaw`); `CAssign/CDecl/CReturn` markers (`LeanC/Stmt.lean`);
`CFunc{declared/body + bodyLeDeclared}` (`LeanC/Func.lean`),
`ProgramMeetsSpec`/`programFitsDevice` (`LeanC/Program.lean`); unified
`CostSpec` exact+`O1` + `lit/exprTime/MemBound` + `fitsBudget`/`poolCells`/
`tableMemCells` (`LeanC/CostSpec.lean`); stdlib pilot + F1–F10
(`Tests/TestLiteralsExpr.lean`, `doc/stdlib_friction.md`).
Executable record: `doc/implementation_plan_literals_expr.md` (HISTORICAL).

Complexity classes (2026-09-17, extended 2026-09-26, review fixes 2026-09-27):
local `BigO` kit; 7+7 base classes; `TagLE` (12 tags)/`QuantLE` with `Zero`
separated; `poly 0 = O1`; `StrictQuantBelow`; closed + open insertion +
`graphInclusion`; `O_linear` in `Examples/`; `CResource`/`HasCost` +
`costInClass` bridge (`Tests/TestComplexity.lean` green).
Design: `doc/complexity_proposal.md`; record:
`doc/implementation_plan.md` (HISTORICAL). Old paired
`ResourceBound`/`seqBound`/`branchBound`/`stmtBound` superseded by generic
`CResource` + bridge.

## 9. Open questions (not blockers)

- Traversal home: `Func.lean` vs new `LeanC/Calls.lean` (imports `Expr` only)
  — agent picks one place, records it; never in `Expr.lean` (would need
  `CallSite`, violating D8 if `CallSite` stays in `Program`).
- `CFunc` opacity: P1 keeps the structure open + documents "non-leaf ⇒
  `mkFuncWithBody`" discipline; full private-constructor opacity is future
  work (needs accessor + `WellFormed` design).
- `bodySrc : String` on `CFunc` (for B2) vs pre-rendered text passed to
  `emitFunc` as today — agent picks, records it.
- `cc` availability in CI for B3 (`cc` vs `gcc` name) — probe at test runtime,
  fail loudly if absent.
