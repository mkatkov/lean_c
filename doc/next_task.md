# Next Task — P0 Cost/Call/Loop soundness

Status: landed (2026-09-28). N1 parametric calls, N2 program certificate
(`ProgramCallsResolve` + `mkFuncWithBody`), N3 `RawExpr` mem fix, N4 bounded
loop with first `linear` (`LeanC/Loop.lean`), N5 `whileTrue` marker, N6 fuel
sketch — `lake build` + `./.lake/build/bin/test` green (see §7).
Previous Literals + Expressions task landed (see §8 archive); this file was
the executable plan (no separate implementation-plan doc for this task).
Related: `doc/roadmap.md` §§2–3, §9 M2; `doc/complexity_proposal.md`;
`LeanC/CostSpec.lean`, `LeanC/Expr.lean`, `LeanC/Func.lean`,
`LeanC/Program.lean`, `LeanC/Loop.lean`, `LeanC/Complexity/Bridge.lean`;
`doc/stdlib_friction.md` F1–F12.

## 1. Goal

Make `call` + program certificate + loops sound before any non-`O1` stdlib
work. Syntax today inhabits only `Zero`/`O1` (all exprs `O1` via
`const_le_one`; program worst is `foldl max` over closed `Nat`s). The first
`linear`/`poly` inhabitant comes from bounded loops in this task (N4).
Unbounded loops (N5) get a marker only — `IsDivergentCost` stays empty under
total costs. Recursion (N6) is a sketch (fuel/variant note), not semantics.

## 2. Why now

Critical review (2026-09-28) found three P0 blockers that loops would inherit:

- `CExpr.call` stores `Nat` bounds; `exprTimeFn/MemFn` do `fun _ => …`
  (`LeanC/CostSpec.lean:387,404`, `LeanC/Expr.lean:97-99`) — parametric
  arg/callee costs die at calls. Loops need `n`-threading through calls.
- `ProgramMeetsSpec` (`LeanC/Program.lean:56-60`) does not bundle
  `callResolves` (`LeanC/Program.lean:49-50`); `CFunc.bodyTime/Mem`
  (`LeanC/Func.lean:27-34`) are producer-supplied `Nat`s untied to
  `exprTimeBound`. “Unresolved call fails program proof” is discipline, not
  type enforcement.
- `rawMemBound (.callRaw _ _) = 0` (`LeanC/Expr.lean:134-135`) drops nested
  call high-water; `mkCallWithRaw` mem understates.

## 3. Frozen decisions (do not relitigate)

| # | Decision | Rationale |
|---|---|---|
| D1–D4 | Carried from Literals/Exprs task: pure vs allocating literal split; global-static + pool memory; staged expr subset A→C; intrinsically typed AST | See §8 archive; unchanged |
| D5 | `call ↔ Func` stays name-broken (`fname` seam, `Expr` never imports `Func`) | Import direction `Program → Func → Expr` preserved |
| D6 | `HALTS`/`BOUNDED` = `IsFiniteCost`, `UNBOUND`/`GROWING` = `IsDivergentCost` (empty under total `Nat → Nat`) | Matches `Bridge.lean`; N5 markers stay stipulated, no fake inhabitants |
| D7 | `O1` = `≤ K`, `Zero` pointwise `costInZero` (no `HasQuantRep` for `Zero`) | Bottom separation stays; `not_costInZero_time_one` regression |

Conventions (binding): `Prop`-valued class fields; no
`sorry`/`axiom`/`admit` in new code; `BigO` kit (`refl/trans/add/max_bound`)
only — no Mathlib dep.

## 4. Scope (P0 only: N1–N6)

- N1 parametric call: `CExpr.call` carries `argTime/argMem : Nat → Nat`
  (computed, not trusted) + `declaredTime/Mem : Nat → Nat`; `exprTimeFn/MemFn`
  thread `n` (`argTime n + declaredTime n + 1`, `max (argMem n) (declaredMem n)`);
  `mkCallWithRaw` computes fns from `List RawExpr`. Leaf/external calls migrate
  to `fun _ => K`. Files: `LeanC/Expr.lean`, `LeanC/CostSpec.lean`.
- N2 program certificate: extend `ProgramMeetsSpec` (or sibling
  `ProgramCallsResolve`) with per-call-site `callResolves` obligation over the
  program’s call-site list/registry match; tie `CFunc.bodyTime/Mem` to measured
  `exprTimeBound`/`exprMemBound` of the body (explicit `bodyExpr` or
  `body_eq_measured` proof, not free `Nat`s). Unresolved call ⇒ no proof.
  Files: `LeanC/Func.lean`, `LeanC/Program.lean`.
- N3 `RawExpr` mem fix: `rawMemBound (.callRaw f args)` = max over args
  (callee mem added at `CExpr` level via `declaredMem`); regression test nested
  `puts(f(x))` mem = max not 0. File: `LeanC/Expr.lean`.
- N4 bounded loop (first non-`O1`): counted-loop combinator
  (`forN iters body`, body `Nat → cost`); time = iterated sum with new
  `∑`-of-`=O` lemma (built on `BigO.add`/`const_le_one`), mem = max;
  deliver one `linear` inhabitant (`n` iters of `O1` body, `costInClass linear`)
  + `O1` case (`0` iters). Files: `LeanC/CostSpec.lean` (or new
  `LeanC/Loop.lean`), `Examples/Resources.lean` lemma reuse.
- N5 unbounded loop (marker): `whileTrue`/stub carrying
  `UNBOUND` time / `GROWING` mem marker; no cost-fn inhabitant (documents
  `no_divergent_cost` limit); tag edges stay stipulated. File: stub + doc note.
- N6 recursion sketch: self-`call` via registry + fuel parameter or
  well-founded variant note; bounded recursion reduces to N4, unbounded to N5.
  No fixpoint semantics. Doc + one `fuel` example only.

Out of scope: resource/pool/budget unification (`StepCost/TimeCost`,
`poolCells/programPoolCells`, `fitsProgramAt/programFitsDevice`); retiring weak
`can_insert_quant_to_list` alias; facade drift; `cellSize` threading; full
Clight semantics; benchmarks.

## 5. Frozen interfaces (agents build against these)

```lean
-- N1: call threads n (Expr.lean, CostSpec.lean)
-- CExpr.call fname argStrs (argTime argMem declaredTime declaredMem : Nat → Nat)
-- exprTimeFn (.call _ _ at _ dt _) n = at n + dt n + 1
-- exprMemFn  (.call _ _ _ am _ dm) n = Nat.max (am n) (dm n)
-- mkCallWithRaw fname args (dt dm : Nat → Nat) : CExpr Γ α  -- sums fns

-- N2: program is proof (Func.lean, Program.lean)
-- CFunc carries bodyMeasured : bodyTime = exprTimeBound bodyExpr (or equivalent)
-- ProgramMeetsSpec / ProgramCallsResolve : ∀ call-site ∈ prog, callResolves …

-- N4: bounded loop (Loop.lean or CostSpec.lean)
-- def forN (iters : Nat) (body : Nat → StepCost) : StepCost  -- time sums
-- theorem forN_linear : O1 body → costInClass linear (forN n body)
```

Bound rules: call time adds + dispatch `+1`, mem maxes; loop time iterates sum,
mem maxes. Reuse `BigO.add`/`max_bound`/`const_le_one`.

## 6. Build order

```text
N3 (rawMem fix) ─▶ N1 (parametric call) ─▶ N2 (program cert) ─▶ N4 (bounded loop + linear) ─▶ N5 (unbound marker) ─▶ N6 (recursion note) ─▶ tests/docs
```

`Tests/TestCostSpec.lean` extends (parametric call at two `n`s, nested-mem,
bounded-loop linear, unbounded marker); `Tests/TestComplexity.lean` untouched.
`doc/stdlib_friction.md` appends F11+ (call parametricity, loop friction).

## 7. Acceptance criteria

1. `lake build` clean; `rg -n "sorry|axiom|admit" LeanC Tests` empty for new
   code; `./.lake/build/bin/test` exits `0`.
2. `exprTimeFn (call …) n` varies with `n` (fails before N1: two `n`s give
   different bounds); leaf `puts` migrates to `fun _ => K`.
3. Unresolved-call program has no `ProgramMeetsSpec`/`ProgramCallsResolve`
   proof (negative test: bogus `fname` rejected).
4. Nested-call mem example: `rawMemBound (callRaw f [strLit …]) = len`, not `0`.
5. Bounded loop: `forN n O1-body` proves `costInClass linear`; `forN 0 _` is
   `O1`/`Zero`-exact as stated; N5 marker compiles with no cost inhabitant
   (cites `no_divergent_cost`).
6. This file + `doc/roadmap.md` §9-M2 status agree (thin status line only).

## 8. Archive — previous tasks

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

- Fuel (`Nat` bound, reduces to N4) vs well-founded variant for N6 — N6 agent
  picks, one example, records in friction log.
- `∑`-lemma shape for N4 (fold over `List.range` vs primitive recursion) —
  reuse `BigO.add` iteratively; no new axioms.
- `declared : Nat → Nat` migration cost for existing `putsSpec` fixtures —
  expect `fun _ => K` wrappers; fix tests, not core.
