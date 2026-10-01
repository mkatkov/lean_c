# lean_c module responsibilities

## Tests and examples

The `Tests/` directory in this repository serves two purposes:

- Test-suite: exercises the code generator and other Lean modules to catch
	regressions and verify correctness.
- Usage examples: small, runnable Lean programs that demonstrate how to invoke
	the code generator and inspect the produced C output. These examples are
	intentionally executable and are run by the top-level `test.lean` runner.

When adding new functionality, prefer adding a small example under `Tests/`
that both documents intended use and acts as a regression test.

This document lists the primary Lean modules created to implement the roadmap and their responsibilities.

See `doc/roadmap.md` for the full project roadmap and milestones.
Current executable plan: `doc/next_task.md` (P1: traversal-closed
certificate + thin emission slice). Program-is-proof intent (§0 there):
emitters and budgets trust only `(p, ProgramMeetsSpec p)`, never a bare
`CProgram`; call sites are computed by traversal, not listed.

- `LeanC.Types` - C type representations, sizes/alignment helpers, struct layouts.
  Status: stub (inductives only, no target-parameterized layout proofs).
- `LeanC.Context` - Generic extended resource model: `CResource` combine ops,
  `HasCost` extraction, type-keyed `RStore` + `CContext` get/set, `DraftCtx` + pool.
- `LeanC.Complexity/*` - Cost classes: `BigO` kit, reps + growth facts
  (`Zero` separated from `BigO` — no `HasQuantRep` for `Zero`,
  pointwise `costInZero`; `poly 0 = O1` both axes, no-funext),
  `StrictQuantBelow` diagonals (`not_bigO_log_le_one/poly_le_one/sq_le_log`),
  `TagLE`/`QuantLE` order (12 closed base tags; memory `log`/`poly` open-only),
  closed `can_insert_complexity_class_to_graph` + open `can_insert_quant_to_list`
  (+ `mem_log/poly_inserted`), `graphInclusion` preorder,
  `costInClass`/`costInZero`/`IsFiniteCost` bridge (`Bridge`: finite promotion,
  divergence empty under total costs).
- `Examples/Resources` - Concrete resources (`TimeCost`/`MemCost`/`EnergyCost`/`ExactCount`)
  + `HasCost` instances + `BigO` preservation (`time_seq_preserves`, `mem_*`,
  `energy_*`, `exact_*`, `O1+O1=O1`, `costInZero` regression).
- `Examples/ComplexityLinear` - Open-extension demo (`O(n)` via `can_insert_quant_to_list`, zero base edits).
- `LeanC.Literals` - Intrinsically typed literals (`CLiteral`, range check, `emitLit`);
  costs assigned per resource via `setResource` (no hardwired bounds).
- `LeanC.Variables` - Typed de Bruijn references (`CVarRef`); P2 E3a
  scope-membership proof (`VarScope`, `DraftCtx` `idx < 2`; dangling without
  proof ill-typed).
- `LeanC.Expr` - Intrinsically typed expressions stages A→C (`CExpr`, `emitExpr`); `CNatIndex` pilot index.
  `RawExpr` untyped syntax + gated `mkCallWithRaw` (P2 E2 `rawArgsFirstOrder`
  hypothesis); P1-A adds `rawCallFnames` here (no `Func`/`Program` imports, D8).
  P2 E3: `ValidCast` (only legal casts), `lit` range proof
  (`litFitsType = true`), `field` membership (`HasStructField`).
- `LeanC.CostSpec` - Unified cost spec: canonical `StepCost`/`CellCost`,
  `CostSpec` exact+`O1`, `lit/exprTime/MemFn/Bound`, `exprCallsO1Time/Mem`
  gating, statement combinators (`assign/decl/returnTime/Mem`), device
  budgets (`DeviceSpec`/`fitsBudget`/`fitsProgramAt`, `poolCells`,
  `tableMemCells`).
- `LeanC.Loop` - Bounded loops (`forNTime/Mem`, `forNDiag` linear via
  `poly 1`), `UnboundedLoop.whileTrue` marker, `recWithFuel` sketch.
- `LeanC.Ops` - Per-operator singleton types + `IsCUnOp/IsCBinOp` emission
  and `UnOpSig/BinOpSig` typing rules.
- `LeanC.Arrays` - RAM/global-static/dynamic blocks, `CIndex`/`CArray`
  (`isAllocated`/`within_bounds`), `CArrayType` alternative formulation.
- `LeanC.Stmt` - Statement stubs (`CAssign/CDecl/CReturn` markers + emitters; `Stmt → Expr + Processes`, leaves stay leaves).
- `LeanC.Func` - Function as spec proof (P2 E1 inductive `CFunc`
  `withBody`/`leaf` + computed `def`s `fname/declared/body/calls/nested/bodySrc`,
  thin `mkFuncWithBody`/`mkLeafFunc` wrappers only). P1-A: `CallSite` lives here (D10),
  traversal (`exprCalls`/`exprNested`) lives here (or `LeanC/Calls.lean`).
- `LeanC.Modules` - Translation unit abstraction and symbol visibility (P2 E4
  True soundness field removed; `moduleAllBodiesFit` only).
- `LeanC.Program` - Top-level program **as proof of specs**: `CProgram` data +
  `ProgramMeetsSpec` certificate; P1-A removes `callSites` field, adds
  `programCallsComputed`/`programNestedComputed` + `ProgramNestedResolve`;
  P2 E4 True soundness field removed.
- `LeanC.Emit` (new in P1-B) - `emitProgram (p) (_ : ProgramMeetsSpec p) : String`:
  the proof is what gets emitted (D11); single-file C + `Tests/TestEmit.lean`
  `cc`-compile-and-run check. P2 E4 matches `CFunc` inductive directly
  (no empty-string test).
- `LeanC.Memory` - Memory model and operations used in proofs.
- `LeanC.Verification` - Proofs and correctness lemmas relating Lean and generated C.
- `LeanC.Bootstrap` - Lean-driven bootstrap probe generator. Emits a minimal C probe (or JSON manifest) describing target `sizeof`/`alignof` and related ABI facts. This module provides:
	- A small, parameterized serializer that emits only the minimal set of C declarations/statements required to probe the target ABI.
	- A proof scaffold that the generated probe's AST nodes belong to a project-local, whitelisted set of safe statements (e.g., printf and return), keeping the emitted probe small and verifiable.
	- Utilities to convert the probe output into a `lean_c_target.h` header consumed by the rest of the pipeline.
