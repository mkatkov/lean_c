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

- `LeanC.Types` - C type representations, sizes/alignment helpers, struct layouts.
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
- `LeanC.Variables` - Typed de Bruijn references (`CVarRef`); producer-owned name→`idx` uniqueness.
- `LeanC.Expr` - Intrinsically typed expressions stages A→C (`CExpr`, `emitExpr`); `CNatIndex` pilot index.
- `LeanC.Stmt` - Statement stubs (`CAssign/CDecl/CReturn` markers + emitters; `Stmt → Expr + Processes`, leaves stay leaves).
- `LeanC.Func` - Function representation, params, locals, and frame layout.
- `LeanC.Modules` - Translation unit abstraction and symbol visibility.
- `LeanC.Program` - Top-level program, linking, and pipeline orchestration.
- `LeanC.Memory` - Memory model and operations used in proofs.
- `LeanC.Verification` - Proofs and correctness lemmas relating Lean and generated C.
- `LeanC.Bootstrap` - Lean-driven bootstrap probe generator. Emits a minimal C probe (or JSON manifest) describing target `sizeof`/`alignof` and related ABI facts. This module provides:
	- A small, parameterized serializer that emits only the minimal set of C declarations/statements required to probe the target ABI.
	- A proof scaffold that the generated probe's AST nodes belong to a project-local, whitelisted set of safe statements (e.g., printf and return), keeping the emitted probe small and verifiable.
	- Utilities to convert the probe output into a `lean_c_target.h` header consumed by the rest of the pipeline.
