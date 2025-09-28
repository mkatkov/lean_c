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
- `LeanC.Literals` - Literal AST nodes and serializers.
- `LeanC.Expr` - Expression AST and type-checking utilities.
- `LeanC.Stmt` - Statement AST and scoping/validation.
- `LeanC.Func` - Function representation, params, locals, and frame layout.
- `LeanC.Modules` - Translation unit abstraction and symbol visibility.
- `LeanC.Program` - Top-level program, linking, and pipeline orchestration.
- `LeanC.Memory` - Memory model and operations used in proofs.
- `LeanC.Verification` - Proofs and correctness lemmas relating Lean and generated C.
- `LeanC.Bootstrap` - Lean-driven bootstrap probe generator. Emits a minimal C probe (or JSON manifest) describing target `sizeof`/`alignof` and related ABI facts. This module provides:
	- A small, parameterized serializer that emits only the minimal set of C declarations/statements required to probe the target ABI.
	- A proof scaffold that the generated probe's AST nodes belong to a project-local, whitelisted set of safe statements (e.g., printf and return), keeping the emitted probe small and verifiable.
	- Utilities to convert the probe output into a `lean_c_target.h` header consumed by the rest of the pipeline.
