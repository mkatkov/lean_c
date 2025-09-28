# lean_c roadmap

This document outlines a roadmap for implementing a Lean4-to-C pipeline targeting a subset of the Clight language (CompCert's simplified C), focusing on constructs necessary to generate provably-correct and performant C code.

Goals
- Produce C code from Lean4 definitions for a well-defined subset of C/Clight.
- Encode semantics in Lean to support machine-checked proofs about the generated C (safety, functional correctness, performance bounds).
- Produce minimal, portable, and verifiable C binaries.

Roadmap sections

## 1. Types
Goal: Implement a C type system subset mirroring Clight's types and map Lean types to C types.

Scope
- Integer types: signed/unsigned char, short, int, long (with bitwidths configurable).
- Floating point: float, double (optional initial scope: none or limited).
- Pointers and arrays.
- Structs and unions (initially structs only).
- Function types (signatures).
- Void type.

Acceptance criteria
- A Lean representation of C types with Decidable equality and size/alignment metadata.
- Mapping functions: lean_type -> c_type and back for supported types.
- Size and alignment functions matching target ABI (configurable per target triple).
- Unit tests in Lean showing correct size/alignment for a selected target (e.g., x86_64 SysV).

Proof obligations
- Correctness of size/alignment computations relative to the configured ABI.
- Type-safety properties used by later code generation and proofs.

Milestones
- M1: Define the core `ctype` inductive in Lean.
- M2: Implement size_of/alignment_of tunable by target.
- M3: Add struct layout and padding rules.

### Bootstrap header generation
Goal: Because C primitive sizes (e.g., `int`, `short`, `long`) are target- and implementation-defined, introduce a bootstrap step that emits a target-specific header file with explicit size and alignment guarantees used by the generator.

Scope
- A small C bootstrap program (or Lean runtime probe) compiled for the target that prints or writes a machine-readable manifest (e.g., JSON or C header) containing:
	- sizeof for primitive C types (char, short, int, long, long long, float, double, pointers)
	- alignof for the same types
	- endianness and pointer width
	- ABI-specific calling-convention hints (if detectable)
- A generated `.h` header (e.g., `lean_c_target.h`) with fixed-width typedefs and macros mapping Lean-level ctype descriptors to concrete C typedefs (e.g., `typedef int32_t lean_c_int;`) and compile-time static_asserts when supported.

Acceptance criteria
- A runnable bootstrap that produces a `lean_c_target.h` for the build target.
- The generator uses `lean_c_target.h` to emit consistent, portable C code (no assumptions about `int` size).
- The header contains static assertions/checks to fail early on ABI mismatches.

Proof obligations
- The emitted header correctly reflects the target ABI used to compile the rest of the generated code.

Milestones (continued)
- M4: Implement bootstrap probe program and header generator.
- M5: Integrate header into the codegen pipeline and add tests that fail when sizes/alignments mismatch.

### Lean-driven minimal bootstrap
Goal: Reduce build-time churn by generating the bootstrap probe from Lean itself when possible. Instead of shipping a fixed `bootstrap_probe.c`, a Lean emitter can produce the tiny C source or JSON manifest required to probe the target ABI. Being generated inside Lean enables small, syntactic proofs that the probe is restricted to a whitelisted set of safe statements (e.g., includes, `printf`, `puts`, and `return`).

Scope
- A Lean module `LeanC.Bootstrap` that emits either a tiny C probe or a JSON manifest. The emitted source should be intentionally minimal.
- A whitelist of AST statement constructors permitted in the probe. The Lean module contains a proof skeleton showing that the generator only uses these constructors.

Acceptance criteria
- `LeanC/Bootstrap.lean` exists and can produce a probe manifest or C source for the target.
- There is a Lean-level lemma asserting that the generated probe AST is a member of the whitelist (syntactic guarantee only).

Milestones (continued)
- M4: Implement `LeanC.Bootstrap` with a minimal emitter and proof skeleton.
- M5: Add end-to-end test using the Lean emitter to produce `lean_c_target.h` consumed by the pipeline.

## 2. Literals
Goal: Represent C literals in Lean and ensure correct encoding and value ranges.

Scope
- Integer literals (decimal, hex) with variants for suffixes (u, l, ul, ll, etc.) mapped to the integer types in the Types section.
- Character literals.
- String literals (as global arrays of char or pointer to char depending on model).
- Floating-point literals.

Acceptance criteria
- Lean AST node definitions for literals.
- Pretty-printer/serializer to produce valid C literal syntax.
- Validation checks that literals fit target types.

Proof obligations
- Literal evaluation semantics matches the intended C semantics (representation, truncation, signedness, range).

Milestones
- M1: Integer and char literal representation and serializer.
- M2: String literal layout in generated C (null-termination, storage class).

## 3. Expressions
Goal: Implement expression AST and semantics corresponding to Clight expressions.

Scope
- Unary and binary operators (arithmetic, bitwise, logical, relational).
- Casts and conversions (explicit and implicit where needed).
- Pointer arithmetic and dereference.
- Struct field access.
- Conditional (?:) expressions.
- Function calls as expressions (where they return a value).

Acceptance criteria
- Lean AST for expressions (variant constructors matching Clight where useful).
- Type-checker / expression validator that enforces type rules and inserts casts when necessary.
- Serializer to C source compatible with the types and literals modules.

Proof obligations
- Preservation: expressions preserve typing under evaluation.
- Determinism and definedness of expression evaluation in the presence of casts and pointer arithmetic.

Milestones
- M1: Define expression AST and type-checker.
- M2: Implement code generation for pure arithmetic expressions and pointer arithmetic.
- M3: Add expression-level proofs (preservation, bounds for pointer arithmetic where needed).

## 4. Statements
Goal: Implement statement AST and semantics for Clight-style statements and control flow.

Scope
- Expression statements (e.g., a = b;)
- Sequencing, block scope, local variable declarations.
- Conditionals (if/else), loops (for, while as a later extension), and break/continue.
- Return statements and function-level control flow.
- Switch can be deferred to future iterations.

Acceptance criteria
- Lean AST for statements with scoping information.
- Well-formedness checker that validates variable visibility and stack layout.
- Code generator that emits structured C for statements.

Proof obligations
- Control-flow correctness: returns only where allowed, breaks/continues behave correctly.
- Stack safety: local variables have non-overlapping storage and correct lifetime.

Milestones
- M1: Statement AST and simple code generation.
- M2: Block scoping and local variable allocation.
- M3: Loop semantics with invariants for verification.

## 5. Functions
Goal: Define functions, calling conventions, parameter passing, and return values aligned with Clight/CompCert model.

Scope
- Function definitions with parameter lists and return types.
- Calling conventions: pass-by-value for scalars, pointers for aggregates (or ABI-defined behavior).
- Local variable allocation and stack frame layout.
- External function declarations (to call libc or runtime helpers).

Acceptance criteria
- Lean representation of functions including body, params, locals.
- Generator emits C functions with correct prototypes and calling conventions for the target.
- A small runtime to support startup (main) and I/O if necessary.

Proof obligations
- Calling convention correctness: parameter/result mapping, caller/callee saved registers (as abstract model for proofs).
- Stack layout correctness and absence of memory overlap.

Milestones
- M1: Function AST and code gen for simple functions.
- M2: Stack frame layout and parameter passing model.
- M3: External function bindings and runtime stubs.

## 6. Modules
Goal: Provide a modular compilation model: multiple translation units, symbol visibility, and linking model.

Scope
- Translation units (files) with global declarations (functions, global variables).
- Internal (static) vs external linkage.
- Name mangling and symbol table management.
- Basic linker model for combining generated units.

Acceptance criteria
- Ability to generate multiple C files with well-scoped symbols.
- Symbol table representation in Lean for linking and visibility checks.

Proof obligations
- Linking preserves semantics: combining units doesn't change per-unit correctness assumptions.

Milestones
- M1: Support multiple translation units and global declarations.
- M2: Implement static vs external linkage semantics.

## 7. Program
Goal: The top-level program representation tying modules, entry point, and runtime configuration together.

Scope
- Program AST consisting of modules, global initializers, and a designated `main`.
- Build configuration (target ABI, optimization/passes, memory model parameters).
- End-to-end generator from a Lean 'program' to a set of C source files ready to compile.

Acceptance criteria
- A `compile` pipeline: Lean program -> C source files -> compiled binary (via system compiler).
- Verification pipeline hooks: ability to attach Lean proofs to generated code artifacts.

Proof obligations
- Preservation of functional behavior across the translation pipeline.
- Performance properties (e.g., complexity bounds) encoded and preserved or approximated.

Milestones
- M1: Minimal end-to-end pipeline that generates and compiles a small program.
- M2: Integration with Lean proofs: map source-level proofs to preserved C behaviors.

## 8. Memory model & evaluation semantics
Goal: Provide a memory model compatible with CompCert/Clight to reason about pointers, aliasing and I/O.

Scope
- Block-structured memory with allocation, load/store, pointer arithmetic semantics.
- Undefined behaviors and well-defined subset identification.
- Side effects and observable behavior model.

Acceptance criteria
- A Lean formalization of the memory model sufficient to prove simple programs correct.
- Generator's emitted code respects the model (e.g., no pointer arithmetic that violates block bounds).

Proof obligations
- Memory safety proofs for generated code.
- Proofs showing absence of undefined behaviors for well-typed Lean inputs.

Milestones
- M1: Define block memory model in Lean.
- M2: Prove simple program memory safety.

## 9. Verification & Performance targets
Goal: Define what properties we will prove and how to measure performance.

Key properties
- Functional correctness: generated C implements the Lean function semantics.
- Memory safety: no invalid reads/writes.
- Stack safety: no stack overflows in verified limits.
- Complexity/performance: bound on time/space where feasible.

Approach
- Encode translation correctness as a theorem: for each Lean function f, the generated C program's semantics refines f's semantics.
- Use CompCert/Clight-style small-step or big-step semantics to relate behaviors.
- For performance, annotate algorithms with cost models (e.g., step counts) and prove bounds are preserved up to a constant factor.

Acceptance criteria
- Example proofs: correctness for a few data-structure operations and a small algorithm (e.g., list map, fold).
- Measurable performance tests comparing generated C vs Lean native code for simple benchmarks.

Milestones
- M1: Prove correctness for a small function.
- M2: Add a cost model and prove a small algorithm's complexity.

## Extras and Notes
- Targeting Clight/CompCert semantics is highly beneficial: reuse proven semantics and style of proofs from CompCert papers and code.
- Start conservative: each stage implements a small, well-specified subset and expands.
- Keep ABIs and target parameters configurable.
- Provide examples and integration tests.

## Next steps
- Implement `ctype` and literal AST in Lean and write unit tests.
- Add a small example Lean function and implement the pipeline to emit C for it.
- Align Lean encodings with Clight definitions and start writing the first translation correctness lemma.




