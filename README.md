# lean_c

the goal of the project is to be able to produce C code from lean4 code with some guarantees. There are two main advantages to this:
 1. C code is portable and can be compiled on many platforms, moreover the final binaries can be very small
 2. C code can be proven to have certain properties, like runtime guarantees, memory safety, etc. using lean proofs.

# Current status
The project is in its early stage. 
we initially implement minimal subset of https://xavierleroy.org/publi/Clight.pdf or C99 that can produce running C program.

## Project layout

Top-level directories and files added for the roadmap implementation:

- `LeanC/` - Lean source modules (Types, Literals, Expr, Stmt, Func, Modules, Program, Memory, Verification).
 - `doc/modules.md` - Responsibilities of each Lean module.
 - `doc/roadmap.md` - Project roadmap and high-level milestones.
 - `doc/ideas.md` - Current understanding 

## Testing and examples

This project uses a small Lean-based test runner rather than a conventional
unit test framework. The top-level `test.lean` file imports modules under the
`Tests/` directory and executes their `test : IO UInt32` entrypoints. Each
test `test` should return `0` on success and a non-zero exit code on
failure.

The `Tests/` directory has a dual purpose:
- Project test-suite: exercises internal code paths, checks correctness of the
	code generator, and ensures changes don't regress behavior.
- Usage examples / documentation: each test is a small, runnable example that
	demonstrates how to use the package to generate C snippets.

How to add a test/example:

- Create `Tests/MyExample.lean` with `def test : IO UInt32 := ...`.
- Add `import Tests.MyExample` to `test.lean` and include `MyExample.test` in
	the `runAll` list.

Running the test runner:

1. Build the project:

```bash
lake build
```

2. Run the compiled test binary (built from `test.lean`):

```bash
./.lake/build/bin/test
```

The runner executes each example/test in sequence and returns a non-zero
exit code on the first failure. Tests are intentionally small programs that
can generate C code for manual inspection or be extended with automated
checks (for example, writing the generated C to a temporary file and invoking
`gcc`/`clang` as part of the test).
