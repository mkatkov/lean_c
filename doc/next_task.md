# Next Task — Resource-Aware Complexity Types

Status: implemented (2026-09-17).
Related: `doc/complexity_proposal.md` (detailed class design),
`doc/roadmap.md` §9 (Verification & Performance, M2 cost model),
`LeanC/Complexity.lean`, `LeanC/Context.lean`, `LeanC/Processes.lean`.

## 1. Goal

Introduce Lean-level complexity types that track worst-case resource
bounds of generated C programs along two axes:

1. **Time** — number of abstract operations / steps.
2. **Memory** — required allocation (bytes/cells, stack + heap high-water mark).

Complexity classes live in the program **context** (`CContext`), every
statement/process carries a bound, and the top-level program exposes a
machine-checkable **worst resource bound** theorem for the algorithm.

In short: `Context + Statement : ComplexityClass ⊢ worst-case (time, memory)`.

## 2. Why now

- Roadmap §9 M2 requires a cost model and a proven complexity bound for
  at least one small algorithm. There is currently no quantitative model,
  only the `CZeroComplexity < CTerminates < CUndecidable` stub chain in
  `LeanC/Complexity.lean:24-54` with `LE := False` and
  `can_insert_complexity_class_to_grapth := False` (`Complexity.lean:75-86`).
- `LeanC/Processes.lean:11` has an explicit
  `TODO: These classes and types should be linked to CComplexity`.
- Memory work in `LeanC/Arrays.lean` (static / fixed / dynamic blocks with
  `within_bounds`) needs a memory-bound counterpart: allocation size is
  tracked per-block, but there is no program-level memory ceiling.

## 3. Required base classes (time axis)

These five are the minimum vocabulary for this task (exact Lean names and
semantics are fixed in `doc/complexity_proposal.md`):

| Class | Meaning |
|---|---|
| `ZeroTimeComplexity` | Definition / empty computation. Zero operations. Bottom of the lattice. |
| `TimeComplexity_O1` | A single operation, or more generally a constant-bounded finite fragment. |
| `TimeComplexity_HALTS` | Terminates in a finite number of steps, but no named quantitative bound is claimed yet. |
| `TimeComplexity_UNBOUND` | Provably never stops (infinite execution, e.g. server loop / divergence). Known non-termination. |
| `TimeComplexity_UNDECIDABLE` | Unknown: we do not know whether the program terminates. Top / default for unanalysed code. |

Ordering intent: `Zero < O1 < HALTS < UNDECIDABLE`, with `UNBOUND < UNDECIDABLE`
but incomparable with `Zero/O1/HALTS`. All quantitative classes to be added
later (`log`, `poly`, …) sit between `O1` and `HALTS`.

A mirror memory axis (`ZeroMemory`, `O1Memory` / constant, `BOUNDED`,
`GROWING/UNBOUNDED`, `UNKNOWN`) is required by the same mechanism; see
proposal doc.

## 4. Extensibility requirement

There must be a **general insertion mechanism** for intermediate classes
such as `TimeComplexity_log`, `TimeComplexity_poly`, etc., without editing
the base lattice:

- A new class declares its immediate predecessors/successors and equalities
  (reuse/extend `CComplexityRelationship`).
- Insertion into `CComplexityGraph` is accepted only with a proof of
  acyclicity + preserved transitivity (`can_insert_…` must stop being `False`).
- The `CComplexityShadowNode` idea already sketched in
  `Complexity.lean:56-73` (rebuild/shadow the chain when an intermediate
  class splits an edge) should be either adopted or explicitly replaced —
  no silent second mechanism.

## 5. Scope

In scope:

- Base time classes (5 above) + base memory classes + relationship instances.
- Working `LE` / `<` over the graph (replace the `:= False` stubs).
- `can_insert` predicate + at least one示范 intermediate insertion
  (`O_log` or `O_poly`) proven correct.
- Threading of bounds through `CContext` and `CStatement` /
  `SequentialProcess` / `BinaryBranchingProcess` (composition rules:
  sequential time adds, branching takes max, memory takes high-water mark).
- One `Tests/` example proving a worst-case bound for a 2–3 statement
  sequential program.

Out of scope (follow-ups):

- Full Clight step-counting semantics and preservation-into-C proofs.
- Loop variants / decreasing-measure rules beyond a stub statement.
- Automatic inference of bounds; manual annotation + checking is enough.
- Benchmarking generated C vs Lean native (roadmap §9 acceptance, later).

## 6. Acceptance criteria

1. `LeanC/Complexity.lean` compiles with no `LE := False` /
   `can_insert := False` stubs for the base lattice.
2. Base time + memory classes exist with the ordering in §3 (checked by
   `example`/`theorem`, not comments).
3. At least one intermediate class (`log` or `poly`) is inserted via the
   general mechanism, with its position proof.
4. `CStatement` / processes expose a complexity bound drawn from the
   context; a `Tests/` program demonstrates sequential + branching
   composition and states its worst bound.
5. `lake build` clean; `doc/roadmap.md` §9 M2 marked in-progress with a
   pointer to this doc.

## 7. Suggested build order

1. Proposal review → freeze names/ordering (§3 + proposal doc §2–§3).
2. Base time lattice + relationships + `LE`.
3. Mirror memory lattice (reuse the same graph machinery).
4. `can_insert` + one intermediate class proof.
5. Context/process threading + composition lemmas.
6. `Tests/` worst-bound example.

## 8. Open questions (to resolve in proposal review)

- Is `O1` "exactly one op" or "≤ K ops for fixed K"? Proposal recommends the latter.
- Should time and memory share one `CComplexity` graph or two parallel
  graphs paired in context? Proposal recommends two graphs + paired bound.
- How are loops bounded (variant + invariant)? Stub only in this task.
