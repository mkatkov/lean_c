# Implementation Plan — Literals + Expressions draft (unblocking stdlib)

Self-contained execution plan. An agent with only this document + a checkout
of the repo must be able to implement, build, and test its assigned task
without reading the design history.

Parent docs (background only): `doc/next_task.md` (summary + frozen
decisions D1–D4 + acceptance), `doc/roadmap.md` §2/§3/§9,
`doc/complexity_proposal.md`, `doc/implementation_plan.md` (historical
complexity plan — task numbers there are closed, do not reuse).
Status of that work: implemented (2026-09-17), `Tests/TestComplexity.lean` green.

## 0. Goal and scope

Give the cost model something to count: bound-carrying, intrinsically typed
literals + a useful expression subset, plus the thin stubs (`Stmt/Func/
Modules/Program/Memory/Verification`) needed to write the first `stdlib`
pilot and see where the design hurts.

In scope: `CLiteral` + `litBound` + range check + serializer; `CVarRef` +
`varBound`; context literal pool (`LiteralPoolEntry/poolBound/DraftCtx`);
`CExpr` stages A→B→C + `exprBound` + `emitExpr` + composition lemmas;
`assign/decl/return` statements; `Func/Modules/Program/Memory/Verification`
stubs; stdlib pilot; `Tests/TestLiteralsExpr.lean`; `doc/stdlib_friction.md`.

Out of scope: full Clight semantics, loop variants beyond `loopBoundStub`,
bound inference, Mathlib dependency, ABI/bootstrap changes, full
pretty-printer, benchmarks.

## 1. Repo facts (frozen at plan time)

- Toolchain: `lean-toolchain` = `leanprover/lean4:v4.23.0`.
- Deps: none (`lake-manifest.json`: `packages: []`). **Do not add any.**
- Done, do not redesign: `BigO` kit (`refl/trans/const_le_one/add/max_bound`),
  `gZero/g1/glog/gpoly/glinear`, `ResourceBound{timeRep,memRep : ℕ → ℕ}`
  (`LeanC/Context.lean`), `seqBound/branchBound` + `seq_bound_add/
  seq_bound_max_mem/branch_bound_max_time/branch_bound_max_mem`
  (`LeanC/Processes.lean`), `CArray.isAllocated/within_bounds`,
  `CGlobalStaticMemoryBlock/CArrayType` (`LeanC/Arrays.lean`),
  `IsCType/CTypeSize`, `CIntType/CCharType/CFloatType/CPointerType`
  (`LeanC/Types.lean`), `CVarScope` (`LeanC/Scopes.lean`),
  `CStatement{isStatementSound, stmtBound}`.
- To replace: `LeanC/Expr.lean` (9-line stub `litInt|var Name|add`);
  `LeanC/Variables.lean` (empty); `LeanC/Values.lean` stays as-is
  (read-only context, do not modify).
- Missing (to create): `LeanC/Literals.lean`, `LeanC/Func.lean`,
  `LeanC/Modules.lean`, `LeanC/Program.lean`, `LeanC/Memory.lean`,
  `LeanC/Verification.lean`, `Tests/TestLiteralsExpr.lean`,
  `doc/stdlib_friction.md`.
- Tests: `test.lean` runner (`runAll : List (IO UInt32) → IO UInt32`,
  `0` = success); pattern `Tests/TestMain.lean`, `Tests/TestComplexity.lean`.
- Commands (repo root): `lake build` (no `sorry`/`axiom` in new code);
  `./.lake/build/bin/test` (exit `0`);
  `rg -n "sorry|axiom|admit" LeanC Tests` (empty for new code).
- Import direction (no cycles):
  `Program → Modules → Func → Expr → Literals → Arrays/Types`;
  `Processes/Context` are leaves imported upward, never import `Expr/Func`.

## 2. Frozen interfaces (do not rename without updating all tasks)

```lean
-- T1 Literals (LeanC/Literals.lean, new; imports Types, TypeClasses, Arrays, Context, Complexity)
inductive CLiteral (α : Type u) [IsCType α] where
| intLit   : (sz : CIntSize) → (sgn : Bool) → (v : Int) → α = CIntType sz sgn → CLiteral α
| charLit  : (v : UInt8) → α = CCharType Unit → CLiteral α
| floatLit : (sz : CFloatSize) → (v : Float) → α = CFloatType sz → CLiteral α
| strLit   : (s : String) → α = CGlobalStaticMemoryBlock (CCharType Unit) s.length → CLiteral α
| tableLit : {β : Type u} → [IsPointedCType β] → (n : Nat) → (elems : Vector Int n)
    → α = CGlobalStaticMemoryBlock β n → CLiteral α
def litBound {α} [IsCType α] : CLiteral α → ResourceBound
def litFitsType {α} [IsCType α] : CLiteral α → Bool
def emitLit {α} [IsCType α] : CLiteral α → String
theorem lit_pure_O1_time {α} [IsCType α] (l : CLiteral α) : BigO (litBound l).timeRep g1
theorem lit_mem_O1 {α} [IsCType α] (l : CLiteral α) : BigO (litBound l).memRep g1

-- T2 Variables (LeanC/Variables.lean; imports Context, Complexity, Types)
inductive CVarRef (Γ : Type v) [CContext Γ] (α : Type u) [IsCType α] where
| mk : (idx : Nat) → CVarRef Γ α
def varBound : ResourceBound := { timeRep := g1, memRep := gZero }
theorem var_load_O1 : BigO varBound.timeRep g1

-- T3 Context pool (LeanC/Context.lean, additive only — keep extend)
structure LiteralPoolEntry where (ty : Type) [h : IsCType ty] (cells : Nat)
def poolBound : List LiteralPoolEntry → ResourceBound
structure DraftCtx where (scope : List Type) (pool : List LiteralPoolEntry) (acc : ResourceBound)
-- + instance : CContext DraftCtx, extendWithLit : DraftCtx → LiteralPoolEntry → DraftCtx

-- T4 Expressions (LeanC/Expr.lean rewrite; imports Literals, Variables, Arrays, Context, Complexity)
inductive CUnOp | neg | not | bnot
inductive CBinOp | add | sub | mul | div | mod | lt | le | eq | and | or | xor | shl | shr | land | lor
inductive CExpr (Γ : Type v) [CContext Γ] : (α : Type u) → [IsCType α] → Type _
| lit   {α} [IsCType α] : CLiteral α → CExpr Γ α
| var   {α} [IsCType α] : CVarRef Γ α → CExpr Γ α
| unop  {α} [IsCType α] : CUnOp → CExpr Γ α → CExpr Γ α
| binop {α} [IsCType α] : CBinOp → CExpr Γ α → CExpr Γ α → CExpr Γ α
| cast  {α β} [IsCType α] [IsCType β] : CExpr Γ α → (α → β → Prop) → CExpr Γ β
| deref {α} [IsCType α] [IsPointedCType α] : CExpr Γ (CPointerType α) → CArray.isAllocated _ → CExpr Γ α
| addr  {α} [IsCType α] : CVarRef Γ α → CExpr Γ (CPointerType α)
| index {α n} [IsPointedCType α] : CExpr Γ (CArrayType α n) → (ι : Type) → [CIndex ι] → CArray.within_bounds _ _ → CExpr Γ α
| field : struct access stub (idx + proof)
| tern  {α} [IsCType α] : CExpr Γ (CIntType .I32 true) → CExpr Γ α → CExpr Γ α → CExpr Γ α
| call  {α} [IsCType α] : (fname : String) → (args : List (Σ α, CExpr Γ α)) → (declaredCost : ResourceBound) → CExpr Γ α
def exprBound {Γ α} [CContext Γ] [IsCType α] : CExpr Γ α → ResourceBound
def emitExpr {Γ α} [CContext Γ] [IsCType α] : CExpr Γ α → String

-- T5 stubs (new files, ≤100 lines each)
-- LeanC/Func.lean: structure CFunc { sig : fname + args : CVarScope + ret; bodyBound declaredCost : ResourceBound }
--   theorem funcSound : BigO bodyBound.timeRep declaredCost.timeRep (per func)
-- LeanC/Modules.lean: translation unit = List funcs + pool : List LiteralPoolEntry
-- LeanC/Program.lean: main ref + pool + worst_bound : ResourceBound
-- LeanC/Memory.lean: re-export Arrays blocks (no new model)
-- LeanC/Verification.lean: preservation skeleton (exprBound soundness statement)
-- Processes additions: assign/decl/return CStatement stubs via seqBound exprBound

-- T7 test (Tests/TestLiteralsExpr.lean): def test : IO UInt32
```

Bound rules (§2 of next_task): unary/binary/cast/deref/addr/index/field =
sum of parts `+ 1` time / `max` memory; `tern` = guard sequenced before
`branchBound b₂ b₃`; `call` = args sum `+ declaredCost + 1` / `max`.
Prove via `BigO.add`, `BigO.max_bound`, `BigO.const_le_one`.

Conventions: `Prop` class fields; `O1` = `≤ K`; call takes
`declaredCost` (never imports `Func`).

## 3. Task DAG and agent assignment

```text
T1 (literals) ─┬─▶ T4 (exprs A→B→C) ─▶ T5 (stubs) ─▶ T6 (stdlib pilot) ─▶ T7 (tests) ─▶ T8 (docs)
T2 (vars) ─────┘     ▲
T3 (ctx pool) ───────┘
```

Suggested 3-agent split: A: T1+T2 | B: T3+T4 | C: T5+T6+T7+T8.
Total 3–5 focused days; per-task budgets below.

## 4. Tasks

### T1 — Literals — new `LeanC/Literals.lean` — 1d — no deps beyond §1

1. Define `CLiteral α` verbatim from §2 (5 ctors, equality proofs `rfl` at use).
2. `litBound`: `int/char/float → ({_=>0},{_=>0})`;
   `str/table → ({_=>1},{_=>cells})`, `cells = CTypeSize.size_of * len`
   (string: `1 * s.length`; table: `n * size_of β` — exact formula agent's
   choice, record in friction log).
3. Prove `lit_pure_O1_time` (`Zero ≤ O1` via `bigO_zero_le_one`, const case
   via `const_le_one`), `lit_mem_O1` (const case via `const_le_one`).
4. `litFitsType`: signed/unsigned range check on `v` for `intLit`
   (float/char/str/table: `true` + TODO comment). `emitLit`: `123u`,
   `'a'`, `"s"`, `{…}` C syntax.
5. `example`s: int fits, `strLit` bound `=O 1`, `tableLit` mem const.
Accept: builds; imports only §1 leaves; exact names of §2.

Pitfalls: intrinsically-typed equality proofs — keep them as trailing
`α = …` args so `rfl` discharges; do not put `IsCType` dictionaries on
`Vector` elements.

### T2 — Variables — `LeanC/Variables.lean` — 0.5d — needs T3 names only for Γ

1. Define `CVarRef Γ α` + `varBound` + `var_load_O1` (via `BigO.refl g1`).
2. Doc comment: name→`idx` uniqueness is producer duty (cf. `doc/ideas.md`);
   no scope-machinery change (`Scopes.CVarScope` untouched).
3. Stub `ofUInt` bridge toward `CVarIndex` (one-line, TODO for value link —
   `Arrays.lean` currently pins `value := 0`).
Accept: builds; `CVarRef` resolves from `Expr`.

### T3 — Context pool — `LeanC/Context.lean` — 0.5d — needs BigO names only

1. Keep `CContext.extend` untouched. Add `LiteralPoolEntry`, `poolBound`
   (time `{_=>0}`, mem `{_=> sum cells}` — plain `List.foldl`, still
   `=O 1` for fixed pools via `const_le_one`), `DraftCtx` + `CContext`
   instance (`extend` folds `acc` via `seqBound`; `extendWithLit` conses pool).
2. Document invariant: `acc` = `seqBound`-fold of prior bounds; `pool` =
   live global-statics; branching joins via `branchBound` (T4/T5).
Accept: builds; `DraftCtx`/`poolBound`/`extendWithLit` resolve from
`Expr/Tests`; `rg appendContext` still clean.

### T4 — Expressions — `LeanC/Expr.lean` — 1.5–2d — needs T1+T2+T3

Stage A (land first, must build alone): `lit/var/unop/binop/cast` +
`exprBound` + `emitExpr` + `example : BigO (exprBound (1+2)).timeRep g1`
(const folding). `cast` carries `(α → β → Prop)` proof (e.g. signed→unsigned
range), no semantics yet.
Stage B: `deref/addr/index/field` with proof args exactly as §2
(`isAllocated`/`within_bounds`). Time `+1`, memory `max`. Note known gap:
`CVarIndex.value` stub makes variable-index proofs weak — use `CConstIndex`
in examples, log the gap.
Stage C: `tern` as `seqBound guard (branchBound b₂ b₃)` (reuse
`branch_bound_max_time/mem`); `call` with `declaredCost` (`+1` dispatch).
Lemmas: `expr_time_add`, `expr_mem_max`, `tern_guard_still_O1`
(all via `BigO.add/max_bound/const_le_one`).
Accept: each stage builds; `emitExpr` covers all ctors; no import of `Func`.

### T5 — Thin stubs closing the loop — 1d — needs T4

1. `Processes.lean`: add `assign/decl/return` `CStatement` stubs
   (`stmtBound = seqBound exprBound …`), keep `nil/cons/branch` as-is.
2. Create `Func/Modules/Program/Memory/Verification` per §2 (≤100 lines
   each, `is…Sound := True` + real bound fields). `Func` discharges
   `call.declaredCost`: `funcSound : BigO bodyBound.timeRep
   declaredCost.timeRep` (per-func `example` suffices for draft).
3. Respect import direction in §1.
Accept: `Program.worst_bound : ResourceBound` resolves; no cycles
(`lake build` is the check).

### T6 — stdlib pilot — 1d — needs T5

In `Tests/` (or new `Stdlib/`, agent's choice — one place, record it):
`add2` (binop), `hello` (str literal in pool + `puts` call),
`aget` (guard `i < len` then `index` with `within_bounds`), `map_step`
(single-iteration body). Each exposes `worst_bound` + `BigO` membership
proof. Goal is friction, not coverage — every pain point goes into
`doc/stdlib_friction.md` (bulleted, with file:line).
Accept: 3–4 bounds compile; friction log exists.

### T7 — Tests — `Tests/TestLiteralsExpr.lean` + `test.lean` — 0.5d — needs T6

Follow `Tests/TestComplexity.lean` pattern (`example`s + `def test`):
pure lit `(0,0)`; str `(O1,O1)`; `exprBound (1+2) =O 1`; `tern` guard
`max+1 =O 1`; `call` sums `declaredCost`; pool test (hello pool has
1 entry, mem const); `aget` positive case with `CConstIndex` proof.
Wire `TestLiteralsExpr.test` into `test.lean:runAll` after
`TestComplexity.test`. Run `lake build && ./.lake/build/bin/test`.
Accept: 2-line `test.lean` diff; binary exits `0`; no unconditional `pure 0`.

### T8 — Docs + final review — 0.5d — needs T7 green

1. Write `doc/stdlib_friction.md` from T6 notes (numbered issues, each with
   proposed next step — this is the deliverable that justifies the draft).
2. `doc/roadmap.md` §§2–3: mark literal/expr M1 in-progress, link
   `doc/next_task.md` + this plan. `doc/modules.md`: add
   `Literals/Variables/Func/Modules/Program` one-liners.
3. Sweep: `lake build`, test binary, `rg sorry|axiom|admit`, confirm §2
   names match code (fix doc if code chose a documented-flexible formula).

## 5. Definition of done (global)

- [ ] `lake build` clean, no `sorry`/`axiom`/`admit` in `LeanC/`+`Tests/`.
- [ ] `./.lake/build/bin/test` exits `0` with new test wired in.
- [ ] §2 names present verbatim; `exprBound` lemmas compile; `aget`
  without proof does not typecheck (documented).
- [ ] `Func/Modules/Program` stubs present with `declaredCost` linkage.
- [ ] Stdlib pilot (3–4 bounds) + friction log present.
- [ ] T8 doc updates landed.

## 6. Risks and fallbacks

- Intrinsically-typed friction — fallback: keep typed core, add untyped
  `RawExpr` + `check : RawExpr → Option (Σ α, CExpr Γ α)` shim (T4 agent
  decides, logs it; never downgrade core).
- `call ↔ Func` cycle — frozen as `declaredCost`; if tempted to import
  `Func` from `Expr`, stop and re-read §1 direction.
- `CVarIndex.value := 0` stub — variable-index bounds stay weak; use
  `CConstIndex` in tests, log the gap for the memory-model task.
- `tableLit` element-type debate — T1 agent picks all-`Int` vector,
  records alternative in friction log; do not block on it.
- `O1`-time for allocating literals disputed — downgrade to `(Zero, O1)`
  is a one-line `litBound` change, lemmas survive via `bigO_zero_le_one`.
