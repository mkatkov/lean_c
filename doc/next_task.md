# Next Task — Literals + Expressions draft (unblocking stdlib)

Status: planned (2026-09-18).
Executable plan: `doc/implementation_plan_literals_expr.md` (start there — it is
self-contained; this file is the summary + decisions + acceptance).
Related: `doc/roadmap.md` §2 (Literals), §3 (Expressions), §9 M2 (cost model);
`doc/complexity_proposal.md`, `LeanC/Complexity.lean`, `LeanC/Context.lean`,
`LeanC/Processes.lean`; previous task archived in §8 below.

## 1. Goal

Fill the `Literals` + `Expressions` drafts with bound-carrying, intrinsically
typed ASTs so `stdlib` experiments can start and expose design weak points.

In short: `Literals : Context → Context'` (produce pool entries),
`Exprs : (Context, bound) → bound'` (thread + update `(timeRep, memRep)`).

A closed `stdlib` pilot (`add2`, hello-world string, guarded `aget`,
one `map`-step body) must compile, emit C sketches, and state its
`(time, memory)` worst bound via the existing `BigO` kit. The point of this
task is friction discovery, not coverage — every place where typing,
pool threading, or bound rules hurt gets logged in `doc/stdlib_friction.md`.

## 2. Why now

- Complexity draft is done (base lattices + `O_log`/`O_linear` + `seqBound`/
  `branchBound` + `Tests/TestComplexity.lean` green). The cost model has
  nothing to count yet.
- `LeanC/Expr.lean` is a 9-line stub (`litInt | var Name | add`) with no
  types, no bounds, no serializer. `LeanC/Variables.lean` is empty.
  `LeanC/Values.lean` has an unscoped `TypedValue` stub only.
- `LeanC/Literals.lean`, `LeanC/Func.lean`, `LeanC/Modules.lean`,
  `LeanC/Program.lean`, `LeanC/Memory.lean`, `LeanC/Verification.lean`
  do not exist. There is no path from a literal to a bound to a program,
  so `stdlib` cannot be written.

## 3. Frozen decisions (do not relitigate in this task)

| # | Decision | Rationale |
|---|---|---|
| D1 | Literals split by cost: pure values (`int/char/float/label`) are `(ZeroTime, ZeroMem)`; allocating literals (string/table, initialized static block requiring load) are `(O1Time, O1Mem)` with `mem = const cells` | Some literals are free values, some require load/allocation. Both still fold via `BigO.const_le_one`. Downgrade path: if codegen shows load is free, change to `(Zero, O1)` — one-line `litBound` change |
| D2 | Literal memory = global-static + `O1Mem` | String/table literals lower to `CGlobalStaticMemoryBlock` / `CArrayType.global_static` (`LeanC/Arrays.lean`); context carries a literal pool (list of live static objects) — the "extra slot in references of memory objects" |
| D3 | Expression subset = pure arith **+ memory access + ternary + calls**, staged A→C | A: `lit/var/unop/binop/cast`; B: `deref/addr/index/field` (with `isAllocated`/`within_bounds` proof args); C: `tern` (guard + `branchBound`) + `call` (with `declaredCost`). A must land first so a buildable prefix always exists |
| D4 | Intrinsically typed AST | `CLiteral α [IsCType α]`, `CExpr Γ α [IsCType α]`. Heavier constructors, but serializer + checker collapse into one. Fallback if blocked: keep typed core, add untyped `RawExpr` + `check` shim (never downgrade core) |

Conventions (inherited, still binding): `Prop`-valued class fields; no
`sorry`/`axiom`/`admit` in new code; `O1` means `≤ K` (`O(1)`), never
"exactly 1 step"; `call ↔ Func` cycle is broken by `declaredCost`
(`call` takes `fname + declaredCost`, not a `Func` body — `Func` later
discharges `bodyBound ≤ declaredCost`).

## 4. Scope

In scope:

- New `LeanC/Literals.lean`: `CLiteral α` (int/char/float/str/table), `litBound`,
  `litFitsType` (range check), `emitLit` (C syntax), `=O 1` lemmas.
- `LeanC/Variables.lean`: `CVarRef Γ α` (de Bruijn `idx`), `varBound = (g1, gZero)`,
  bridge stub to `CVarIndex`.
- `LeanC/Context.lean` (additive only): `LiteralPoolEntry`, `poolBound`,
  `DraftCtx` + `CContext` instance + `extendWithLit`.
- `LeanC/Expr.lean` rewrite: `CUnOp/CBinOp/CExpr` per §5, `exprBound`
  (time adds, memory maxes), `emitExpr`, composition lemmas via
  `BigO.add/max_bound/const_le_one`.
- Thin stubs closing the loop: `assign/decl/return` `CStatement`s,
  `LeanC/Func.lean` (signature + `bodyBound` + `declaredCost` check),
  `LeanC/Modules.lean`, `LeanC/Program.lean`, `LeanC/Memory.lean`
  (re-export `Arrays`), `LeanC/Verification.lean` (preservation skeleton).
- `Tests/TestLiteralsExpr.lean` + stdlib pilot + `doc/stdlib_friction.md`.

Out of scope (follow-ups):

- Full Clight semantics, loop variants beyond `loopBoundStub`, inference of
  bounds, Mathlib dependency, ABI/bootstrap changes, full C pretty-printer,
  benchmarks.

## 5. Frozen interfaces (exact names — agents build against these)

```lean
-- Literals (LeanC/Literals.lean, new)
inductive CLiteral (α : Type u) [IsCType α] where
| intLit   : (sz : CIntSize) → (sgn : Bool) → (v : Int) → α = CIntType sz sgn → CLiteral α
| charLit  : (v : UInt8) → α = CCharType Unit → CLiteral α
| floatLit : (sz : CFloatSize) → (v : Float) → α = CFloatType sz → CLiteral α
| strLit   : (s : String) → α = CGlobalStaticMemoryBlock (CCharType Unit) s.length → CLiteral α
| tableLit : {β : Type u} → [IsPointedCType β] → (n : Nat) → (elems : Vector Int n)
    → α = CGlobalStaticMemoryBlock β n → CLiteral α
def litBound {α} [IsCType α] : CLiteral α → ResourceBound
-- pure → ({_=>0},{_=>0}); str/table → ({_=>1},{_=>cells})
def litFitsType {α} [IsCType α] : CLiteral α → Bool
def emitLit {α} [IsCType α] : CLiteral α → String

-- Variables (LeanC/Variables.lean)
inductive CVarRef (Γ : Type v) [CContext Γ] (α : Type u) [IsCType α] where
| mk : (idx : Nat) → CVarRef Γ α
def varBound : ResourceBound := { timeRep := g1, memRep := gZero }

-- Context pool (LeanC/Context.lean, additive)
structure LiteralPoolEntry where (ty : Type) [h : IsCType ty] (cells : Nat)
def poolBound : List LiteralPoolEntry → ResourceBound
structure DraftCtx where (scope : List Type) (pool : List LiteralPoolEntry) (acc : ResourceBound)
-- + CContext DraftCtx instance, extendWithLit : DraftCtx → LiteralPoolEntry → DraftCtx

-- Expressions (LeanC/Expr.lean rewrite)
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
```

Bound rules: unary/binary/cast/deref/addr/index = sum of parts `+ 1` time,
`max` memory; `tern` = guard sequenced before `branchBound b₂ b₃`
(reuse `branch_bound_max_time/mem`); `call` = sum of args `+ declaredCost + 1`
time, `max` memory. Import direction (no cycles):
`Program → Modules → Func → Expr → Literals → Arrays/Types`.

## 6. Build order

```text
T1 (literals) ─┬─▶ T4 (exprs A→B→C) ─▶ T5 (stmt/func/mod/prog stubs) ─▶ T6 (stdlib pilot) ─▶ T7 (tests) ─▶ T8 (docs)
T2 (vars) ─────┘     ▲
T3 (ctx pool) ───────┘
```

Detail per task: see `doc/implementation_plan_literals_expr.md` T1–T8.
Suggested split: A: T1+T2 | B: T3+T4 | C: T5+T6+T7+T8. Total 3–5 focused days.

## 7. Acceptance criteria

1. `lake build` clean; `rg -n "sorry|axiom|admit" LeanC Tests` empty for new code;
   `./.lake/build/bin/test` exits `0` with `TestLiteralsExpr.test` in `runAll`.
2. Pure literals `(0,0)`, allocating literals `(O1,O1)` with `BigO` lemmas;
   context pool present (`extendWithLit`, `poolBound`).
3. Expression stages A–C present with `exprBound` lemmas (`1+2 =O 1`,
   `tern` guard `max+1 =O 1`, `call` sums `declaredCost`); `aget` requires a
   `within_bounds` proof (no proof = no term).
4. `Func/Modules/Program` stubs present with `call.declaredCost` linkage
   (`funcSound : BigO bodyBound declaredCost`); stdlib pilot (3–4 bounds)
   present; `doc/stdlib_friction.md` logs every friction point.
5. This file + executable plan + `doc/roadmap.md` §§2–3 statuses agree
   (fix doc if code chose a documented-flexible name).

## 8. Archive — previous task (complexity classes)

Status: implemented (2026-09-17). Design: `doc/complexity_proposal.md`;
execution record: `doc/implementation_plan.md` (historical — T1–T9 done,
do not reuse its task numbers for the new work).
Delivered: local `BigO` kit, 5+5 base classes over two axes, `TagLE`/
`QuantLE` ordering, `can_insert` + `O_log` insertion, `O_linear` extension
in its own file, `ResourceBound` + `seqBound`/`branchBound` + lemmas,
`Tests/TestComplexity.lean` green. Old open questions (O1 = `≤ K`,
two graphs + paired bound, loop stub) are resolved as stated there.

## 9. Open questions (for this task, not blockers)

- Exact `C` surface for `tableLit` elements (all-`Int` vector vs per-type
  vectors) — T1 agent picks, records in friction log.
- `field` shape (index-based vs name-based) — stub only in this task.
- `RawExpr` escape hatch: add only if T6 is actually blocked (do not pre-build).
