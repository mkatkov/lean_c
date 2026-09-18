# stdlib friction log — Literals + Expressions draft

Goal of this task was friction discovery, not coverage (`doc/next_task.md`
§1). Every place where typing, pool threading, or bound rules hurt is logged
below with a concrete next step. All items verified by `lake build` +
`./.lake/build/bin/test` green (2026-09-18).

## F1 — `tableLit` is all-`Int` (`Vector Int n`), not per-type
Where: `LeanC/Literals.lean:33` (`tableLit`), pilot `agetBase`
(`Tests/TestLiteralsExpr.lean:141`).
Pain: C `int` tables cover the pilot (`aget`/map-step), but float/char
tables would need one ctor per width (or a `Σ`-typed element vector).
Choice: frozen fallback from the plan (all-`Int`), per-type vectors
explicitly deferred.
Next: when stdlib needs a float table, add `tableLitFloat` (or
`{β} → Vector β n` with `[IsCType β] [CTypeSize β]` + `Inhabited β` for
`size_of`) and compare elaboration pain; keep one ctor until then.

## F2 — `litBound` cells ignore `CTypeSize.size_of`
Where: `LeanC/Literals.lean:41` (`litBound`), doc comment on size factor.
Pain: `CTypeSize.size_of : α → Nat` needs a *value*, but `strLit`/`tableLit`
only have types (`β : Type`) + lengths. `cells = s.length` / `n` assumes
width 1.
Choice: `cells = len` (bytes == cells for char; `n` cells for tables).
Next: thread an explicit `cellSize : Nat` (or `Inhabited β` witness) into
`tableLit`/`LiteralPoolEntry` when codegen needs byte counts; `poolBound`
(`LeanC/Context.lean`) already sums `cells`, so only the producer changes.

## F3 — `index` uses `CGlobalStaticMemoryBlock`, not `CArrayType`
Where: `LeanC/Expr.lean:75` (`CExpr.index`), `LeanC/Arrays.lean:234`
(`CArrayType α : (n : Type (u+1)) → Type (u+1)`).
Pain: `CArrayType` needs `n : Type (u+1)` (e.g. `CConstIndex i : Type 1`),
forcing `CExpr`'s `α` into `Type 1` for *all* ctors and breaking `lit`
(`CLiteral : Type 0`, `LeanC/Literals.lean:28`). `CGlobalStaticMemoryBlock
β n` (`n : Nat`, `Type 0`) stays in `Type 0`.
Choice: draft `index` is over `GlobalStatic` only; `CArrayType` (dynamic /
fixed-size blocks) untouched.
Next: universe-polymorphic `CExpr` (`α : Type u`) with `ULift`-ed literals,
or a separate `CArrayExpr` family for dynamic blocks, when the memory-model
task lands. Do not special-case further statics until then.

## F4 — `CConstIndex` cannot be used with `within_bounds`
Where: `LeanC/Arrays.lean:117` (`within_bounds (β : Type)`), `:126`
(`CConstIndex : Type (u+1)`), replacement `LeanC/Expr.lean:63`
(`CNatIndex : Type`).
Pain: all shipped indices (`CConstIndex`/`CFixedIndex`/`CVarIndex`) live in
`Type (u+1)`, but `within_bounds` expects `β : Type 0` — so no shipped
index inhabits an access proof (verified: `CConstIndex 2` fails to
elaborate against `within_bounds (GlobalStatic …)`, `Type 0 ≠ Type 1`).
`CVarIndex.value := 0` (`LeanC/Arrays.lean:158`) additionally makes
variable-index proofs weak by construction.
Choice: pilot defines `CNatIndex (v : Nat) : Type` (`Type 0`,
`value := v`) and proves `2 < 4` by `decide`
(`Tests/TestLiteralsExpr.lean:153`); spec's "`CConstIndex` in examples" is
deviated here and documented.
Next: make `within_bounds` universe-polymorphic (`{w} (β : Type w)`)
and update the six `CArray` instances, *or* move all indices to `Type 0`.
Keep `CConstIndex` as the `CArrayType` *size descriptor* (where `Type 1`
is required) regardless.

## F5 — `call` takes pre-folded `argsBound + argStrs`, not typed args
Where: spec `List (Σ α, CExpr Γ α)` → `LeanC/Expr.lean:75`
(`CExpr.call`: `fname + argStrs + argsBound + declaredCost`).
Pain (two layers):
1. `Sigma` nesting with local `Γ` is kernel-rejected ("nested inductive
   parameters cannot contain local variables").
2. A mutual `CExpr`/`CExprArg` wrapper (`List (CExprArg Γ)`) typechecks for
   the inductive but breaks dependent elimination for `deref`/`index`
   (input index computed from result index — supported singly, not
   mutually; verified by minimised prototypes).
Choice: `call` stores `argStrs : List String` (for `emitExpr`) +
`argsBound : ResourceBound` (pre-folded by the caller via `exprBound`).
Pilot folds correctly by construction (`helloArgsBound :=
litBound helloLit`, `Tests/TestLiteralsExpr.lean:119`); the link is not
machine-checked.
Next: restore typed args via the plan's fallback — untyped `RawExpr` +
`check : RawExpr → Option (Σ α, CExpr Γ α)` shim, or `ULift`-ed mutual
encoding — when call-site checking (arity/type mismatch) is needed. Never
import `Func` from `Expr` to fix this (`declaredCost` stays the seam).

## F6 — GADT ctors need equality proofs (`deref`/`index`/`addr`)
Where: `LeanC/Expr.lean:75` (`deref`/`index`/`addr` carry `γ = …` proofs,
like `CLiteral`), `@`-patterns in `exprBound`/`emitExpr`
(`LeanC/Expr.lean:101`, `:161`).
Pain: with ≥3 GADT ctors, even innocent dot-patterns (`.deref e _`)
fail dependent elimination ("motive … mismatch") — verified: `lit+deref`
alone works, adding `cast` *or* `addr` breaks `deref`. Implicit/instance
args (`{β}`, `[IsCType β]`, `[CIndex ι]`) additionally trigger "stuck
metavariable" synthesis when wildcarded (`_`) instead of `@`-bound.
Choice: all computed-index ctors use existential inputs + equality proofs
(`deref: γ → γ = Pointer β`; `index: γ → γ = GlobalStatic β n`;
`addr: β = Pointer α`), matched with `@CExpr.… _ …` patterns.
Next: keep this pattern for every future GADT ctor (pointer arithmetic,
struct field base); add a "new ctor" checklist to `doc/modules.md` if a
third contributor hits it.

## F7 — `assign/decl/return` live in `Stmt`, not `Processes`
Where: `LeanC/Stmt.lean:34` (`assignBound`/`declBound`/`returnBound`),
`LeanC/Processes.lean:29` (`CStatement`, untouched).
Pain: plan asked for `Processes.lean` additions, but `Expr → Processes`
(`seqBound`/`branchBound` in `tern`/`call`, `LeanC/Expr.lean:101`) forbids
`Processes → Expr` (cycle). `Context ↔ Processes` would also cycle if
`DraftCtx.extend` imported `seqBound` (inlined instead,
`LeanC/Context.lean` instance).
Choice: `Stmt → Expr + Processes`, `Func → Stmt`, leaves stay leaves.
`rg appendContext` still clean (no `appendContext` reintroduced).
Next: keep `Stmt` separate permanently; if statement lists must live in
`Processes` (`SequentialProcess`), move `seqBound`/`branchBound` down to
`Context` (leaf) so both can share without a cycle.

## F8 — `CStatement.stmtBound` is per-type, not per-value
Where: `LeanC/Stmt.lean:34` (value-level `assignBound` etc. vs marker
`CAssign`/`CDecl`/`CReturn` with `O(1)` instances).
Pain: `stmtBound` belongs to the *type* (`CStatement Γ α`), but
`assign rhs`'s cost depends on the *value* `rhs` (`seqBound (exprBound
rhs) …`). A faithful instance needs the bound (or the expr) in the type.
Choice: draft split — value functions for the pilot's `worst_bound`,
marker types (`isStatementSound := True`, `(O1,O1)`) as `SequentialProcess`
placeholders.
Next: value-dependent statements via `Σ (b : ResourceBound), …` payloads
in the statement type, or move bounds out of the class (parallel
`stmtBoundOf : stmt → ResourceBound` function). Required before loop
variants (which need per-iteration bounds).

## F9 — `deref` is practically unusable (generic pointer `isAllocated = False`)
Where: `LeanC/Arrays.lean:119` (`CArray (CPointerType α)`: `isAllocated :=
False`), `LeanC/Expr.lean:75` (`deref` needs that proof).
Pain: the only shipped `CPointerType` instance claims `False`, so no
closed `deref` term typechecks without a local override instance. Definition
compiles (good — the ctor exists for the loop), but the pilot avoids `deref`
entirely (uses `index` over `GlobalStatic`, whose instance is `True`).
Next: model allocated pointers (e.g. coerce `CFixedSizeMemoryBlock` /
`CDynamicMemoryBlock` to pointers with `True` instances, or add an
`AllocatedPointer` wrapper) in the memory-model task. Keep the proof
requirement (no proof = no term) — only the instance changes.

## F10 — small additive fixes (no design impact)
- `CCharType` had `CTypeSize` but no `IsCType` (`LeanC/Types.lean`) —
  added generic `instance {α} : IsCType (CCharType α)`; `IsPointedCType`
  follows. Required by `charLit`/`strLit`.
- Stdlib location: pilot lives in `Tests/TestLiteralsExpr.lean` (not a new
  `Stdlib/` dir) to keep the import closure (`test.lean` +2 lines) small.
  Split to `Stdlib/` when the pilot exceeds ~4 funcs.
- `emitFunc` is single-line (`"void f(void) { body }"`) — `s!"…{{\n…}}"`
  brace/newline escaping proved more annoying than useful for the draft.
