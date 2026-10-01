# stdlib friction log — Literals + Expressions draft

Goal of this task was friction discovery, not coverage (`doc/next_task.md`
§1). Every place where typing, pool threading, or bound rules hurt is logged
below with a concrete next step. All items verified by `lake build` +
`./.lake/build/bin/test` green (2026-09-18).

> Fixes 1–5 follow-up (2026-09-27, program-as-proof; `lake build` +
> `./.lake/build/bin/test` green): F2 CLOSED (`tableMemCells`/`litMemFnWithWidth`
> + `poolCells` + 4-limit `fitsBudget`/`fitsProgramAt`/`programFitsDevice` in
> `LeanC/CostSpec.lean` + `LeanC/Program.lean`); F5 CLOSED (`CExpr.call` now
> `fname + argStrs + argTime/argMem + declaredTime/declaredMem` with gated local
> `O1` + `RawExpr`/`mkCallWithRaw` checked path in `LeanC/Expr.lean` +
> `CFunc{declared/body + bodyLeDeclared}` + `callResolves`/`ProgramMeetsSpec` in
> `LeanC/Func.lean`/`LeanC/Program.lean`); F8 narrowed (statement costs still
> per-value combinators, but `ProgramMeetsSpec` + `programWorstCovers` +
> `mkProgramMeetsSpec` make the program the spec certificate). F1, F3, F4, F6,
> F7, F9, F10 remain as logged (deferred by design).

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

## F2 — literal `cells` ignore `CTypeSize.size_of`
Where: `LeanC/Literals.lean:33` (`tableLit` cells), `LeanC/Context.lean`
(`LiteralPoolEntry.cells`).
Pain: `CTypeSize.size_of : α → Nat` needs a *value*, but `strLit`/`tableLit`
only have types (`β : Type`) + lengths. `cells = s.length` / `n` assumes
width 1.
Choice: `cells = len` (bytes == cells for char; `n` cells for tables).
Costs are assigned per resource via `HasCost`/`costInClass`
(`Examples/Resources.lean` bridge), not via a hardwired `litBound`.
Next: thread an explicit `cellSize : Nat` (or `Inhabited β` witness) into
`tableLit`/`LiteralPoolEntry` when codegen needs byte counts; only the
producer changes.

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

## F5 — `call` takes `argStrs`, not typed args (no `argsBound` in core)
Where: spec `List (Σ α, CExpr Γ α)` → `LeanC/Expr.lean`
(`CExpr.call`: `fname + argStrs`).
Pain (two layers):
1. `Sigma` nesting with local `Γ` is kernel-rejected ("nested inductive
   parameters cannot contain local variables").
2. A mutual `CExpr`/`CExprArg` wrapper (`List (CExprArg Γ)`) typechecks for
   the inductive but breaks dependent elimination for `deref`/`index`
   (input index computed from result index — supported singly, not
   mutually; verified by minimised prototypes).
Choice: `call` stores `argStrs : List String` (for `emitExpr`) only.
Call-site costs flow per-resource when needed (`HasCost`/`costInClass`
bridge in `Examples/Resources.lean`); no hardwired `argsBound : ResourceBound`
in core (the old paired-`ResourceBound` design was superseded 2026-09-26).
Next: restore typed args via the plan's fallback — untyped `RawExpr` +
`check : RawExpr → Option (Σ α, CExpr Γ α)` shim, or `ULift`-ed mutual
encoding — when call-site checking (arity/type mismatch) is needed. Never
import `Func` from `Expr` to fix this (name stays the seam).

## F6 — GADT ctors need equality proofs (`deref`/`index`/`addr`)
Where: `LeanC/Expr.lean` (`deref`/`index`/`addr` carry `γ = …` proofs,
like `CLiteral`), `@`-patterns in `emitExpr`.
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
Where: `LeanC/Stmt.lean` (`CAssign`/`CDecl`/`CReturn` markers + emitters),
`LeanC/Processes.lean` (`CStatement` soundness-only, untouched).
Pain: plan asked for `Processes.lean` additions, but `Expr → Processes`
would forbid `Processes → Expr` (cycle). `Context ↔ Processes` would also
cycle if the context imported statement-level combinators.
Choice: `Stmt → Expr + Processes`, `Func → Stmt`, leaves stay leaves.
Costs stay per-resource (`HasCost`/`costInClass` bridge in
`Examples/Resources.lean`); no hardwired `seqBound`/`branchBound` in core.
`rg appendContext` still clean (no `appendContext` reintroduced).
Next: keep `Stmt` separate permanently; per-resource preservation lemmas
already live with their resources (`time_seq_preserves`, …).

## F8 — statement bounds are per-value, not per-type
Where: `LeanC/Stmt.lean` (value-level costs vs marker
`CAssign`/`CDecl`/`CReturn` with `True` soundness).
Pain: a `CStatement Γ α` bound on the *type* cannot depend on the *value*
(e.g. `assign rhs` cost depends on `rhs`). A faithful per-type bound would
need the bound (or the expr) in the type.
Choice: draft split — markers (`isStatementSound := True`) as
`SequentialProcess` placeholders; real costs flow per-value through
`HasCost`/`costInClass` (`Examples/Resources.lean`: `time_seq_preserves`,
`costInClass_mono`).
Next: value-dependent statements via `Σ` payloads in the statement type,
or parallel `costOf : stmt → R` functions per resource. Required before loop
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

> P0 Cost/Call/Loop follow-up (2026-09-28, `lake build` +
> `./.lake/build/bin/test` green): N1–N6 landed (`LeanC/Expr.lean`
> `CExpr.call : Nat → Nat` ×4 + `rawArgsTimeFn/MemFn` + `mkCallWithRaw`
> `dt/dm : Nat → Nat`; `LeanC/CostSpec.lean` threads `n`
> (`argTime n + declaredTime n + 1`, `max (argMem n) (declaredMem n)`) +
> `exprTime/MemBound : Nat → Nat` + conditional `O1`
> (`exprCallsO1Time/Mem`, `callTime/Mem_in_o1`, `o1_add_one/binop/tern`);
> `LeanC/Func.lean` parametric `CFunc` (`Nat → Nat` ×4, pointwise
> `bodyLeDeclared`) + `mkFuncWithBody` measured tie;
> `LeanC/Modules.lean`/`LeanC/Program.lean` pointwise worst
> (`Nat → Nat`) + `CallSite`/`callSites`/`ProgramCallsResolve` +
> `ProgramMeetsSpec.callsResolve` + `programFitsDevice` at `n`;
> N3 `rawMemBound (.callRaw _ args)` maxes over args; N4 `LeanC/Loop.lean`
> `forNTime/Mem` (sums/maxes) + `forNDiag` linear (`poly 1` core,
> `linear` in `Tests`) + `0`-iter `Zero`/`O1`; N5 `UnboundedLoop.whileTrue`
> marker (no inhabitant, cites `no_divergent_cost`); N6 `recWithFuel`
> (= `forN`, fuel example `countdown`). Tests: `Tests/TestCostSpec.lean`
> extends (parametric at two `n`s `6`/`11`, nested-mem `2`, bogus
> `¬ ProgramCallsResolve/MeetsSpec`, measured `add2Measured`,
> `poly1`+`linear` diag, `whileTrue` marker).

## F11 — call parametricity (`Nat` → `Nat → Nat`) ripples to `O1` gating
Where: `LeanC/Expr.lean:102-104` (`CExpr.call` ×4 fns), `LeanC/CostSpec.lean`
(`exprTime/MemFn`, `exprTime/MemBound : Nat → Nat`, `exprCallsO1Time/Mem`,
`exprTime/Mem_in_o1` conditional), `LeanC/Func.lean` (`CFunc` ×4 fns +
`mkFuncWithBody`), `LeanC/Program.lean` (`CallSite`, `callSites`,
`ProgramCallsResolve`, `programWorstTime/Mem : Nat → Nat`).
Pain: universal `exprTime_in_o1 ∀ e` is false for linear callees
(`fun n => n`); closed `Nat` bounds cannot bound varying fns; function
equality (`declaredTime = dt`) needs `rfl` on same literal (no funext in
tests); `at` is a Lean keyword (use `argT`/`decT`); `programWorstTime`
`Nat` → `Nat → Nat` breaks `ToString`/`rfl` tests (evaluate at `n`).
Choice: `O1` iff call specs `O1` (`exprCallsO1Time/Mem` hypothesis,
`BigO` on fns, not closed `K`); `mkCallWithRaw` lifts closed arg sums to
const fns (`rawArgsTimeFn/MemFn`), `n`-variation enters via `declared`;
`CFunc`/`Program` worst pointwise (`∀ n`); `CProgram.callSites` explicit
list (omission still possible — bodies not stored — but listed bogus
`fname` has no proof).
Next: store bodies (`CExpr`/`RawExpr`) in `CFunc`/`CProgram` when the
memory-model task needs omission-freedom (exact call-site enumeration);
keep `fname` seam (`Expr` never imports `Func`).

## F12 — loop friction (`forN` shape, uniform bound, `linear` location)
Where: `LeanC/Loop.lean` (`forNTimeAux/MemAux` recursion on `iters`,
`forNTime/Mem`, `forNDiagTime/Mem`, `forNTimeAux_le/MemAux_le`,
`forNDiag_linear` (`poly 1`), `Tests/TestCostSpec.lean`
`forNDiag_linear_glinear`).
Pain: `forN iters body` with fixed `iters` is `O1`, not linear — linear
needs diagonal (`iters = input size`, `forNDiagTime body n =
Σ_{i<n}`); per-iteration `O1` with distinct `K_i`/`N₀` does not give
uniform linear (e.g. `body i = const i`); `List.range` fold blocks
`BigO.add` induction (use `Nat` recursion `Aux`); `gpoly 1`
(`fun n => n ^ 1`) vs `glinear` (`fun n => n`) need `pow_one` bridge
(core proves `poly 1`, `Tests` proves `linear` — core never imports
`Examples/`); `ComplexityLE` needs `Lattice` import (not via `Bridge`).
Choice: N6 picks fuel (`Nat` bound, reduces to N4) over well-founded
variant (one example `countdown`, `recWithFuel = forN` by `rfl`);
unbounded (`whileTrue`, no fuel) reduces to N5 marker; `0` iters is
`Zero`-exact by `rfl` (both auxiliaries `0` definitionally).
Next: `∑`-lemma for variable-`iters` loops when `for`/`while` syntax
lands (`Stmt`); keep mem `max` (diagonal mem stays `O1` under uniform
bound).
## F13 -- traversal-closed certificate friction (P1-A)
Where: LeanC/Expr.lean (rawCallFnames, rawIsFirstOrder/rawArgsFirstOrder), LeanC/Func.lean (CallSite moved per D10, exprCalls/exprNested, CFunc calls/nested/bodySrc, mkFuncWithBody computed + mkLeafFunc), LeanC/Program.lean (programCallsComputed/programNestedComputed, ProgramCallsResolve/ProgramNestedResolve, no stored list), Tests/TestCostSpec.lean (computed-list fixtures, bogus-in-body).
Pain: GADT collectors must mirror exprTimeFn @-patterns for all 11 ctors; simp needs explicit unfolding set (mkLeafFunc/mkFuncWithBody/exprCalls/exprNested + body defs); List.Mem uses head/tail (not inl/inr), cases already unifies f (no subst needed); struct literals with fun need parens inside list; ascribed ({...} : CallSite) membership fails to parse -- use field form for equality + angle for membership; CExpr.call stores only erased argStrs, so exprNested (.call ...) is [] by construction (first-order-only choice P1-A4).
Choice (recorded): traversal home = Func.lean (not new Calls.lean); bodySrc := emitExpr body ++ ";" computed; mkLeafFunc for externals (bodySrc := ""); nestedRaw stays rawMemBound-only (second branch of A4).
Next: full CFunc opacity; transitive closure (currently direct calls only).

## F14 -- thin emission slice friction (P1-B)
Where: LeanC/Emit.lean (emitFuncDef/emitProgram demanding ProgramMeetsSpec, D11), Tests/TestEmit.lean (certified demo to /tmp .c via cc to run exit 0), test.lean.
Pain: Lean s! with C braces needs escaping which parses fragilely -- use ++ concatenation; IO.Process.output simpler than spawn+wait; postfix catch invalid in 4.23 -- use try/catch blocks; cc vs gcc probed at runtime (try cc, fallback gcc); empty-args puts() does NOT compile against libc puts(const char*) -- demo uses pure add2-shaped body; pool ty : Type unprintable -- emit as comment.
Choice (recorded): new Emit.lean (not Program extension); external emits comment, never redefinition; int main calls p.main + return 0.
Next: multi-unit linking, ABI header, struct layout, Clight semantics out of scope.

> P2 enforcing follow-up (2026-09-30, `lake build` + `./.lake/build/bin/test` green): E1–E6 landed (`CFunc` inductive, first-order gate, var/cast/field/lit proofs, module/program/emit cleanup, fixtures migrated, docs updated). F15–F20 below (each Where/Pain/Choice/Next + green marker).

## F15 -- `CFunc` inductive vs private-structure fallback (P2 E1)
Where: `LeanC/Func.lean:92-138` (inductive `withBody`/`leaf` + computed `def`s + `bodyLeDeclared`), `LeanC/Emit.lean:26-30` (match, not empty-string test).
Pain: computed `def`s matching on existential `CExpr Γ α` (`withBody` stores body + `IsCType` instance) trigger "typeclass stuck on metavariable" with dot-patterns (`| .withBody f _ _ _ _`); `bodyLeDeclared` for `withBody` must be defeq to `hle` after unfolding `bodyTime`/`declaredTime`; `emitFuncDef` with `.withBody ..` similarly stuck; old `simp` sets unfolding structure literals no longer unfold `CFunc.calls/nested/fname`.
Choice (recorded): inductive preferred (no fallback needed — kernel accepts existential `Γ α` as ctor-implicits, same reason `RawExpr` works); all computed `def`s + `emitFuncDef` use `@CFunc.withBody _ _ _ _ …` patterns (cf. F6); `Tests/` simp sets add `CFunc.calls/nested/fname/declaredTime/declaredMem`; `emitFunc` helper deleted (no deprecated alias); `CallSite` expected values switched to angle `⟨…⟩` so open-constructor `rg` shows only `Func.lean`.
Next: keep inductive permanently; never reintroduce structure fields/defaults on proof-relevant data.
Green: `lake build` + `./.lake/build/bin/test` exits `0`.

## F16 -- `VarScope` shape + `DraftCtx` decidability (P2 E3a)
Where: `LeanC/Variables.lean:31-42` (`VarScope` class + `DraftCtx idx < 2` instance + `Decidable` helper), `LeanC/Expr.lean:233` (`@CVarRef.mk` in `emitExpr`), `Tests/TestLiteralsExpr.lean:145-146` (`mk 0/1 by decide`).
Pain: `CVarRef.mk` with ctor-only `[VarScope Γ]` + proof breaks dot-pattern `| .mk idx _ _` in `toNat` ("function expected … has type CVarRef") — instance-implicit cannot be wild-carded in dot-pattern; `by decide` for `scopeContains` fails ("failed to synthesize Decidable") because class projection does not unfold for synthesis; `CExpr.var`/`addr` signatures would need `[VarScope Γ]` if the instance lived on the type (avoided via ctor-only).
Choice (recorded): one place `VarScope` in `Variables.lean` (`scopeContains : Nat → α → Prop`); ctor-only instance+proof so type mentions need no new constraint; `@CVarRef.mk _ _ _ _ idx _ _` in `toNat`/`emitExpr`; `DraftCtx` minimal `idx < 2` + explicit `Decidable` instance via `inferInstanceAs (Decidable (idx < 2))` (defeq unfolding); tests use `mk 0 (by decide)`/`mk 1 (by decide)`; proof-free alias deleted (no deprecated alias); attempted `CVarRef.mk 999` without proof fails (verified 2026-09-30): `Type mismatch CVarRef.mk 999 has type VarScope.scopeContains … → CVarRef … but expected CVarRef …` (missing proof arg).
Next: full `scope[idx]?` lookup + type equality + de Bruijn shifting when statement syntax lands; keep `Scopes.CVarScope` untouched.
Green: `lake build` + `./.lake/build/bin/test` exits `0`.

## F17 -- `ValidCast` home + ctor set (P2 E3b)
Where: `LeanC/Expr.lean:79-86` (`ValidCast` inductive), `LeanC/Expr.lean:110-111` (`cast` takes `ValidCast`), `Tests/TestLiteralsExpr.lean:89-95` (`widen8_16` positive).
Pain: `inductive ValidCast (α β : Type) [IsCType α] …` with concrete ctors (`ValidCast (I8) (I16)`) rejected ("mismatched inductive type parameter … must be fixed … consider making an index"); `cast e (fun _ _ => True)` previously allowed anything.
Choice (recorded): home `Expr.lean` (not `Ops.lean` — avoids `Context` import); `α β` are indices (`inductive ValidCast : Type → Type → Type`), no `IsCType` on `ValidCast` itself (already on `CExpr.cast`); minimal 5 ctors (`widen8_16/16_32/32_64`, `toUnsigned32`, `toFloat32`); positive `i16FromI8` via `.widen8_16` + `emitExpr = "((cast)1)"`; no existing `cast` uses in `Tests/` to migrate (verified by `rg`); attempted `cast e (fun _ _ => True)` fails (verified 2026-09-30): `has type … → Prop but expected ValidCast …`.
Next: wider table (signed→unsigned with range proof, float sizes) when stdlib needs it; keep second arg as `ValidCast`, never a function.
Green: `lake build` + `./.lake/build/bin/test` exits `0`.

## F18 -- `StructField` shape + test struct (P2 E3c)
Where: `LeanC/Expr.lean:90-92` (`HasStructField` Prop class), `LeanC/Expr.lean:122` (`field` takes instance), `Tests/TestLiteralsExpr.lean:97-110` (`PairI32` + instance + `pairField0`).
Pain: `(h : True)` allowed any `idx` + any `α`; explicit vs instance-implicit choice affects `@`-pattern arity (both keep 9 args, so collectors unchanged — verified); no core instances desired (would weaken enforcement).
Choice (recorded): `class HasStructField (S) (idx) (α) : Prop` with `ok : True` (Prop-valued class, instance-implicit on `field`); no core instances; minimal test struct `PairI32` in `Tests/` with `HasStructField PairI32 0 I32`; positive `pairField0` emits `x0.f0`; `field` with `True` no longer elaborates.
Next: real struct layout/padding when Types M3 lands; keep membership proof, never `True`.
Green: `lake build` + `./.lake/build/bin/test` exits `0`.

## F19 -- `lit`-proof threading pain (P2 E3d)
Where: `LeanC/Expr.lean:104` (`.lit` takes `litFitsType = true`), `LeanC/Func.lean:44,63` (`.lit _ _` wildcards), `LeanC/CostSpec.lean` (8 `.lit` arms), `Tests/` (all `CExpr.lit` sites + `rfl`).
Pain: every `.lit` match arm needs extra `_` (`.lit _` → `.lit _ _`, `.lit l` → `.lit l _`); every construction needs proof (`42 fits I32` is `rfl`-provable since `decide` reduces, but `simp` set must still unfold `litFitsType` for non-obvious values); non-int `= true` cases stay vacuous (same as today, recorded TODO).
Choice (recorded): `rfl` for all int `1`/`2`/`42` + table `rfl` (definitionally `true`); `CostSpec` arms forwarded to `litTimeFn`/`litMemFn` unchanged otherwise; out-of-range `intLit` + `rfl` fails (verified 2026-09-30): `has type ? = ? but expected litFitsType (intLit I8 … 99999 …) = true`.
Next: float/char/str/table range checks when Literals M2 lands; keep proof arg, never bare `CLiteral`.
Green: `lake build` + `./.lake/build/bin/test` exits `0`.

## F20 -- `h`-at-call-sites pain + `Bootstrap` removal (P2 E2/E6)
Where: `LeanC/Expr.lean:220-225` (`mkCallWithRaw` takes `h`), `LeanC/CostSpec.lean:735-758` (theorems take `h`), `Tests/` (all 4+1 sites), `LeanC/Bootstrap.lean` deleted.
Pain: `rfl` proves `rawArgsFirstOrder []` (empty reduces definitionally) but NOT `rawArgsFirstOrder [.strLit …]` ("application type mismatch … has type ? = ?") — `flatMap` does not reduce by `rfl`; `by decide` fails ("failed to synthesize Decidable") because def does not unfold for synthesis; old `simp` sets miss `CFunc.calls/nested` after E1 (see F15).
Choice (recorded): empty args `rfl`; non-call args `by simp [rawArgsFirstOrder, rawCallFnames]` (both defs in set, per spec); `CostSpec` theorems thread `h` (proof-erased, `rfl` still proves time/mem equations); `nestedRaw` stays `rawMemBound`-only + `¬ rawArgsFirstOrder […]` + "lifting ill-typed by E2" comment; attempted nested lift with `rfl` fails (verified 2026-09-30): `has type ? = ? but expected rawArgsFirstOrder [callRaw "f" …]`; `Bootstrap.whitelist_bool` deleted (was `true` for all ctors, dead code, unimported — deletion chosen over vacuous proof, per spec C9).
Next: keep gate permanently; never lift `callRaw` args without proof.
Green: `lake build` + `./.lake/build/bin/test` exits `0`.
