# Next Step — P2 Deep certificates (enforcing, not culture)

Status: landed (2026-09-30). P2 enforcing (`CFunc` inductive, first-order gate,
var/cast/field/lit proofs landed). P1 landed 2026-09-29 (`doc/next_task.md` §§0–7):
traversal collectors + `mkFuncWithBody`/`mkLeafFunc` + computed
`programCalls/NestedComputed` + `emitProgram` demanding the proof +
`Tests/TestEmit.lean` `cc`-green. `lake build` + `./.lake/build/bin/test`
green at plan time.
Related: `doc/next_task.md` §0 (binding intent, §0 here restates it
strictly); `doc/roadmap.md` §§5,7,9-M2; `doc/stdlib_friction.md`
F1–F14; `LeanC/Expr.lean`, `LeanC/Func.lean`, `LeanC/Modules.lean`,
`LeanC/Program.lean`, `LeanC/Emit.lean`, `LeanC/Variables.lean`,
`LeanC/Literals.lean`, `LeanC/Stmt.lean`, `LeanC/Context.lean`,
`LeanC/CostSpec.lean`.

## 0. Ideology (binding, corrects P1 culture drift)

**Compile → correct.** A Lean `def` that typechecks IS the certificate.
There is no separate “please follow the discipline” step. If the docs
say “don’t do X” but `lake build` accepts X, that is a bug in the
types, not in the producer. P2 converts every such “don’t” into a
kernel rejection.

**Program is proof of specifications (statement of theorem).**
Formally:

```lean
Spec p : Prop := ProgramMeetsSpec p
Cert := Σ p : CProgram, Spec p
emit : Cert → String   -- LeanC/Emit.lean:34 emitProgram (p) (_ : Spec p)
```

`CProgram` alone is data. `Spec p` is the theorem statement
(main resolves, every body fits, every computed call site resolves,
every nested name exists, worst covers). The proof term IS what gets
emitted. Budgets and emitters trust only `Cert`, never a bare program.

Enforcement rule for P2: **store the body, compute everything.**
Any value that can be computed from a stored body MUST be a `def`
by pattern-matching, never a structure field with a default that a
producer can forge. Any obligation that must hold for correctness
MUST be a constructor argument (no default), never a `Prop := True`
field, never a doc comment. Where older text conflicts with this
section, this section wins.

## 1. Goal

One track only, in order: harden P1 into deep certificates.
No new features. Out of scope: `Stmt` payload syntax, memory model
(allocation/load/store/aliasing), ABI header (`lean_c_target.h`),
multi-unit linking, Clight semantics, benchmarks, `≤`-spec
relaxation (see §7).

* **E1 (core):** `CFunc` becomes an inductive with 2 constructors only.
  `calls`/`nested`/`bodyTime`/`bodyMem`/`bodySrc`/`fname`/`declared`
  become `def`s by matching. Forged `calls := []` becomes
  unrepresentable (no field to set).
* **E2 (first-order gate):** `mkCallWithRaw` takes
  `(h : rawArgsFirstOrder args)`. Nested lift fails to typecheck.
* **E3 (leaf holes):** `CVarRef` membership, `cast` validity,
  `field` membership, `lit` range — all by proof args, no `True`.
* **E4 (cleanup):** delete all `is*Sound := True` fields and all
  `:= fun _ => 0 / [] / ""` defaults on proof-relevant data;
  emitter uses computed `def`s.
* **E5–E6:** migrate fixtures, add negative `¬`-examples that only
  typecheck if enforcement holds, update docs + friction log.

## 2. Why now (culture inventory — each row is an E-task)

| # | Culture today (builds but wrong) | Forgery demo | Enforcement target (P2) |
|---|---|---|---|
| C1 | `CFunc` open `structure` + defaults `LeanC/Func.lean:98-109` (`declared/body := fun _=>0`, `calls/nested := []`, `bodySrc := ""`, `isFuncSound := True`); doc “direct literals ill-discipline” `Func.lean:96-97` | `{ fname := "f", calls := [] }` hiding a `call "bogus"` typechecks | E1: inductive, no fields, no defaults |
| C2 | `bodySrc : String` stored field `Func.lean:108,126`; old `emitFunc (f bodyStr)` `Func.lean:151-152` takes pre-rendered text | stale `bodySrc` mismatching body emits wrong C | E1+E4: `def CFunc.bodySrc : CFunc → String := match … => emitExpr body ++ ";"` ; `Emit.lean:24-28` uses it; delete `emitFunc` or mark deprecated |
| C3 | `mkCallWithRaw` no hypothesis `Expr.lean:196-200`; `rawArgsFirstOrder` defined `Expr.lean:189-190` but unused; `exprNested (.call) := []` `Func.lean:73` + “discipline” `Func.lean:39-40`, `Expr.lean:178-184` | `mkCallWithRaw "puts" [.callRaw "f" …] …` erases inner `"f"` to `argStrs`, `nested` stays `[]`, hole invisible | E2: require `(h : rawArgsFirstOrder args)`; `nestedRaw` (`TestCostSpec.lean:251`) becomes unliftable by type |
| C4 | `CVarRef.mk idx` no membership `Variables.lean:21-22`; “producer duty” `Variables.lean:7-10`; `ofUInt` alias `33-35`; `CVarIndex.value := 0` weak | `CVarRef.mk 999` emits `x999` with no binding | E3a: membership proof arg (see §4) |
| C5 | `cast : CExpr α → (α→β→Prop) → CExpr β` `Expr.lean:89-90` | `cast e (fun _ _ => True)` casts anything | E3b: `ValidCast` inductive, only legal ctors |
| C6 | `field … (h : True)` `Expr.lean:98-99` | any `idx` + any `α` typechecks | E3c: `StructField` proof arg |
| C7 | `litFitsType = true` for char/float/str/table `Literals.lean:49-56` (`TODO:48`); `.lit` takes no proof `Expr.lean:83` | out-of-range `intLit` via direct `.lit` (range check exists but unenforced) | E3d: `.lit` takes `(h : litFitsType l = true)` |
| C8 | `CModule.isModSound := True` `Modules.lean:17`; `CProgram.isProgSound := True` `Program.lean:34`; `CStatement := True` `Stmt.lean:28-35`, `Processes.lean` | vacuous “soundness” masquerades as certificate | E4: delete all three fields (keep real `moduleAllBodiesFit`, `ProgramMeetsSpec`) |
| C9 | `Bootstrap.whitelist_bool := true` all ctors (dead code, unimported) | any probe “proven” safe | E6: either prove properly or delete file; do not leave vacuous proof as example |

Note on transitivity: `programCallsComputed` (`Program.lean:68-69`,
`flatMap` over ALL funcs) already covers multi-hop bogus (B’s direct
`calls` include `bogus` even if A calls B). No fixpoint needed in P2.
Keep flatMap, document this in code comment (E4).

## 3. Frozen decisions (do not relitigate)

| # | Decision | Rationale / ref |
|---|---|---|
| D1–D12 | Carried from P1 (`doc/next_task.md` §3): name seam `call ↔ Func` (`Expr` never imports `Func`); computed not listed (D9); `CallSite` in `Func` (D10); emitter takes proof (D11); nested existence-only (D12); `match`-recursion not `induction … with` (GADT motives) | Import direction `Emit → Program → Modules → Func → Expr` preserved; see `Func.lean:8-10`, `Stmt.lean:10-12` |
| D13 | Parametric costs `Nat → Nat` kept (`declaredTime/Mem`, `bodyTime/Mem`, `argTime/Mem`); bound rules unchanged (`argT n + declT n + 1`, `max argM declM`) | `CostSpec.lean:379-443`; changing to `≤`-specs is future work (§7) |
| D14 | `callResolves` stays function-equality (`=`, `rfl` on same literal) for P2 | `Program.lean:61-63`; relaxing to pointwise `≤` changes `ProgramCallsResolve` + all fixtures — explicitly out of scope |
| D15 | Enforcement by construction, not by `Subtype` + separate well-formedness predicate where a direct inductive works | `CFunc`/`CVarRef`/`cast`/`field` below: fewer predicates, fewer `τ ∉ τs`-style burdens; `Subtype` allowed only if inductive breaks elimination (record + friction log) |
| D16 | No new constructors for `CExpr` in P2 (only added proof args on `lit`/`cast`/`field`, changed `mkCallWithRaw` hypothesis) | Keeps `CostSpec.lean` `exprTime/MemFn/Bound` + `exprCalls/exprNested` match arms compiling with minimal diff |
| D17 | Conventions binding: `Prop`-valued obligations; no `sorry`/`axiom`/`admit` in new code; `BigO` kit only, no Mathlib dep; `@`-patterns for GADT ctors (cf. `doc/stdlib_friction.md` F6) | Matches P0/P1 conventions |

## 4. Scope (P2 only: E1–E6)

### E1 — `CFunc` inductive (core, breaks everything else first)

File: `LeanC/Func.lean` (rewrite `98-136`; keep `CallSite:24-27`,
`exprCalls:42-54`, `exprNested:61-73` unchanged except new `.lit`
proof arg wildcard).

Replace `structure CFunc` with:

```lean
inductive CFunc where
| withBody {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (fname : String) (body : CExpr Γ α)
    (declaredTime declaredMem : Nat → Nat)
    (hle : (∀ n, exprTimeBound body n ≤ declaredTime n) ∧
           (∀ n, exprMemBound body n ≤ declaredMem n)) : CFunc
| leaf (fname : String) (declaredTime declaredMem : Nat → Nat) : CFunc
```

Then computed projections (all `def`, no fields):

```lean
-- def CFunc.fname : CFunc → String | .withBody f _ _ _ _ => f | .leaf f _ _ => f
-- def CFunc.declaredTime : CFunc → Nat → Nat | .withBody _ _ dt _ _ => dt | .leaf _ dt _ => dt
-- def CFunc.declaredMem : CFunc → Nat → Nat | .withBody _ _ _ dm _ => dm | .leaf _ _ dm => dm
-- def CFunc.bodyTime : CFunc → Nat → Nat | .withBody _ body _ _ _ => exprTimeBound body | .leaf _ dt _ => dt
-- def CFunc.bodyMem : CFunc → Nat → Nat | .withBody _ body _ _ _ => exprMemBound body | .leaf _ _ dm => dm
-- def CFunc.calls : CFunc → List CallSite | .withBody _ body _ _ _ => exprCalls body | .leaf _ _ _ => []
-- def CFunc.nested : CFunc → List String | .withBody _ body _ _ _ => exprNested body | .leaf _ _ _ => []
-- def CFunc.bodySrc : CFunc → String | .withBody _ body _ _ _ => emitExpr body ++ ";" | .leaf _ _ _ => ""
-- def CFunc.bodyLeDeclared (f) : (∀ n, f.bodyTime n ≤ f.declaredTime n) ∧ _ := by match f …
```

Keep names `mkFuncWithBody` / `mkLeafFunc` as thin wrappers
(`def mkFuncWithBody … : CFunc := .withBody …`) so existing call
sites diff minimally — but they are now the ONLY constructors
reachable (no structure-literal syntax remains). Delete
`isFuncSound`, all `:=` defaults, old `emitFunc (f bodyStr)`
(or keep one-line deprecated alias with doc, agent records choice).

Import note: `CFunc.withBody` stores `CExpr Γ α` existentially —
`Γ α` are constructor-implicit, not parameters, so no kernel
`Sigma`-nesting rejection (same reason `RawExpr` works; cf.
`Expr.lean:40-60`). If Lean rejects the inductive (universe/type
motive), fallback D15: keep `structure` but make constructor
`private` + expose only the two smart constructors + computed
`def`s + `rg` check that no `{ … : CFunc }` literal remains outside
`Func.lean`. Record choice + friction entry either way.

### E2 — first-order gate (one-line enforcement)

File: `LeanC/Expr.lean:196-200`. Change signature only:

```lean
-- def mkCallWithRaw {Γ} [CContext Γ] {α} [IsCType α]
--     (fname : String) (args : List RawExpr)
--     (h : rawArgsFirstOrder args)
--     (declaredTime declaredMem : Nat → Nat) : CExpr Γ α
```

Body unchanged. Update every call site in `Tests/` to supply `h`
(empty args: `rfl`; non-call args: `by simp [rawArgsFirstOrder,
rawCallFnames]` or `by decide`; agent picks per site, `simp` set
must include both defs). `nestedRaw`
(`Tests/TestCostSpec.lean:251`) must REMAIN unlifted; existing
`¬ rawArgsFirstOrder […]` example (`TestCostSpec.lean:339-340`)
becomes the proof that lifting it is ill-typed (add comment +
`example : ¬ rawArgsFirstOrder nestedRaw.args` shape if needed).

### E3 — leaf holes (each small, all required)

* **E3a vars** (`LeanC/Variables.lean:21-22`, `LeanC/Context.lean:88-96`
  `DraftCtx.scope`): add minimal scope-membership class (agent’s
  naming, one place, record it), e.g.

  ```lean
  -- class VarScope (Γ : Type v) [CContext Γ] where
  --   scopeContains : Γ → Nat → (α : Type) → [IsCType α] → Prop
  -- inductive CVarRef (Γ) [CContext Γ] (α) [IsCType α] where
  -- | mk : (idx : Nat) → scopeContains Γ … idx α → CVarRef Γ α  -- exact binder shape agent's choice
  ```

  Provide `DraftCtx` instance via `scope[idx]?` lookup + type
  equality (producer still chooses names, but dangling `idx`
  is now ill-typed). Update `CVarRef.toNat` (drop proof),
  `emitAssign/Decl/Return` (`Stmt.lean:38-52`), `addr`
  (`Expr.lean:93-94`, carries `CVarRef` so gets membership free).
  Delete `ofUInt` or reimplement via `decide`+proof (record choice).
  Out of scope: full de Bruijn shifting, `Scopes.CVarScope` migration.

* **E3b cast** (`LeanC/Expr.lean:89-90`): replace `(α → β → Prop)`
  with new `inductive ValidCast (α β : Type) [IsCType α] [IsCType β]`
  in `LeanC/Expr.lean` (or `Ops.lean`, agent picks, records it).
  Minimal ctors: `widen` (I8→I16→I32→I64 signed), `toUnsigned`
  with range proof, `toFloat`. Migrate existing `cast` uses
  (list them in friction log; if none in `Tests/`, add one positive
  `widen` example + one comment that `fun _ _ => True` no longer
  elaborates). Keep cost `+1`/mem-passthrough in `CostSpec.lean`
  unchanged.

* **E3c field** (`LeanC/Expr.lean:98-99`): replace `(h : True)` with
  `(h : StructField S idx α)` where `StructField` is a new
  `Prop`-valued class/predicate (minimal: `class HasStructField
  (S : Type) (idx : Nat) (α : Type) where ok : Prop`, ctor takes
  `[HasStructField S idx α]` or explicit `h`). No instances needed
  in P2 beyond one test struct (agent defines minimal test struct
  in `Tests/`, records it). Emits same `.f{idx}`.

* **E3d lit range** (`LeanC/Expr.lean:83`, `Literals.lean:49-56`):
  change `.lit` ctor to `| lit {α} [IsCType α] : (l : CLiteral α) →
  litFitsType l = true → CExpr Γ α`. Update `emitExpr (.lit l _)`,
  `exprTime/MemFn/Bound (.lit …)`, `exprCalls/exprNested (.lit …)`
  wildcards, and every `CExpr.lit (…)` site in `Tests/` (append
  `rfl`/`by decide`; `42 fits I32` already `rfl`-provable).
  Non-int `= true` cases stay vacuous in P2 (recorded TODO, same
  as today) — enforcement lands for `intLit` now, extensible later.

### E4 — module/program/emitter cleanup

Files: `LeanC/Modules.lean:14-17`, `LeanC/Program.lean:30-34`,
`LeanC/Emit.lean:21-28`, `LeanC/Func.lean:138-152`.

* Delete `isModSound`, `isProgSound` fields entirely (not `:= True`
  → keep; DELETE). Keep `moduleAllBodiesFit`, `ProgramMeetsSpec`.
* `modWorstTime/Mem`, `programWorstTime/Mem`, `programCalls/NestedComputed`,
  `callResolves`, `ProgramCalls/NestedResolve`, `programWorstCovers`,
  `mkProgramMeetsSpec` unchanged except they now project through E1
  `def`s (same names, new bodies — `.declaredTime` etc. still resolve).
* `Emit.lean`: `emitFuncDef` matches on `CFunc` inductive directly
  (`leaf` → comment, `withBody` → definition from computed
  `bodySrc`); drop `== ""` string test (keep behaviour, enforce by
  construction). Add code comment documenting why flatMap covers
  multi-hop bogus (no fixpoint needed).
* Delete `Func.emitFunc (f bodyStr)` single-line helper, or keep as
  deprecated alias (record choice). `Tests/` must use `emitFuncDef`.

### E5 — tests migration (no new coverage, only hardening)

Files: `Tests/TestCostSpec.lean`, `Tests/TestEmit.lean`,
`Tests/TestLiteralsExpr.lean`, `test.lean` (wire only if new file).

* Migrate ALL `mkCallWithRaw` sites (E2 `h`), ALL `CExpr.lit` sites
  (E3d proof), ALL `CVarRef.mk`/`ofUInt` sites (E3a proof), ALL
  `cast`/`field` sites (E3b/c). `lake build` is the checklist —
  every breakage is a migrated site, no silent skips.
* Keep existing green examples: `add2` bounds `1`/`0`, `hello` mem
  `5`, `linearCall` `n`-variance, `nestedRaw` mem, `progPuts`
  computed `[puts]`, `progBogus` `¬ Resolve` + `¬ MeetsSpec`,
  `demoProg` `cc`-compile+run `exit 0`.
* Add hardening examples (compile-time, not `IO`):
  1. `example : ¬ ProgramMeetsSpec progBogus` (exists — keep).
  2. Comment + `rg`-check that no `{ … : CFunc }`,
     `{ … : CModule }`, `{ … : CProgram }` literal remains outside
     smart constructors (acceptance §6.3).
  3. `nestedRaw`-lift rejection: keep `¬ rawArgsFirstOrder […]`
     + comment “lifting this arg list is ill-typed by E2”.
  4. One `ValidCast.widen` positive example (E3b).
  5. One dangling-var rejection comment (E3a): `CVarRef.mk 999`
     without proof does not elaborate (do NOT commit a failing
     file — document the attempted term + error in friction log).
* `Tests/TestComplexity.lean` untouched.

### E6 — docs + friction

* `doc/roadmap.md` §§5,7: one-line status “P2 enforcing (CFunc
  inductive, first-order gate, var/cast/field/lit proofs landed)”;
  fix stale “P1-A adds…” future tense if still present.
* `doc/modules.md`: update `Variables` (membership), `Expr`
  (`ValidCast`, `lit` proof, gated `mkCallWithRaw`), `Func`
  (inductive + computed defs), `Modules`/`Program` (True fields
  removed), `Emit` (match, not `== ""`).
* `doc/stdlib_friction.md`: append F15+ (one entry per E-choice:
  inductive vs private-structure fallback, `VarScope` shape,
  `ValidCast` home, `StructField` test struct, `lit`-proof
  threading pain, `h`-at-call-sites pain). Convention: each entry
  has Where/Pain/Choice/Next + green marker.
* This file + `roadmap.md` status must agree (thin line only).

Out of scope (explicit non-goals, do not sneak in):
resource/pool/budget unification; `callResolves` `=` → `≤`
relaxation; `Stmt` payloads; memory model; ABI header;
multi-unit linking; benchmarks; Mathlib.

## 5. Frozen interfaces (agents build against these)

```lean
-- E1 (Func.lean): inductive, computed defs, thin wrappers kept
-- inductive CFunc | withBody {Γ α} [CContext Γ] [IsCType α] (fname) (body : CExpr Γ α) (dt dm) (hle) | leaf (fname) (dt dm)
-- def CFunc.fname / declaredTime / declaredMem / bodyTime / bodyMem / calls / nested / bodySrc : CFunc → …
-- def mkFuncWithBody {Γ α} [CContext Γ] [IsCType α] (fname) (body) (dt dm) (hle) : CFunc := .withBody …
-- def mkLeafFunc (fname) (dt dm) : CFunc := .leaf …
-- def CFunc.bodyLeDeclared (f) : (∀ n, f.bodyTime n ≤ f.declaredTime n) ∧ _

-- E2 (Expr.lean): gated checked-call
-- def rawArgsFirstOrder (args : List RawExpr) : Prop  -- unchanged
-- def mkCallWithRaw {Γ α} [CContext Γ] [IsCType α] (fname) (args : List RawExpr) (h : rawArgsFirstOrder args) (dt dm) : CExpr Γ α

-- E3a (Variables.lean + Context instance): membership (exact binder names agent's choice, one place, recorded)
-- class VarScope (Γ) [CContext Γ] where scopeContains : Γ → Nat → (α : Type) → [IsCType α] → Prop
-- inductive CVarRef (Γ) [CContext Γ] (α) [IsCType α] where | mk : (idx : Nat) → … scopeContains … → CVarRef Γ α

-- E3b (Expr.lean or Ops.lean): valid casts only
-- inductive ValidCast (α β : Type) [IsCType α] [IsCType β] where | widen … | toUnsigned … | toFloat …
-- CExpr.cast : CExpr Γ α → ValidCast α β → CExpr Γ β

-- E3c (Expr.lean): field membership
-- CExpr.field : CExpr Γ S → (idx : Nat) → StructField/HasStructField S idx α → CExpr Γ α  -- no True

-- E3d (Expr.lean): lit validity
-- CExpr.lit : (l : CLiteral α) → litFitsType l = true → CExpr Γ α

-- E4: Modules/Program True fields DELETED (not defaulted)
-- structure CModule where (funcs : List CFunc) (pool : List LiteralPoolEntry)
-- structure CProgram where (mods : List CModule) (main : String) (pool : List LiteralPoolEntry)
-- Emit matches CFunc inductive; no `bodySrc == ""` test
```

Bound rules unchanged (§3 D13). Reuse `BigO.add`/`max_bound`/`const_le_one`.

## 6. Build order

```text
E1 (CFunc inductive + defs) ─▶ E2 (first-order h) ─▶ E3 (a vars → b cast → c field → d lit) ─▶ E4 (modules/program/emit cleanup) ─▶ E5 (fixtures migrate + negatives) ─▶ E6 (docs)
```

E1 first (everything breaks on it by design). E2 before E3 (call
sites thread through all fixtures). E3a→d in listed order (var
proofs touch most lines). E4 after E1–E3 compile. E5 migrates as
each E lands (do not batch to the end). E6 last.

`Tests/TestComplexity.lean` untouched throughout. New test file only
if E3 needs a `ValidCast`/struct demo that does not fit existing
files (agent picks, records it, wires into `test.lean:runAll`).

## 7. Acceptance criteria

1. `lake build` clean; `rg -n "sorry|axiom|admit" LeanC Tests`
   empty for new code (comments excluded only if they quote the
   word, as today `Arrays.lean:61`, `Context.lean:48`); 
   `./.lake/build/bin/test` exits `0`.
2. No open-constructor forgery remains, checked by `rg`:
   `rg "isFuncSound|isModSound|isProgSound" LeanC Tests` empty;
   `rg "\{ *fname *:=" LeanC Tests` shows only inside
   `LeanC/Func.lean` wrappers (or empty if inductive — no structure
   literals at all); `rg "bodySrc :=|calls :=|nested :=|bodyTime :=|bodyMem :=" LeanC Tests`
   empty outside `Func.lean` defs; `rg 'bodySrc == ""' LeanC`
   empty; `rg "ofUInt|emitFunc \(f" LeanC Tests` either empty or
   single deprecated alias with doc (recorded).
3. Bogus-in-body still rejected over COMPUTED lists:
   `progBogus`-shaped fixture (rebuilt via E1+E2 constructors)
   yields `¬ ProgramCallsResolve` + `¬ ProgramMeetsSpec`
   (existing `Tests/TestCostSpec.lean:283-324` migrated, not deleted).
   Omission trick impossible — there is no list/field to omit from
   (E1 defs + E4 program `flatMap` defs).
4. First-order gate enforced: every `mkCallWithRaw` site passes `h`;
   `nestedRaw` stays `rawMemBound`-only; attempting to lift
   `[.callRaw "f" …]` fails to elaborate (friction log quotes the
   error; `¬ rawArgsFirstOrder […]` example green).
5. Leaf holes closed: `CVarRef.mk` without membership proof does not
   elaborate (friction log documents attempt); `cast` with
   `fun _ _ => True` does not elaborate (one `ValidCast` positive
   example green); `field` with `True` does not elaborate;
   `CExpr.lit` with out-of-range `intLit` + `rfl` does not elaborate
   (existing `42 fits I32` still `rfl`).
6. Thin slice still closed: `Tests/TestEmit.lean` emits FROM a
   `ProgramMeetsSpec` proof (emitter signature unchanged, D11),
   `cc` compiles, binary exits `0` (same `/tmp/lean_c_emit_test*`
   paths).
7. Docs agree: this file + `doc/roadmap.md` §§5,7 + `doc/modules.md`
   status lines match (thin lines only); `doc/stdlib_friction.md`
   appends F15+ with Where/Pain/Choice/Next per E-choice.
8. No scope creep: `rg "ValidCast|scopeContains|StructField|rawArgsFirstOrder"`
   shows only E2/E3 homes + call sites; no `Stmt` payloads, no
   memory-model inductives, no `lean_c_target.h`, no Mathlib import,
   no `callResolves` signature change.

## 8. Open questions (not blockers, agent picks one + records it)

* `CFunc` inductive vs `private`-constructor structure fallback (D15)
  — inductive preferred; fallback allowed only if kernel/universe
  rejects, with friction entry.
* `VarScope.scopeContains` exact shape (bool vs `Prop`, `Γ`-generic
  vs `DraftCtx`-only, `CIndex`-link) — minimal `DraftCtx`
  instance suffices for P2.
* `ValidCast` home (`Expr.lean` vs `Ops.lean`) + ctor set (minimal
  widen/toUnsigned/toFloat vs wider table).
* `StructField` shape (class vs `Prop` def) + test struct location.
* `cc` vs `gcc` probe (reuse `Tests/TestEmit.lean:77-96` as-is).
* `callResolves` `=` → pointwise `≤` relaxation: FUTURE, not P2
  (would need `funcMatchesCall` + `ProgramCallsResolve` redesign +
  fixture-wide `rfl`→`omega` migration; log as Next, do not start).

## 9. Archive — finished work (context only, not executable)

P1 (landed 2026-09-29, executable record was `doc/next_task.md`
§§1–7 before this file): collectors (`rawCallFnames`, `exprCalls`,
`exprNested`), `CFunc` stored computed `calls`/`nested`/`bodySrc` +
`mkLeafFunc`, program computed `flatMap` + `ProgramNestedResolve`,
first-order-only nested rule + bogus-in-body rejection, `emitProgram`
from proof + `cc`-compiled test. P1’s remaining culture (open
structure, defaults, `True` fields, unhypothesised `mkCallWithRaw`,
proof-free `var`/`cast`/`field`/`lit`) is exactly what P2 enforces.
P0 Cost/Call/Loop (2026-09-28) + Literals/Expr (2026-09-27) + Complexity
(2026-09-17/26/27): see `doc/next_task.md` §8; `doc/implementation_plan*.md`
HISTORICAL, do not build from.
