import LeanC.Types
import LeanC.TypeClasses
import LeanC.Arrays
import LeanC.Context
import LeanC.Literals
import LeanC.Variables
import LeanC.Ops

/-!
# Expressions — intrinsically typed C expressions (T4)

Stages (D3): A (`lit/var/unop/binop/cast`) lands first; B adds
`deref/addr/index/field` (proof-carrying memory access); C adds `tern`
+ `call` (`call` takes `fname + argStrs`, never a `Func` body, breaking
the `call ↔ Func` cycle — `Expr` never imports `Func`).

Operator typing (see `Ops.lean`): `unop`/`binop` are polymorphic over
the operator type `Op` with `[IsCUnOp/IsCBinOp Op]` + `[UnOpSig/BinOpSig
Op In Out]`. Arithmetic/bitwise/shift preserve (`In = Out`);
comparison/logic/logical-not return signed `I32`.

Costs (if any) are assigned per resource type via `CContext.setResource`
+ `CResource` instances in the caller's file — no hardwired bounds here.

Universe note: `α : Type` (`Type 0`), not `Type u` — concrete C types live
in `Type 0` (see `Literals.lean`). `index` uses `CGlobalStaticMemoryBlock`
(`Type 0`), NOT `CArrayType` (`Type 1`): `CArrayType α n` needs
`n : Type (u+1)`, which would force `CExpr` into `Type 1` for all `α` and
break `lit` (`CLiteral : Type 0`). See friction F3. `field` is a stub
(`base + idx + True` proof). `addr` carries an equality proof
`β = CPointerType α` (like `CLiteral`) so `emitExpr` matches without
refining the fixed result index.

`within_bounds` note: `CConstIndex/CFixedIndex/CVarIndex` all live in
`Type (u+1)`, but `CArray.within_bounds (β : Type)` expects `Type 0` —
so they cannot be used as access indices (see friction F4). This file
provides `CNatIndex : Type 0` for the pilot; `CConstIndex` remains the
size descriptor for `CArrayType` (unused here).

`call` args note: spec writes `List (Σ α, CExpr Γ α)`, but `Sigma`
nesting with local `Γ` is rejected by the kernel, and a mutual
`CExpr`/`CExprArg` wrapper breaks dependent elimination for `deref`/
`index` (input index computed from result index — supported singly,
not mutually). Fix (F5 closed, N1 parametric): `call` takes `argStrs`
for emission PLUS explicit cost functions `argTime/argMem`
(summed arg costs, computed via `mkCallWithRaw`, not trusted) and
`declaredTime/declaredMem : Nat → Nat` (callee's declared spec as a
cost function of input size `n`, discharged at `Func`/`Program` level
— see `LeanC/Func.lean`, `LeanC/Program.lean`: a program is a proof of
specs, so every `call` must resolve to a registered `CFunc` with
matching bounds, otherwise the program proof fails). Typed args are
threaded via the non-mutual `RawExpr` below (untyped syntax +
`rawTimeBound`/`rawMemBound` + `mkCallWithRaw`, which computes the arg
fns by construction): `RawExpr` never mentions `Γ`/`CExpr`, so no
kernel nesting rejection and no mutual block. Direct `call` with
explicit `fun _ => K` remains for leaf/external calls (e.g. `puts`);
`Program.callResolves` must still discharge them. Parametric `n`
threads through calls: `exprTimeFn (call …) n = argTime n +
declaredTime n + 1`, `exprMemFn … n = max (argMem n) (declaredMem n)`
(see `LeanC/CostSpec.lean`).
-/

namespace LeanC
universe v

/-- Type-0 constant index for `within_bounds` proofs (see module doc).
`CConstIndex` cannot be used here (universe `Type (u+1)` vs `Type 0`). -/
inductive CNatIndex (v : Nat) : Type where
| mk
instance {v : Nat} : CIndex (CNatIndex v) where
  isIndex := True
  value := v

/-- P2 E3b valid casts only (home: `Expr.lean`, agent's choice — keeps
import closure small; `Ops.lean` rejected to avoid `Context` import).
Each ctor IS the permission: `cast e (fun _ _ => True)` no longer
elaborates (second arg must be `ValidCast`, not a function). Minimal ctor
set: signed widening chain + unsigned + float.
`α β` are indices (after `:`), not params, so ctors may vary them
(same reason `CExpr` uses indices; params would force `α` fixed). -/
inductive ValidCast : (α β : Type) → Type where
| widen8_16 : ValidCast (CIntType .I8 true) (CIntType .I16 true)
| widen16_32 : ValidCast (CIntType .I16 true) (CIntType .I32 true)
| widen32_64 : ValidCast (CIntType .I32 true) (CIntType .I64 true)
| toUnsigned32 : ValidCast (CIntType .I32 true) (CIntType .I32 false)
| toFloat32 : ValidCast (CIntType .I32 true) (CFloatType .F32)

/-- P2 E3c struct-field membership (home: `Expr.lean`). `HasStructField`
is a `Prop`-valued class: only types with an instance can form `field`.
No core instances (test struct instance lives in `Tests/`). Emits same
`.f{idx}`. -/
class HasStructField (S : Type) (idx : Nat) (α : Type) : Prop where
  ok : True

/-- Intrinsically typed C expression. `Γ` is the context type, `α` the C
type (`Type 0`). Memory safety by proof args (`isAllocated`/
`within_bounds` — no proof = no term). `deref`/`index`/`addr` carry
equality proofs (like `CLiteral`) so `emitExpr` matches without
refining indices — required for dependent elimination with multiple
GADT ctors (see friction F5b). `unop`/`binop` carry their operator type
`Op` plus `UnOpSig`/`BinOpSig` typing (see `Ops.lean`) — illegal
`lt`-returns-pointer terms are unrepresentable.
P2 E3: `lit` takes `litFitsType = true`, `cast` takes `ValidCast`,
`field` takes `HasStructField` (no `True`). -/
inductive CExpr (Γ : Type v) [CContext Γ] : (α : Type) → [IsCType α] → Type 1 where
| lit   {α : Type} [IsCType α] : (l : CLiteral α) → litFitsType l = true → CExpr Γ α
| var   {α : Type} [IsCType α] : CVarRef Γ α → CExpr Γ α
| unop  {Op In Out : Type} [IsCType In] [IsCType Out] [IsCUnOp Op] [UnOpSig Op In Out] :
    Op → CExpr Γ In → CExpr Γ Out
| binop {Op In Out : Type} [IsCType In] [IsCType Out] [IsCBinOp Op] [BinOpSig Op In Out] :
    Op → CExpr Γ In → CExpr Γ In → CExpr Γ Out
| cast  {α β : Type} [IsCType α] [IsCType β] :
    CExpr Γ α → ValidCast α β → CExpr Γ β
| deref {γ β : Type} [IsCType γ] [IsCType β] [IsPointedCType β] [CArray γ] :
    CExpr Γ γ → γ = CPointerType β → CArray.isAllocated γ → CExpr Γ β
| addr  {α β : Type} [IsCType α] [IsCType β] :
    CVarRef Γ α → β = CPointerType α → CExpr Γ β
| index {γ β : Type} {n : Nat} [IsCType γ] [IsCType β] [IsPointedCType β] [CArray γ] :
    CExpr Γ γ → γ = CGlobalStaticMemoryBlock β n → (ι : Type) → [CIndex ι] →
    CArray.within_bounds γ ι → CExpr Γ β
| field {S α : Type} [IsCType S] [IsCType α] :
    CExpr Γ S → (idx : Nat) → [HasStructField S idx α] → CExpr Γ α
| tern  {α : Type} [IsCType α] :
    CExpr Γ (CIntType .I32 true) → CExpr Γ α → CExpr Γ α → CExpr Γ α
| call  {α : Type} [IsCType α] :
    (fname : String) → (argStrs : List String) →
    (argTime argMem declaredTime declaredMem : Nat → Nat) → CExpr Γ α

/-- Untyped call-arg syntax (F5 fallback implemented, not deferred).

`RawExpr` never mentions `Γ` or `CExpr`, so `List RawExpr` as a `call`
payload causes no kernel nesting rejection and no mutual-elimination
block. `rawTimeBound`/`rawMemBound` give the summed arg costs by
recursion (closed `Nat`s, lifted via `rawArgsTimeFn`/`rawArgsMemFn`);
`mkCallWithRaw` builds a `CExpr.call` with `argTime/argMem` computed
(not trusted). `declaredTime/declaredMem : Nat → Nat` still come from
the callee's `CFunc` spec and are discharged by
`Program.callResolves` — a program is a proof that every call's
declared bounds match the registry. -/
inductive RawExpr where
| intLit (v : Int) : RawExpr
| strLit (s : String) : RawExpr
| var (idx : Nat) : RawExpr
| add (l r : RawExpr) : RawExpr
| callRaw (fname : String) (args : List RawExpr) : RawExpr

/-- Closed time bound of untyped syntax: `+1` per node, call adds
dispatch `+1` on top of summed args (callee body excluded — counted
via `declaredTime` at `CExpr.call`). -/
def rawTimeBound : RawExpr → Nat
| .intLit _ => 0
| .strLit _ => 1
| .var _ => 1
| .add l r => rawTimeBound l + rawTimeBound r + 1
| .callRaw _ args => (args.map rawTimeBound).sum + 1

/-- Closed memory bound of untyped syntax: high-water `max`
(`strLit` = byte length, others `0` except `add` max; `callRaw` maxes
over args — callee mem added at `CExpr` level via `declaredMem`,
so nested `puts(f(x))` keeps the inner high-water, not `0`). -/
def rawMemBound : RawExpr → Nat
| .intLit _ => 0
| .strLit s => s.length
| .var _ => 0
| .add l r => Nat.max (rawMemBound l) (rawMemBound r)
| .callRaw _ args => (args.map rawMemBound).foldl (fun acc m => Nat.max acc m) 0

/-- Emit untyped syntax (proof-free, for `argStrs` construction). -/
def emitRaw : RawExpr → String
| .intLit v => toString v
| .strLit s => "\"" ++ s ++ "\""
| .var idx => s!"x{idx}"
| .add l r => s!"({emitRaw l} + {emitRaw r})"
| .callRaw f args => s!"{f}({String.intercalate ", " (args.map emitRaw)})"

/-- Summed arg costs for a `List RawExpr` (what `CExpr.call` stores
as `argTime/argMem`, lifted to cost functions — args are closed syntax
so the fn is const; `n`-variation enters via `declaredTime/Mem`). -/
def rawArgsTime (args : List RawExpr) : Nat :=
  (args.map rawTimeBound).sum

def rawArgsMem (args : List RawExpr) : Nat :=
  args.foldl (fun acc a => Nat.max acc (rawMemBound a)) 0

/-- Lifted arg-cost functions (N1): closed sums as `Nat → Nat`. -/
def rawArgsTimeFn (args : List RawExpr) : Nat → Nat :=
  fun _ => rawArgsTime args

def rawArgsMemFn (args : List RawExpr) : Nat → Nat :=
  fun _ => rawArgsMem args

/-- Collect all `callRaw` fnames in untyped syntax (P1-A1): `callRaw`
conses its fname plus flatMap over args; `add` appends; leaves `[]`.
Used by `Func.exprNested` for nested obligations (D12) and by the
first-order restriction (`rawIsFirstOrder`) below. -/
def rawCallFnames : RawExpr → List String
  | .callRaw f args => f :: (args.flatMap rawCallFnames)
  | .add l r => rawCallFnames l ++ rawCallFnames r
  | _ => []

/-- First-order restriction (P1-A4, agent's choice): `RawExpr` args lifted
to `CExpr` via `mkCallWithRaw` must contain no nested `callRaw`
(`rawCallFnames = []`). `nestedRaw` stays a `rawMemBound`-only example,
never lifted. Nested `fname`s that *are* lifted become `exprNested`
obligations discharged by `ProgramNestedResolve`; this predicate documents
the discipline that `CExpr.call`'s erased `argStrs` carry no hidden calls.
See `doc/stdlib_friction.md` F13. -/
def rawIsFirstOrder : RawExpr → Prop
  | r => rawCallFnames r = []

/-- List version: all args first-order (no nested `callRaw`). -/
def rawArgsFirstOrder (args : List RawExpr) : Prop :=
  args.flatMap rawCallFnames = []

/-- Checked `call` constructor: `argTime/argMem` by computation from
`args` (as const fns), `argStrs` by emission. P2 E2: takes
`(h : rawArgsFirstOrder args)` — nested lift fails to typecheck.
Only `declaredTime/declaredMem : Nat → Nat` remain as explicit assumptions
(discharged by `Program`). -/
def mkCallWithRaw {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (fname : String) (args : List RawExpr)
    (_h : rawArgsFirstOrder args)
    (declaredTime declaredMem : Nat → Nat) : CExpr Γ α :=
  CExpr.call fname (args.map emitRaw)
    (rawArgsTimeFn args) (rawArgsMemFn args) declaredTime declaredMem

/-- WHAT `emitExpr` produces: minimal C syntax for every ctor (proof args
are erased — they have no runtime content). Operators emit via their
`IsCUnOp`/`IsCBinOp` instances — no closed match on all ops. -/
def emitExpr {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → String
| .lit l _ => emitLit l
| .var (@CVarRef.mk _ _ _ _ idx _ _) => s!"x{idx}"
| @CExpr.unop _ _ Op In _ _ _ _ _ op e =>
    s!"({emitUnOp (Op := Op) op}{emitExpr (α := In) e})"
| @CExpr.binop _ _ Op In _ _ _ _ _ op l r =>
    s!"({emitExpr (α := In) l} {emitBinOp (Op := Op) op} {emitExpr (α := In) r})"
| @CExpr.cast _ _ _ _ _ _ e _ => s!"((cast){emitExpr e})"
| @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => s!"(*{emitExpr e})"
| @CExpr.addr _ _ _ _ _ _ v _ => s!"&x{v.toNat}"
| @CExpr.index _ _ _ _ _ _ _ _ _ base _ ι _ _ => s!"{emitExpr base}[{CIndex.value ι}]"
| @CExpr.field _ _ _ _ _ _ base idx _ => s!"{emitExpr base}.f{idx}"
| .tern g t e => s!"({emitExpr g} ? {emitExpr t} : {emitExpr e})"
| .call fname argStrs _ _ _ _ => s!"{fname}({String.intercalate ", " argStrs})"

/-! ## Per-operation typing facts (the payoff for per-op types)

The `BinOpSig` instance *is* the typing rule, resolved by typeclass
search per `Op`. Arithmetic preserves (`Out = In`, so the input's
`IsIntegerNonnegative` instance carries over); comparison/logic return
signed `I32` by construction. -/

/-- `add` preserves the operand type (hence unsigned): output `Out`
unifies with input `In`, so `[IsIntegerNonnegative In]` is still the
output's marker — no cast, no side condition on `op`. -/
theorem add_preserves_type {Γ : Type v} [CContext Γ]
    {α : Type} [IsCType α] [IsIntegerType α]
    (l r : CExpr Γ α) :
    ∃ (e : CExpr Γ α), e = CExpr.binop (Op := CAddOp) .add l r :=
  ⟨_, rfl⟩

/-- `add` on unsigned yields unsigned-typed result: same `α`. Demonstrated
on concrete unsigned `CUInt32Type`. -/
example {Γ : Type v} [CContext Γ]
    (l r : CExpr Γ CUInt32Type) :
    CExpr Γ CUInt32Type :=
  CExpr.binop (Op := CAddOp) .add l r

example {Γ : Type v} [CContext Γ]
    (_ : CExpr Γ CUInt32Type) :
    IsIntegerNonnegative CUInt32Type :=
  inferInstance

/-- `lt` returns signed `I32`, never the operand type. -/
example {Γ : Type v} [CContext Γ]
    {α : Type} [IsCType α] [IsIntegerType α]
    (l r : CExpr Γ α) :
    CExpr Γ (CIntType .I32 true) :=
  CExpr.binop (Op := CLtOp) .lt l r

/-- `&&` returns signed `I32` as well. -/
example {Γ : Type v} [CContext Γ]
    {α : Type} [IsCType α] [IsIntegerType α]
    (l r : CExpr Γ α) :
    CExpr Γ (CIntType .I32 true) :=
  CExpr.binop (Op := CLandOp) .land l r

/-- `!x` returns signed `I32`. -/
example {Γ : Type v} [CContext Γ]
    {α : Type} [IsCType α] [IsIntegerType α]
    (e : CExpr Γ α) :
    CExpr Γ (CIntType .I32 true) :=
  CExpr.unop (Op := CNotOp) .not e

/-- `-x` preserves (signed or unsigned input stays same type). -/
example {Γ : Type v} [CContext Γ]
    {α : Type} [IsCType α] [IsIntegerType α]
    (e : CExpr Γ α) : CExpr Γ α :=
  CExpr.unop (Op := CNegOp) .neg e

end LeanC
