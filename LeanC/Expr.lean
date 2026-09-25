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
not mutually). Draft workaround (friction F5): `call` takes
`argStrs : List String` (emitted args) instead of typed args.
Restoring typed args needs a non-mutual encoding
(e.g. `ULift` or well-founded recursion over untyped `RawExpr`).
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

/-- Intrinsically typed C expression. `Γ` is the context type, `α` the C
type (`Type 0`). Memory safety by proof args (`isAllocated`/
`within_bounds` — no proof = no term). `deref`/`index`/`addr` carry
equality proofs (like `CLiteral`) so `emitExpr` matches without
refining indices — required for dependent elimination with multiple
GADT ctors (see friction F5b). `unop`/`binop` carry their operator type
`Op` plus `UnOpSig`/`BinOpSig` typing (see `Ops.lean`) — illegal
`lt`-returns-pointer terms are unrepresentable. -/
inductive CExpr (Γ : Type v) [CContext Γ] : (α : Type) → [IsCType α] → Type 1 where
| lit   {α : Type} [IsCType α] : CLiteral α → CExpr Γ α
| var   {α : Type} [IsCType α] : CVarRef Γ α → CExpr Γ α
| unop  {Op In Out : Type} [IsCType In] [IsCType Out] [IsCUnOp Op] [UnOpSig Op In Out] :
    Op → CExpr Γ In → CExpr Γ Out
| binop {Op In Out : Type} [IsCType In] [IsCType Out] [IsCBinOp Op] [BinOpSig Op In Out] :
    Op → CExpr Γ In → CExpr Γ In → CExpr Γ Out
| cast  {α β : Type} [IsCType α] [IsCType β] :
    CExpr Γ α → (α → β → Prop) → CExpr Γ β
| deref {γ β : Type} [IsCType γ] [IsCType β] [IsPointedCType β] [CArray γ] :
    CExpr Γ γ → γ = CPointerType β → CArray.isAllocated γ → CExpr Γ β
| addr  {α β : Type} [IsCType α] [IsCType β] :
    CVarRef Γ α → β = CPointerType α → CExpr Γ β
| index {γ β : Type} {n : Nat} [IsCType γ] [IsCType β] [IsPointedCType β] [CArray γ] :
    CExpr Γ γ → γ = CGlobalStaticMemoryBlock β n → (ι : Type) → [CIndex ι] →
    CArray.within_bounds γ ι → CExpr Γ β
| field {S α : Type} [IsCType S] [IsCType α] :
    CExpr Γ S → (idx : Nat) → (h : True) → CExpr Γ α
| tern  {α : Type} [IsCType α] :
    CExpr Γ (CIntType .I32 true) → CExpr Γ α → CExpr Γ α → CExpr Γ α
| call  {α : Type} [IsCType α] :
    (fname : String) → (argStrs : List String) → CExpr Γ α

/-- WHAT `emitExpr` produces: minimal C syntax for every ctor (proof args
are erased — they have no runtime content). Operators emit via their
`IsCUnOp`/`IsCBinOp` instances — no closed match on all ops. -/
def emitExpr {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → String
| .lit l => emitLit l
| .var (.mk idx) => s!"x{idx}"
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
| .call fname argStrs => s!"{fname}({String.intercalate ", " argStrs})"

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
