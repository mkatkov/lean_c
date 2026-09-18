import LeanC.Types
import LeanC.TypeClasses
import LeanC.Arrays
import LeanC.Context
import LeanC.Complexity
import LeanC.Processes
import LeanC.Literals
import LeanC.Variables

/-!
# Expressions — intrinsically typed, bound-carrying C expressions (T4)

Stages (D3): A (`lit/var/unop/binop/cast`) lands first; B adds
`deref/addr/index/field` (proof-carrying memory access); C adds `tern`
(guard + `branchBound`) + `call` (`declaredCost`, breaking the
`call ↔ Func` cycle — `Expr` never imports `Func`).

Universe note: `α : Type` (`Type 0`), not `Type u` — concrete C types live
in `Type 0` (see `Literals.lean`). `index` uses `CGlobalStaticMemoryBlock`
(`Type 0`), NOT `CArrayType` (`Type 1`): `CArrayType α n` needs
`n : Type (u+1)`, which would force `CExpr` into `Type 1` for all `α` and
break `lit` (`CLiteral : Type 0`). See friction F3. `field` is a stub
(`base + idx + True` proof). `addr` carries an equality proof
`β = CPointerType α` (like `CLiteral`) so `exprBound`/`emitExpr` match
without refining the fixed result index.

`within_bounds` note: `CConstIndex/CFixedIndex/CVarIndex` all live in
`Type (u+1)`, but `CArray.within_bounds (β : Type)` expects `Type 0` —
so they cannot be used as access indices (see friction F4). This file
provides `CNatIndex : Type 0` for the pilot; `CConstIndex` remains the
size descriptor for `CArrayType` (unused here).

`call` args note: spec writes `List (Σ α, CExpr Γ α)`, but `Sigma`
nesting with local `Γ` is rejected by the kernel, and a mutual
`CExpr`/`CExprArg` wrapper breaks dependent elimination for `deref`/
`index` (input index computed from result index — supported singly,
not mutually). Draft workaround (friction F5): `call` takes pre-folded
`argsBound : ResourceBound` + `argStrs : List String` (emitted args)
instead of typed args. Callers (stdlib pilot) fold `exprBound`/`emitExpr`
of the real args beforehand — the link is by construction, not
machine-checked. Restoring typed args needs a non-mutual encoding
(e.g. `ULift` or well-founded recursion over untyped `RawExpr`).
-/

namespace LeanC
universe v

/-- Stage-A unary operators. -/
inductive CUnOp where
| neg | not | bnot
deriving DecidableEq, Repr

/-- Stage-A binary operators. -/
inductive CBinOp where
| add | sub | mul | div | mod
| lt | le | eq
| and | or | xor | shl | shr
| land | lor
deriving DecidableEq, Repr

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
equality proofs (like `CLiteral`) so `exprBound`/`emitExpr` match without
refining indices — required for dependent elimination with multiple
GADT ctors (see friction F5b). -/
inductive CExpr (Γ : Type v) [CContext Γ] : (α : Type) → [IsCType α] → Type 1 where
| lit   {α : Type} [IsCType α] : CLiteral α → CExpr Γ α
| var   {α : Type} [IsCType α] : CVarRef Γ α → CExpr Γ α
| unop  {α : Type} [IsCType α] : CUnOp → CExpr Γ α → CExpr Γ α
| binop {α : Type} [IsCType α] : CBinOp → CExpr Γ α → CExpr Γ α → CExpr Γ α
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
    (fname : String) → (argStrs : List String) → (argsBound : ResourceBound) →
    (declaredCost : ResourceBound) → CExpr Γ α

/-- WHAT `exprBound` costs: leaf `lit/var` reuse their bounds;
unary/binary/cast/deref/addr/index/field = sum of parts `+ 1` time,
`max` memory; `tern` = guard sequenced before `branchBound` (reuses
`branch_bound_max_time/mem`); `call` = `argsBound + declaredCost + 1`. -/
def exprBound {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → ResourceBound
| .lit l => litBound l
| .var _ => varBound
| .unop _ e =>
    { timeRep := fun n => (exprBound e).timeRep n + 1,
      memRep := (exprBound e).memRep }
| .binop _ l r =>
    { timeRep := fun n => (exprBound l).timeRep n + (exprBound r).timeRep n + 1,
      memRep := fun n => Nat.max ((exprBound l).memRep n) ((exprBound r).memRep n) }
| @CExpr.cast _ _ _ _ _ _ e _ =>
    { timeRep := fun n => (exprBound e).timeRep n + 1,
      memRep := (exprBound e).memRep }
| @CExpr.deref _ _ _ _ _ _ _ _ e _ _ =>
    { timeRep := fun n => (exprBound e).timeRep n + 1,
      memRep := (exprBound e).memRep }
| @CExpr.addr _ _ _ _ _ _ _ _ =>
    { timeRep := fun _ => 2, memRep := gZero }
| @CExpr.index _ _ _ _ _ _ _ _ _ e _ _ _ _ =>
    { timeRep := fun n => (exprBound e).timeRep n + 1,
      memRep := (exprBound e).memRep }
| @CExpr.field _ _ _ _ _ _ base _ _ =>
    { timeRep := fun n => (exprBound base).timeRep n + 1,
      memRep := (exprBound base).memRep }
| .tern g t e =>
    seqBound (exprBound g) (branchBound (exprBound t) (exprBound e))
| .call _ _ ab dc =>
    { timeRep := fun n => ab.timeRep n + dc.timeRep n + 1,
      memRep := fun n => Nat.max (ab.memRep n) (dc.memRep n) }

/-- Emit unary operator symbol. -/
def emitUnOp : CUnOp → String
| .neg => "-"
| .not => "!"
| .bnot => "~"

/-- Emit binary operator symbol. -/
def emitBinOp : CBinOp → String
| .add => "+"
| .sub => "-"
| .mul => "*"
| .div => "/"
| .mod => "%"
| .lt => "<"
| .le => "<="
| .eq => "=="
| .and => "&"
| .or => "|"
| .xor => "^"
| .shl => "<<"
| .shr => ">>"
| .land => "&&"
| .lor => "||"

/-- WHAT `emitExpr` produces: minimal C syntax for every ctor (proof args
are erased — they have no runtime content). -/
def emitExpr {Γ : Type v} [CContext Γ] {α : Type} [IsCType α] :
    CExpr Γ α → String
| .lit l => emitLit l
| .var (.mk idx) => s!"x{idx}"
| .unop op e => s!"({emitUnOp op}{emitExpr e})"
| .binop op l r => s!"({emitExpr l} {emitBinOp op} {emitExpr r})"
| @CExpr.cast _ _ _ _ _ _ e _ => s!"((cast){emitExpr e})"
| @CExpr.deref _ _ _ _ _ _ _ _ e _ _ => s!"(*{emitExpr e})"
| @CExpr.addr _ _ _ _ _ _ v _ => s!"&x{v.toNat}"
| @CExpr.index _ _ _ _ _ _ _ _ _ base _ ι _ _ => s!"{emitExpr base}[{CIndex.value ι}]"
| @CExpr.field _ _ _ _ _ _ base idx _ => s!"{emitExpr base}.f{idx}"
| .tern g t e => s!"({emitExpr g} ? {emitExpr t} : {emitExpr e})"
| .call fname argStrs _ _ => s!"{fname}({String.intercalate ", " argStrs})"

/-- Binop time composes by sum `+ 1` (definitional — the bound *is* the sum). -/
theorem expr_time_add {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (op : CBinOp) (l r : CExpr Γ α) :
    (exprBound (CExpr.binop op l r)).timeRep =
      fun n => (exprBound l).timeRep n + (exprBound r).timeRep n + 1 := rfl

/-- Binop memory composes by `max` (definitional). -/
theorem expr_mem_max {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (op : CBinOp) (l r : CExpr Γ α) :
    (exprBound (CExpr.binop op l r)).memRep =
      fun n => Nat.max ((exprBound l).memRep n) ((exprBound r).memRep n) := rfl

/-- `1 + 2` is `O(1)` (const folding via `const_le_one`: `0+0+1 = 1`). -/
example : BigO
    (exprBound (Γ := DraftCtx)
      (CExpr.binop (α := CIntType .I32 true) .add
        (CExpr.lit (.intLit .I32 true 1 rfl))
        (CExpr.lit (.intLit .I32 true 2 rfl)))).timeRep g1 := by
  apply BigO.const_le_one (K := 1)
  intro n
  exact Nat.le_refl _

/-- `tern` with `O(1)` parts stays `O(1)`: guard sequenced before
`branchBound`, all via `BigO.add`/`max_bound`/`const_le_one`.
Concrete instance (all leaves `O(1)`) folds to const `3`. -/
theorem tern_guard_still_O1 {Γ : Type v} [CContext Γ]
    (g : CExpr Γ (CIntType .I32 true))
    (t e : CExpr Γ (CIntType .I32 true))
    (hg : ∀ n, (exprBound g).timeRep n ≤ 1)
    (ht : ∀ n, (exprBound t).timeRep n ≤ 1)
    (he : ∀ n, (exprBound e).timeRep n ≤ 1) :
    BigO (exprBound (CExpr.tern g t e)).timeRep g1 := by
  apply BigO.const_le_one (K := 3)
  intro n
  simp only [exprBound, seqBound, branchBound]
  have hg1 := hg n
  have ht1 := ht n
  have he1 := he n
  have hmax : Nat.max ((exprBound t).timeRep n) ((exprBound e).timeRep n) ≤ 1 :=
    Nat.max_le.mpr ⟨ht1, he1⟩
  omega

/-- `call` time = `argsBound + declaredCost + 1` (definitional). -/
theorem call_time_eq {Γ : Type v} [CContext Γ] {α : Type} [IsCType α]
    (fname : String) (argStrs : List String) (ab dc : ResourceBound) :
    (exprBound (Γ := Γ) (CExpr.call (Γ := Γ) (α := α) fname argStrs ab dc)).timeRep =
      fun n => ab.timeRep n + dc.timeRep n + 1 := rfl

end LeanC
