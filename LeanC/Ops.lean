import LeanC.Types
import LeanC.TypeClasses
import LeanC.Values

/-!
# Operators — per-operation types + grouping classes (open extension)

Why not one `inductive CBinOp` with 15 cases (old design): a single
closed type forces `binop : CBinOp → Expr α → Expr α → Expr α`
(homogeneous, same-type-in/out for every op). Consequences:
- `lt`/`land` (which in C return `int`) could produce *any* `α`
  (e.g. a pointer) — unsound, and no per-op typing rule can be stated.
- "sum of unsigned is unsigned" is trivial-but-meaningless (output is
  `α` by construction for *every* op, including `lt`), so the real
  property — "`add` preserves unsigned *while `lt` returns signed
  int*" — is inexpressible without `if op = .add then …` hypotheses
  threaded through every lemma.

Design (same pattern as `CTypes`: one type per case, classes for groups):
- Each operator is its own singleton type (`CAddOp`, `CLtOp`, …) with
  one constructor (kept named `.add`, `.lt`, … so `.add` syntax keeps
  working — `Op` is inferred from the constructor).
- `IsCUnOp`/`IsCBinOp` are the base emission classes.
- Group classes mirror C families: arithmetic / comparison / bitwise /
  shift / logic (binary) and arithmetic / logic / bitwise (unary).
- `UnOpSig Op In Out` / `BinOpSig Op In Out` are the typing rules:
  arithmetic/bitwise/shift preserve (`In = Out`, needs
  `[IsIntegerType In]`); comparison/logic/logical-not return signed
  `I32` (`Out = CIntType .I32 true`). Adding a new operator (e.g.
  C11 `_Generic` op, embedded saturating-add) is a new type + instances
  in your own file — no edits here.

Unsigned preservation (the motivating theorem) now reads:
`add` on `α` with `[IsIntegerNonnegative α]` yields `α` (same instance
carries over), while `lt` yields signed `I32` — see `BinOpSig` instances
and `add_preserves_unsigned`/`lt_returns_signed` in `Expr.lean`.
`IsIntegerNonnegative` (see `Values.lean`) is the existing "unsigned"
marker (`CIntType sz false`). -/

namespace LeanC

/-! ## Unary operators: one type per op -/

inductive CNegOp where | neg deriving DecidableEq, Repr
inductive CNotOp where | not deriving DecidableEq, Repr
inductive CBnotOp where | bnot deriving DecidableEq, Repr

/-- Base class for unary operators (emission). -/
class IsCUnOp (Op : Type) where
  emitUn : Op → String

instance : IsCUnOp CNegOp where emitUn | .neg => "-"
instance : IsCUnOp CNotOp where emitUn | .not => "!"
instance : IsCUnOp CBnotOp where emitUn | .bnot => "~"

/-- Group: arithmetic unary (`-x`). -/
class IsArithUnOp (Op : Type) extends IsCUnOp Op where
/-- Group: logical unary (`!x` → int). -/
class IsLogicUnOp (Op : Type) extends IsCUnOp Op where
/-- Group: bitwise unary (`~x`). -/
class IsBitUnOp (Op : Type) extends IsCUnOp Op where

instance : IsArithUnOp CNegOp where emitUn | .neg => "-"
instance : IsLogicUnOp CNotOp where emitUn | .not => "!"
instance : IsBitUnOp CBnotOp where emitUn | .bnot => "~"

/-- Typing rule for unary operators: `Op` maps `In` to `Out`. -/
class UnOpSig (Op In Out : Type) where

instance [IsIntegerType α] : UnOpSig CNegOp α α where
instance [IsIntegerType α] : UnOpSig CBnotOp α α where
instance [IsIntegerType α] : UnOpSig CNotOp α (CIntType .I32 true) where

/-- Emit (generic over `Op`). Keeps old `emitUnOp op` call shape. -/
def emitUnOp {Op : Type} [IsCUnOp Op] (op : Op) : String :=
  IsCUnOp.emitUn op

/-! ## Binary operators: one type per op -/

inductive CAddOp where | add deriving DecidableEq, Repr
inductive CSubOp where | sub deriving DecidableEq, Repr
inductive CMulOp where | mul deriving DecidableEq, Repr
inductive CDivOp where | div deriving DecidableEq, Repr
inductive CModOp where | mod deriving DecidableEq, Repr
inductive CLtOp where | lt deriving DecidableEq, Repr
inductive CLeOp where | le deriving DecidableEq, Repr
inductive CEqOp where | eq deriving DecidableEq, Repr
inductive CAndOp where | and deriving DecidableEq, Repr
inductive COrOp where | or deriving DecidableEq, Repr
inductive CXorOp where | xor deriving DecidableEq, Repr
inductive CShlOp where | shl deriving DecidableEq, Repr
inductive CShrOp where | shr deriving DecidableEq, Repr
inductive CLandOp where | land deriving DecidableEq, Repr
inductive CLorOp where | lor deriving DecidableEq, Repr

/-- Base class for binary operators (emission). -/
class IsCBinOp (Op : Type) where
  emitBin : Op → String

instance : IsCBinOp CAddOp where emitBin | .add => "+"
instance : IsCBinOp CSubOp where emitBin | .sub => "-"
instance : IsCBinOp CMulOp where emitBin | .mul => "*"
instance : IsCBinOp CDivOp where emitBin | .div => "/"
instance : IsCBinOp CModOp where emitBin | .mod => "%"
instance : IsCBinOp CLtOp where emitBin | .lt => "<"
instance : IsCBinOp CLeOp where emitBin | .le => "<="
instance : IsCBinOp CEqOp where emitBin | .eq => "=="
instance : IsCBinOp CAndOp where emitBin | .and => "&"
instance : IsCBinOp COrOp where emitBin | .or => "|"
instance : IsCBinOp CXorOp where emitBin | .xor => "^"
instance : IsCBinOp CShlOp where emitBin | .shl => "<<"
instance : IsCBinOp CShrOp where emitBin | .shr => ">>"
instance : IsCBinOp CLandOp where emitBin | .land => "&&"
instance : IsCBinOp CLorOp where emitBin | .lor => "||"

/-- Group: arithmetic (`+ - * / %`, preserve type). -/
class IsArithBinOp (Op : Type) extends IsCBinOp Op where
/-- Group: comparison (`< <= ==`, return signed int). -/
class IsCmpBinOp (Op : Type) extends IsCBinOp Op where
/-- Group: bitwise (`& | ^`, preserve type). -/
class IsBitwiseBinOp (Op : Type) extends IsCBinOp Op where
/-- Group: shift (`<< >>`, preserve type). -/
class IsShiftBinOp (Op : Type) extends IsCBinOp Op where
/-- Group: logic (`&& ||`, return signed int). -/
class IsLogicBinOp (Op : Type) extends IsCBinOp Op where

instance : IsArithBinOp CAddOp where emitBin | .add => "+"
instance : IsArithBinOp CSubOp where emitBin | .sub => "-"
instance : IsArithBinOp CMulOp where emitBin | .mul => "*"
instance : IsArithBinOp CDivOp where emitBin | .div => "/"
instance : IsArithBinOp CModOp where emitBin | .mod => "%"
instance : IsCmpBinOp CLtOp where emitBin | .lt => "<"
instance : IsCmpBinOp CLeOp where emitBin | .le => "<="
instance : IsCmpBinOp CEqOp where emitBin | .eq => "=="
instance : IsBitwiseBinOp CAndOp where emitBin | .and => "&"
instance : IsBitwiseBinOp COrOp where emitBin | .or => "|"
instance : IsBitwiseBinOp CXorOp where emitBin | .xor => "^"
instance : IsShiftBinOp CShlOp where emitBin | .shl => "<<"
instance : IsShiftBinOp CShrOp where emitBin | .shr => ">>"
instance : IsLogicBinOp CLandOp where emitBin | .land => "&&"
instance : IsLogicBinOp CLorOp where emitBin | .lor => "||"

/-- Typing rule for binary operators: `Op` maps `In` (both operands,
homogeneous — C promotions beyond this are future work) to `Out`.
Arithmetic/bitwise/shift preserve; comparison/logic return signed
`I32` (C `int` for `0`/`1`). All need integer operands in this draft
(float arith is an added instance, no edits to `Expr`). -/
class BinOpSig (Op In Out : Type) where

instance [IsIntegerType α] : BinOpSig CAddOp α α where
instance [IsIntegerType α] : BinOpSig CSubOp α α where
instance [IsIntegerType α] : BinOpSig CMulOp α α where
instance [IsIntegerType α] : BinOpSig CDivOp α α where
instance [IsIntegerType α] : BinOpSig CModOp α α where
instance [IsIntegerType α] : BinOpSig CLtOp α (CIntType .I32 true) where
instance [IsIntegerType α] : BinOpSig CLeOp α (CIntType .I32 true) where
instance [IsIntegerType α] : BinOpSig CEqOp α (CIntType .I32 true) where
instance [IsIntegerType α] : BinOpSig CAndOp α α where
instance [IsIntegerType α] : BinOpSig COrOp α α where
instance [IsIntegerType α] : BinOpSig CXorOp α α where
instance [IsIntegerType α] : BinOpSig CShlOp α α where
instance [IsIntegerType α] : BinOpSig CShrOp α α where
instance [IsIntegerType α] : BinOpSig CLandOp α (CIntType .I32 true) where
instance [IsIntegerType α] : BinOpSig CLorOp α (CIntType .I32 true) where

/-- Emit (generic over `Op`). Keeps old `emitBinOp op` call shape. -/
def emitBinOp {Op : Type} [IsCBinOp Op] (op : Op) : String :=
  IsCBinOp.emitBin op

end LeanC
