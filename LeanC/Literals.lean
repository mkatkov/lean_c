import LeanC.Types
import LeanC.TypeClasses
import LeanC.Arrays
import LeanC.Context
import LeanC.Complexity

/-!
# Literals — bound-carrying, intrinsically typed C literals (T1)

`CLiteral α` is indexed by its C type so the serializer + checker collapse
into one (D4). Cost split (D1): pure values are `(Zero,Zero)`, allocating
literals (string/table lowering to `CGlobalStaticMemoryBlock`) are `(O1,O1)`
with `mem = const cells`. `O1` means `≤ K`, never "exactly 1 step".

Table element-type choice (open question in `doc/next_task.md` §9): all-`Int`
`Vector Int n` (frozen fallback). Rationale: C `int` table covers the stdlib
pilot (`aget`/map-step bodies); per-type vectors would duplicate one ctor per
width. Recorded alternative in `doc/stdlib_friction.md` F1.
-/

namespace LeanC

/-- Intrinsically typed C literal. Equality proofs are trailing `α = …` args
so `rfl` discharges them at use sites.
NOTE (universe): `α : Type` (i.e. `Type 0`), not `Type u` — all concrete C
types (`CIntType`, `CCharType`, …) live in `Type 0`, so a polymorphic `Type u`
would make every `rfl` ill-typed (`Type 0 ≠ Type u`). Same for `β`. -/
inductive CLiteral (α : Type) [IsCType α] where
| intLit   : (sz : CIntSize) → (sgn : Bool) → (v : Int) → α = CIntType sz sgn → CLiteral α
| charLit  : (v : UInt8) → α = CCharType Unit → CLiteral α
| floatLit : (sz : CFloatSize) → (v : Float) → α = CFloatType sz → CLiteral α
| strLit   : (s : String) → α = CGlobalStaticMemoryBlock (CCharType Unit) s.length → CLiteral α
| tableLit : {β : Type} → [IsPointedCType β] → (n : Nat) → (elems : Vector Int n)
    → α = CGlobalStaticMemoryBlock β n → CLiteral α

/-- WHAT `litBound` costs: pure values are free `(Zero,Zero)`; allocating
literals cost `(O1,O1)` with `mem = const cells` (`cells = s.length` for
strings, `cells = n` for tables — size_of factor is 1 here; see friction F2).
Downgrade path (D1): if codegen shows load is free, change str/table time to
`gZero` — one-line change, lemmas survive via `bigO_zero_le_one`. -/
def litBound {α} [IsCType α] : CLiteral α → ResourceBound
| .intLit _ _ _ _ => { timeRep := gZero, memRep := gZero }
| .charLit _ _ => { timeRep := gZero, memRep := gZero }
| .floatLit _ _ _ => { timeRep := gZero, memRep := gZero }
| .strLit s _ => { timeRep := g1, memRep := fun _ => s.length }
| @CLiteral.tableLit _ _ _ _ n _ _ => { timeRep := g1, memRep := fun _ => n }

/-- Range bounds for `intLit` (`lo, hi` inclusive). -/
def intMinMax : CIntSize → Bool → Int × Int
| .I8, true => (-128, 127)
| .I8, false => (0, 255)
| .I16, true => (-32768, 32767)
| .I16, false => (0, 65535)
| .I32, true => (-2147483648, 2147483647)
| .I32, false => (0, 4294967295)
| .I64, true => (-9223372036854775808, 9223372036854775807)
| .I64, false => (0, 18446744073709551615)

/-- WHAT `litFitsType` checks: signed/unsigned range for `intLit`;
float/char/str/table return `true` (TODO: float range, char always fits). -/
def litFitsType {α} [IsCType α] : CLiteral α → Bool
| .intLit sz sgn v _ =>
    let (lo, hi) := intMinMax sz sgn
    decide (lo ≤ v ∧ v ≤ hi)
| .charLit _ _ => true
| .floatLit _ _ _ => true
| .strLit _ _ => true
| @CLiteral.tableLit _ _ _ _ _ _ _ => true

/-- WHAT `emitLit` produces: minimal C syntax (`123u`, `'a'`, `"s"`, `{…}`). -/
def emitLit {α} [IsCType α] : CLiteral α → String
| .intLit _ sgn v _ => if sgn then toString v else toString v ++ "u"
| .charLit v _ => "'" ++ toString (Char.ofNat v.toNat) ++ "'"
| .floatLit _ v _ => toString v
| .strLit s _ => "\"" ++ s ++ "\""
| @CLiteral.tableLit _ _ _ _ _ elems _ =>
    "{" ++ String.intercalate ", " ((elems.toList).map toString) ++ "}"

/-- Pure literals are free in time, hence `=O 1` (Zero ≤ O1). -/
theorem lit_pure_O1_time {α} [IsCType α] (l : CLiteral α) :
    BigO (litBound l).timeRep g1 := by
  cases l <;> simp only [litBound] <;>
    (first | exact bigO_zero_le_one | exact BigO.refl _)

/-- Every literal's memory footprint is `=O 1` (const cells fold via
`const_le_one`; pure case via `Zero ≤ O1`). -/
theorem lit_mem_O1 {α} [IsCType α] (l : CLiteral α) :
    BigO (litBound l).memRep g1 := by
  cases l <;> simp only [litBound] <;>
    (first | exact bigO_zero_le_one | exact BigO.const_le_one _ (fun _ => Nat.le_refl _))

/-- `intLit` example: 42 fits in signed I32. -/
example : litFitsType (α := CIntType .I32 true) (.intLit .I32 true 42 rfl) = true := by
  decide

/-- `strLit` bound is `(O1,O1)`. -/
example : BigO (litBound (α := CGlobalStaticMemoryBlock (CCharType Unit) 5)
    (.strLit "hello" rfl)).timeRep g1 :=
  BigO.refl _

/-- `tableLit` memory is const (`=O 1`). -/
example : BigO (litBound (α := CGlobalStaticMemoryBlock (CIntType .I32 true) 2)
    (.tableLit (β := CIntType .I32 true) 2 ⟨#[1, 2], by decide⟩ rfl)).memRep g1 :=
  BigO.const_le_one _ (fun _ => Nat.le_refl _)

end LeanC
