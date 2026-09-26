import LeanC.Complexity
import LeanC.Complexity.Bridge
import Examples.ComplexityLinear
import LeanC.Context
import Examples.Resources

open Lean IO

/- WHAT this file is: the acceptance suite for the complexity system +
generic resources — one checked `example` per required property
(compile = pass) plus a `test : IO UInt32` runner (exit `0` = pass)
wired into `test.lean`. WHY both levels: `example`s machine-check the
*proofs* (a broken edge fails `lake build`); the `IO` runner replays the
*computed witnesses* (`log2 9 = 3`, …) so `./.lake/build/bin/test`
independently confirms the numbers the proofs reason about. -/
namespace TestComplexity

open LeanC

/-- Dummy context for tests (empty store: nothing exists). -/
inductive DummyCtx where | mk

/-- Empty `Type 1` (no constructors) for `DummyCtx`'s absent resources. -/
inductive NoRes : Type 1 where

instance : CContext DummyCtx where
  isCContext := True
  resourceExists := (fun {_R} [_] _ => NoRes)
  getResource := (fun {_R} [_] _ h => nomatch h)
  setResource := (fun {_R} [_] ctx _ => ctx)

/-- 1a. Big-O kit smoke checks (compile-time). -/
example : BigO g1 g1 := BigO.refl _
example : BigO g1 glog := bigO_one_le_log
example : BigO glog (fun n => n ^ 2) := bigO_log_le_sq
example : BigO g1 (fun n => n ^ 2) :=
  BigO.trans bigO_one_le_log bigO_log_le_sq
example : BigO glog (gpoly 2) := bigO_log_le_poly (by decide : 1 ≤ 2)

/-- 1b. Base order checks derived from Big-O. `Zero` edges are by
stipulation (`trivial`), NOT `bigO_zero_le_one` — `bigO_one_le_zero`
also holds, so `BigO` cannot order the bottom (see `Lattice`). -/
example : ComplexityLE ZeroTimeComplexity TimeComplexity_O1 :=
  complexityLE_zero_o1
example : ComplexityLE TimeComplexity_O1 TimeComplexity_HALTS :=
  complexityLE_o1_halts
example : ComplexityLE TimeComplexity_HALTS TimeComplexity_UNDECIDABLE :=
  complexityLE_halts_undecidable
example : ComplexityLE TimeComplexity_UNBOUND TimeComplexity_UNDECIDABLE :=
  complexityLE_unbound_undecidable
example : ¬ ComplexityLE TimeComplexity_HALTS TimeComplexity_UNBOUND :=
  complexityLE_halts_not_unbound
/-- Separation witness: `O1 ≰ Zero` even though `BigO g1 gZero` holds. -/
example : ¬ ComplexityLE TimeComplexity_O1 ZeroTimeComplexity :=
  complexityLE_o1_not_zero
example : BigO g1 gZero := bigO_one_le_zero

/-- 1c. Intermediate insertion `O1 < O_log < HALTS` via `can_insert`.
WHY this is the extensibility bar: a new class is accepted only with
its two `BigO` edges as evidence (`can_insert_log` packages
`bigO_one_le_log` + forgetting into `HALTS`) — never by assertion. -/
example : ComplexityLE TimeComplexity_O1 TimeComplexity_log :=
  complexityLE_o1_log
example : ComplexityLE TimeComplexity_log TimeComplexity_HALTS :=
  complexityLE_log_halts
example : can_insert_complexity_class_to_graph (axis := .time)
    (τ := TimeComplexity_log) baseGraph :=
  can_insert_log
-- Deprecated typo spelling still resolves (alias).
example : can_insert_complexity_class_to_grapth (axis := .time)
    (τ := TimeComplexity_log) baseGraph :=
  can_insert_log

/-- `poly` insertion (`1 ≤ k`) and the degenerate `poly 0 = O1` case. -/
example : can_insert_complexity_class_to_graph (axis := .time)
    (τ := TimeComplexity_poly 2) baseGraph :=
  can_insert_poly (by decide : 1 ≤ 2)
example : can_insert_complexity_class_to_graph (axis := .time)
    (τ := TimeComplexity_poly 0) baseGraph :=
  can_insert_poly_zero
example : QuantLE (axis := .time) (TimeComplexity_poly 0) TimeComplexity_O1 :=
  quant_poly0_le_o1
example : QuantLE (axis := .time) TimeComplexity_O1 (TimeComplexity_poly 0) :=
  quant_o1_le_poly0

/-- 1c'. Extension without base edits: `O_linear` from
`ComplexityLinear.lean` (separate file, zero base changes) slots
`O1 < O_linear`, `O_log < O_linear < O_poly₂` via plain `BigO` on its
own rep — the open `HasQuantRep`/`QuantLE` path working end to end. -/
example : QuantLE (axis := .time) TimeComplexity_O1 TimeComplexity_linear :=
  linear_above_o1
example : QuantLE (axis := .time) TimeComplexity_log TimeComplexity_linear :=
  linear_above_log
example : QuantLE (axis := .time) TimeComplexity_linear (TimeComplexity_poly 2) :=
  linear_below_poly2
example : BigO g1 glinear ∧ BigO glinear (gpoly 2) ∧ BigO glog glinear ∧
    can_insert_quant_to_list (axis := .time) TimeComplexity_linear linearList :=
  linear_inserted

/-! ## 1d. Type-keyed resources — no strings, no `Option`.

Keys are types distinguished by `CResourceType` (`Examples/Resources.lean`,
not core). `resourceExists` is a `ResMem` witness in `Type` (head = newest,
tail = older); `getResource` takes its witness and returns the correct
type `R` directly — no `Option`, so callers prove properties about the
value itself. Combining uses `CResource` instances
(`seqCombine`/`branchCombine`).
-/

def existsAfterSet : CContext.resourceExists
    (R := TimeCost) (CContext.setResource (R := TimeCost) emptyDraft ⟨fun _ => 3⟩) :=
  draft_set_exists _ _ _

example : CContext.getResource (CContext.setResource (R := TimeCost) emptyDraft ⟨fun _ => 3⟩)
    (ResMem.head) = (⟨fun _ => 3⟩ : TimeCost) :=
  draft_get_set_same _ _ _

/-- Setting `TimeCost` preserves `ExactCount` looked up via an old tail
witness (no type disequality needed — the witness selects old vs new). -/
example (h : ResMem emptyDraft.rs ExactCount) :
    CContext.getResource
      (CContext.setResource (R := TimeCost) emptyDraft ⟨fun _ => 7⟩)
      (ResMem.tail h) =
      CContext.getResource emptyDraft h :=
  draft_get_set_other _ _ h

/-- Combining via instances: sequencing adds time; exact adds. -/
example : CResource.seqCombine (⟨fun n => n + 1⟩ : TimeCost) ⟨fun n => n + 2⟩ =
    ⟨fun n => (n + 1) + (n + 2)⟩ := rfl

example : CResource.branchCombine (⟨5⟩ : ExactCount) ⟨7⟩ = ⟨7⟩ := rfl

example : CResource.zero (R := ExactCount) = ⟨0⟩ := rfl

/-- Nothing exists in the empty draft (no `ResMem []` witness). -/
example : CContext.resourceExists (R := TimeCost) emptyDraft → False :=
  fun h => nomatch h

/-! ## 1e. Bridge: resource values in classes + preservation.

`costInClass` is `BigO (cost v) rep`; preservation is one `BigO.add` /
`max_bound` per combine. These examples lock the extended resource model:
time adds, memory maxes, `O1+O1=O1`, branch `O1` stays `O1`. -/

example : costInClass (axis := .time) TimeCost TimeComplexity_O1 ⟨fun _ => 3⟩ :=
  time_const_in_o1 3
example : costInClass (axis := .memory) MemCost MemoryComplexity_O1 ⟨fun _ => 5⟩ :=
  mem_const_in_o1 5
example : costInClass (axis := .time) TimeCost TimeComplexity_O1
    (CResource.seqCombine ⟨fun _ => 1⟩ ⟨fun _ => 2⟩) :=
  time_seq_o1 1 2
example : costInClass (axis := .time) TimeCost TimeComplexity_O1
    (CResource.branchCombine ⟨fun _ => 1⟩ ⟨fun _ => 1⟩) :=
  time_branch_o1
example {a b : TimeCost} {ga gb : Nat → Nat}
    (ha : BigO a.val ga) (hb : BigO b.val gb) :
    BigO (CResource.seqCombine a b).val (fun n => ga n + gb n) :=
  time_seq_preserves ha hb
example {a b : MemCost} {ga gb : Nat → Nat}
    (ha : BigO a.val ga) (hb : BigO b.val gb) :
    BigO (CResource.seqCombine a b).val (fun n => Nat.max (ga n) (gb n)) :=
  mem_seq_preserves ha hb

def assertEq (expected actual : String) : IO Bool :=
  if expected == actual then
    IO.println s!"OK: {actual}" *> pure true
  else
    IO.eprintln s!"FAIL: expected {expected} but got {actual}" *> pure false

def test : IO UInt32 := do
  let mut ok := true
  -- concrete witnesses behind the Big-O proofs
  ok := (← assertEq "3" (toString (Nat.log2 (8 + 1)))) && ok
  ok := (← assertEq "1" (toString (Nat.log2 (1 + 1)))) && ok
  ok := (← assertEq "2" (toString (1 + 1))) && ok
  ok := (← assertEq "1" (toString (Nat.max 1 1))) && ok
  -- kit + order + insertion + extension all compiled
  -- (examples above); runtime confirms the computed witnesses
  IO.println "OK: BigO.refl / trans / one_le_log / log_le_sq"
  IO.println "OK: Zero < O1 < HALTS < UNDECIDABLE, UNBOUND < UNDECIDABLE, ¬(HALTS ≤ UNBOUND), ¬(O1 ≤ Zero)"
  IO.println "OK: O1 < O_log < HALTS via can_insert (BigO evidence); poly 2 + poly 0 = O1"
  IO.println "OK: O_linear extension (own file, no base edits): O1/log < linear < poly2"
  IO.println "OK: type-keyed resources (no strings/Option): set/get/exists + seq/branch instances"
  IO.println "OK: bridge costInClass + preservation: time adds, memory maxes, O1+O1=O1"
  if ok then
    IO.println "All complexity tests passed." *> pure 0
  else
    pure 1

end TestComplexity
