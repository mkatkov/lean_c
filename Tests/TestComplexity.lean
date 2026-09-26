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

/-- 1c''. Semantic strictness: `<` is `BigO` + reverse `¬ BigO`
(`StrictQuantBelow`), not tag `≠`. Tag `≠` is syntactic freshness only;
a duplicate envelope under a fresh tag would pass `≤` both ways. -/
example : StrictQuantBelow (axis := .time) TimeComplexity_O1 TimeComplexity_log :=
  strict_o1_log_time
example : StrictQuantBelow (axis := .time) TimeComplexity_O1 (TimeComplexity_poly 2) :=
  strict_o1_poly_time (by decide : 1 ≤ 2)
example : StrictQuantBelow (axis := .time) TimeComplexity_log (TimeComplexity_poly 2) :=
  strict_log_poly_time (by decide : 2 ≤ 2)
example : StrictQuantBelow (axis := .memory) MemoryComplexity_O1 MemoryComplexity_log :=
  strict_o1_log_mem
example : StrictQuantBelow (axis := .time) TimeComplexity_O1 TimeComplexity_linear :=
  strict_o1_linear
example : StrictQuantBelow (axis := .time) TimeComplexity_linear (TimeComplexity_poly 2) :=
  strict_linear_poly2
example : ¬ BigO (gpoly 2) glog := not_bigO_sq_le_log
example : ¬ BigO (gpoly 2) (fun n => n) := not_bigO_sq_le_linear

/-- 1c'''. Memory mirror via the OPEN path (no new base tags by design):
`O1Mem < O_logMem < BOUNDED`, `polyMem`, and `polyMem 0 = O1Mem`. Same
reps as time, read as live cells. -/
example : QuantLE (axis := .memory) MemoryComplexity_O1 MemoryComplexity_log :=
  bigO_one_le_log
example : QuantLE (axis := .memory) MemoryComplexity_log (MemoryComplexity_poly 2) :=
  bigO_log_le_poly (by decide : 1 ≤ 2)
example : QuantLE (axis := .memory) (MemoryComplexity_poly 0) MemoryComplexity_O1 :=
  quant_mem_poly0_le_o1
example : QuantLE (axis := .memory) MemoryComplexity_O1 (MemoryComplexity_poly 0) :=
  quant_mem_o1_le_poly0
example : BigO g1 glog ∧
    StrictQuantBelow (axis := .memory) MemoryComplexity_O1 MemoryComplexity_log ∧
    can_insert_quant_to_list (axis := .memory) MemoryComplexity_log baseTimeMem :=
  mem_log_inserted
example : QuantLE (axis := .memory) (MemoryComplexity_poly 0) MemoryComplexity_O1 ∧
    QuantLE (axis := .memory) MemoryComplexity_O1 (MemoryComplexity_poly 0) ∧
    can_insert_quant_to_list (axis := .memory) (MemoryComplexity_poly 0) baseTimeMem :=
  mem_poly_zero_inserted

/-- 1c''''. Symmetric incomparabilities + `Zero`-vs-divergent: both
directions are `¬ False` (catch-all doing real work). -/
example : ¬ ComplexityLE TimeComplexity_UNBOUND TimeComplexity_HALTS :=
  complexityLE_unbound_not_halts
example : ¬ ComplexityLE MemoryComplexity_BOUNDED MemoryComplexity_GROWING :=
  complexityLE_bounded_not_growing
example : ¬ ComplexityLE MemoryComplexity_GROWING MemoryComplexity_BOUNDED :=
  complexityLE_growing_not_bounded
example : ¬ ComplexityLE ZeroTimeComplexity TimeComplexity_UNBOUND :=
  complexityLE_zero_not_unbound
example : ¬ ComplexityLE TimeComplexity_UNBOUND ZeroTimeComplexity :=
  complexityLE_unbound_not_zero
example : ¬ ComplexityLE MemoryComplexity_O1 ZeroMemoryComplexity :=
  complexityLE_o1Mem_not_zeroMem

/-- 1c'''''. Knowledge growth across lists is `graphInclusion`
(intra-list `LE` is equivalence by design; the class order lives on
tags/reps). -/
example : graphInclusion baseTimeMem baseTimeMem :=
  graphInclusion_refl _
example {l₁ l₂ l₃ : List Type} (h12 : graphInclusion l₁ l₂) (h23 : graphInclusion l₂ l₃) :
    graphInclusion l₁ l₃ :=
  graphInclusion_trans h12 h23
example : graphInclusion baseTimeMem
    (TimeComplexity_log :: baseTimeMem) :=
  graphInclusion_cons _ _
example : (TimeComplexity_log : Type) ∈ (TimeComplexity_log :: baseTimeMem) :=
  graphInclusion_head_mem _ _
example : TimeComplexity_O1 ∈ baseTimeMem := o1_mem_base
example : MemoryComplexity_O1 ∈ baseTimeMem := o1Mem_mem_base

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
time adds, memory maxes, `O1+O1=O1`, branch `O1` stays `O1`. `Zero` is
pointwise (`costInZero`), never `BigO`-based — constant-`1` is the
regression. `HALTS`/`BOUNDED` are existential (`IsFiniteCost`); every
`costInClass` promotes; divergence is empty under total costs. -/

example : costInClass (axis := .time) TimeCost TimeComplexity_O1 ⟨fun _ => 3⟩ :=
  time_const_in_o1 3
example : costInClass (axis := .memory) MemCost MemoryComplexity_O1 ⟨fun _ => 5⟩ :=
  mem_const_in_o1 5
example : costInClass (axis := .time) EnergyCost TimeComplexity_O1 ⟨fun _ => 4⟩ :=
  energy_const_in_o1 4
example : costInClass (axis := .time) ExactCount TimeComplexity_O1 ⟨7⟩ :=
  exact_const_in_o1_time 7
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
example {a b : EnergyCost} {ga gb : Nat → Nat}
    (ha : BigO a.val ga) (hb : BigO b.val gb) :
    BigO (CResource.seqCombine a b).val (fun n => ga n + gb n) :=
  energy_seq_preserves ha hb
example {a b : ExactCount} {ga gb : Nat → Nat}
    (ha : BigO (fun _ : Nat => a.val) ga)
    (hb : BigO (fun _ : Nat => b.val) gb) :
    BigO (HasCost.cost (CResource.seqCombine a b)) (fun n => ga n + gb n) :=
  exact_seq_preserves ha hb

/-- 1f. `Zero` exact vs `O1`: `zero` promotes, constant-`1` does not. -/
example : costInZero TimeCost (CResource.zero (R := TimeCost)) :=
  time_zero_in_zero
example : costInClass (axis := .time) TimeCost TimeComplexity_O1
    (CResource.zero (R := TimeCost)) :=
  time_zero_in_o1
example : ¬ costInZero TimeCost (⟨fun _ => 1⟩ : TimeCost) :=
  not_costInZero_time_one
example : ¬ costInZero MemCost (⟨fun _ => 1⟩ : MemCost) :=
  not_costInZero_mem_one

/-- 1g. Finite/divergent ceiling: every `costInClass` forgets into
`HALTS`/`BOUNDED`; every total cost is finite, so divergence is empty. -/
example {v : TimeCost} {C : Type} [HasQuantRep .time C]
    (h : costInClass (axis := .time) TimeCost C v) : costInHalts TimeCost v :=
  costInClass_to_halts h
example {v : MemCost} {C : Type} [HasQuantRep .memory C]
    (h : costInClass (axis := .memory) MemCost C v) : costInBounded MemCost v :=
  costInClass_to_bounded h
example (c : Nat → Nat) : IsFiniteCost c := every_cost_finite c
example (c : Nat → Nat) : ¬ IsDivergentCost c := no_divergent_cost c
example : costInHalts TimeCost ⟨fun _ => 3⟩ :=
  costInClass_to_halts (time_const_in_o1 3)
example : costInBounded MemCost ⟨fun _ => 5⟩ :=
  costInClass_to_bounded (mem_const_in_o1 5)
example : costInHalts TimeCost ⟨fun _ => 1⟩ :=
  costInClass_to_halts
    (costInClass_mono (C₂ := TimeComplexity_linear) (time_const_in_o1 1)
      (show QuantLE (axis := .time) TimeComplexity_O1 TimeComplexity_linear from
        linear_above_o1))


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
  -- strictness witnesses: n where the envelope separation bites
  -- `not_bigO_log_le_one` uses n = max N₀ 2^(c+1): with c = 2, n = 8
  ok := (← assertEq "3" (toString (Nat.log2 ((8 : Nat) + 1)))) && ok
  -- `self_le_pow`: 3 ≤ 3^2
  ok := (← assertEq "true" (toString (decide (3 ≤ (3 : Nat) ^ 2)))) && ok
  -- `not_bigO_sq_le_log` squeeze: (c+1)*n ≤ n*n with c = 2, n = 8
  ok := (← assertEq "24" (toString ((2 + 1) * (8 : Nat)))) && ok
  ok := (← assertEq "64" (toString ((8 : Nat) * 8))) && ok
  -- resource combines compute (per-resource composition, not just proofs)
  ok := (← assertEq "13"
    (toString ((CResource.seqCombine (⟨fun n => n + 1⟩ : TimeCost) ⟨fun n => n + 2⟩).val 5))) && ok
  ok := (← assertEq "2"
    (toString ((CResource.branchCombine (⟨fun _ => 1⟩ : TimeCost) ⟨fun _ => 1⟩).val 0))) && ok
  ok := (← assertEq "7"
    (toString ((CResource.seqCombine (⟨fun _ => 3⟩ : MemCost) ⟨fun _ => 7⟩).val 0))) && ok
  ok := (← assertEq "12"
    (toString ((CResource.seqCombine (⟨5⟩ : ExactCount) ⟨7⟩).val))) && ok
  ok := (← assertEq "7"
    (toString ((CResource.branchCombine (⟨5⟩ : ExactCount) ⟨7⟩).val))) && ok
  ok := (← assertEq "0" (toString ((CResource.zero (R := TimeCost)).val 42))) && ok
  -- `Zero` vs `O1`: zero computes 0, constant-1 computes 1 at every point
  ok := (← assertEq "1" (toString ((⟨fun _ => 1⟩ : TimeCost).val 0))) && ok
  -- knowledge base shape: 10 base classes
  ok := (← assertEq "10" (toString baseTimeMem.length)) && ok
  -- kit + order + insertion + extension all compiled
  -- (examples above); runtime confirms the computed witnesses
  IO.println "OK: BigO.refl / trans / one_le_log / log_le_sq + strictness (poly/log/sq diagonals)"
  IO.println "OK: Zero < O1 < HALTS < UNDECIDABLE, UNBOUND < UNDECIDABLE, both HALTS/UNBOUND dirs, Zero-vs-divergent, O1-vs-Zero (both axes)"
  IO.println "OK: O1 < O_log < HALTS via can_insert (BigO evidence); poly 2 + poly 0 = O1 (both axes, open+closed)"
  IO.println "OK: O_linear extension (own file, no base edits): O1/log < linear < poly2 + strict O1<linear<poly2"
  IO.println "OK: type-keyed resources (no strings/Option): set/get/exists + seq/branch instances (time/mem/energy/exact)"
  IO.println "OK: bridge costInClass + costInZero (not-O1-regression) + finite/divergent ceiling + preservation"
  if ok then
    IO.println "All complexity tests passed." *> pure 0
  else
    pure 1

end TestComplexity
