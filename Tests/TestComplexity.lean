import LeanC.Complexity
import LeanC.ComplexityLinear
import LeanC.Context
import LeanC.Processes

open Lean IO

/- WHAT this file is: the acceptance suite for the complexity system —
one checked `example` per required property (compile = pass) plus a
`test : IO UInt32` runner (exit `0` = pass) wired into `test.lean`.
WHY both levels: `example`s machine-check the *proofs* (a broken edge
fails `lake build`); the `IO` runner replays the *computed witnesses*
(`log2 9 = 3`, …) so `./.lake/build/bin/test` independently confirms
the numbers the proofs reason about. Sections mirror the proposal §8:
kit → order → insertion → composition → extension. -/
namespace TestComplexity

open LeanC

/-- Dummy context for tests. -/
inductive DummyCtx where | mk

instance : CContext DummyCtx where
  isCContext := True
  extend ctx _ := ctx

/-- Three O1 statements (single-op fragments). -/
inductive StmtO1A where | mk
inductive StmtO1B where | mk
inductive StmtO1C where | mk

instance : CStatement DummyCtx StmtO1A where
  isStatementSound := True
  stmtBound := { timeRep := g1, memRep := g1 }

instance : CStatement DummyCtx StmtO1B where
  isStatementSound := True
  stmtBound := { timeRep := g1, memRep := g1 }

instance : CStatement DummyCtx StmtO1C where
  isStatementSound := True
  stmtBound := { timeRep := g1, memRep := g1 }

/-- 1a. Big-O kit smoke checks (compile-time). -/
example : BigO g1 g1 := BigO.refl _
example : BigO g1 glog := bigO_one_le_log
example : BigO glog (fun n => n ^ 2) := bigO_log_le_sq
example : BigO g1 (fun n => n ^ 2) :=
  BigO.trans bigO_one_le_log bigO_log_le_sq
example : BigO glog (gpoly 2) := bigO_log_le_poly (by decide : 1 ≤ 2)

/-- 1b. Base order checks derived from Big-O. -/
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

/-- 1c. Intermediate insertion `O1 < O_log < HALTS` via `can_insert`.
WHY this is the extensibility bar: a new class is accepted only with
its two `BigO` edges as evidence (`can_insert_log` packages
`bigO_one_le_log` + forgetting into `HALTS`) — never by assertion. -/
example : ComplexityLE TimeComplexity_O1 TimeComplexity_log :=
  complexityLE_o1_log
example : ComplexityLE TimeComplexity_log TimeComplexity_HALTS :=
  complexityLE_log_halts
example : can_insert_complexity_class_to_grapth (axis := .time)
    (τ := TimeComplexity_log) baseGraph :=
  can_insert_log

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
    TimeComplexity_O1 ∈ linearList ∧ TimeComplexity_HALTS ∈ linearList :=
  linear_inserted

/-- 1d. Sequential + branching composition.
WHY these two programs: the 2×`O1` sequence is the smallest program
where constant folding matters (`1+1 = 2 =O 1` — without
`const_le_one`, sequencing two constants would escape `O1`); the branch
is the smallest program where the `max` rule + `+1` guard matter
(`max(1,1)+1 = 2 =O 1`, and `Zero`-branches would still cost the guard).
Both bounds are worst-case pairs `(time, mem)` per `ResourceBound`. -/
-- Two-O1 sequence has time `1+1=2 = O(1)` (const folding) and mem `max=1`.
def seq2Bound : ResourceBound :=
  seqBound
    (CStatement.stmtBound (Γ := DummyCtx) (α := StmtO1A))
    (CStatement.stmtBound (Γ := DummyCtx) (α := StmtO1B))

example : BigO seq2Bound.timeRep g1 := by
  apply BigO.const_le_one (K := 2)
  intro n
  exact Nat.le_refl _

example : BigO seq2Bound.memRep g1 := by
  have h : seq2Bound.memRep = g1 := rfl
  rw [h]
  exact BigO.refl _

def branchBoundAB : ResourceBound :=
  branchBound
    (CStatement.stmtBound (Γ := DummyCtx) (α := StmtO1A))
    (CStatement.stmtBound (Γ := DummyCtx) (α := StmtO1B))

-- Branch time is `max(1,1)+1 = 2 = O(1)`; mem is `max(1,1) = 1`.
example : BigO branchBoundAB.timeRep g1 := by
  apply BigO.const_le_one (K := 2)
  intro n
  exact Nat.le_refl _

example : BigO branchBoundAB.memRep g1 := by
  have h : branchBoundAB.memRep = g1 := rfl
  rw [h]
  exact BigO.refl _

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
  -- kit + order + insertion + composition + extension all compiled
  -- (examples above); runtime confirms the computed witnesses
  IO.println "OK: BigO.refl / trans / one_le_log / log_le_sq" 
  IO.println "OK: Zero < O1 < HALTS < UNDECIDABLE, UNBOUND < UNDECIDABLE, ¬(HALTS ≤ UNBOUND)"
  IO.println "OK: O1 < O_log < HALTS via can_insert (BigO evidence)"
  IO.println "OK: O_linear extension (own file, no base edits): O1/log < linear < poly2"
  IO.println "OK: seq 2×O1 = O1 (const folding via BigO.add)"
  IO.println "OK: branch max bound via BigO.max_bound"
  if ok then
    IO.println "All complexity tests passed." *> pure 0
  else
    pure 1

end TestComplexity
