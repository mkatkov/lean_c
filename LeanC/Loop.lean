import LeanC.CostSpec
import LeanC.Complexity.BigO
import LeanC.Complexity.Growth
import LeanC.Complexity.Classes
import LeanC.Complexity.Lattice
import LeanC.Complexity.Quant
import LeanC.Complexity.Bridge
import LeanC.Context

/-!
# Bounded loops (N4) + unbounded marker (N5) + recursion sketch (N6)

N4 is the first non-`O1` inhabitant: counted loops (`forN`) with
iterated-sum time (`BigO.add` chain) and high-water-max mem
(`BigO.max_bound`). `forN iters body` repeats `body i` for
`i < iters`; time sums, mem maxes (bound rules). With `0` iters the
loop is `Zero`-exact/`O1`; with `n` iters of uniform-`O1` body the
diagonal (`iters = input size`) is `linear` (`poly 1` in core;
`TimeComplexity_linear` in `Examples`/`Tests` via same bound).

N5 is a marker only: `whileTrue` carries `UNBOUND` time / `GROWING` mem
tags by stipulation; under total `Nat → Nat` costs `IsDivergentCost` is
empty (`no_divergent_cost`), so there is deliberately NO cost-function
inhabitant. Tag edges stay stipulated (`complexityLE_unbound_undecidable`,
`complexityLE_growing_unknown`).

N6 is a sketch (fuel, not semantics): bounded recursion with `Nat` fuel
reduces to N4 (`recWithFuel fuel body = forN fuel …`); unbounded
recursion reduces to N5. No fixpoint semantics. One `fuel` example only.

Only `BigO` kit (`refl/trans/add/max_bound/const_le_one`), no Mathlib,
no placeholders.
-/

namespace LeanC

/-! ## N4: counted-loop combinator (`forN`, time sums, mem maxes) -/

/-- Iterated-sum auxiliary: `forNTimeAux iters body n = Σ_{i<iters} (body i).val n`.
Recursion on `iters` (not `List.range` fold) so induction + `BigO.add`
is direct. -/
def forNTimeAux : Nat → (Nat → StepCost) → Nat → Nat
  | 0, _, _ => 0
  | n + 1, body, m => forNTimeAux n body m + (body n).val m

/-- High-water-max auxiliary: `max_{i<iters} (body i).val m`. -/
def forNMemAux : Nat → (Nat → CellCost) → Nat → Nat
  | 0, _, _ => 0
  | n + 1, body, m => Nat.max (forNMemAux n body m) ((body n).val m)

/-- Counted loop, time: `forN iters body` at size `n` sums per-iteration
costs. `body : Nat → StepCost` (per-iteration variation; const body gives
`iters * K`). -/
def forNTime (iters : Nat) (body : Nat → StepCost) : StepCost :=
  ⟨fun n => forNTimeAux iters body n⟩

/-- Counted loop, mem: high-water `max` (slots reused). -/
def forNMem (iters : Nat) (body : Nat → CellCost) : CellCost :=
  ⟨fun n => forNMemAux iters body n⟩

/-- `O1 + O1 = O1` for arbitrary cost functions (the `∑`-of-`=O` step,
one `BigO.add` + const folding to `g1`). -/
theorem o1_add_o1_time {f h : Nat → Nat}
    (hf : BigO f g1) (hh : BigO h g1) : BigO (fun n => f n + h n) g1 := by
  have hadd : BigO (fun n => f n + h n) (fun n => g1 n + g1 n) :=
    BigO.add hf hh
  have hfold : BigO (fun n => g1 n + g1 n) g1 :=
    BigO.const_le_one _ (K := 2) (fun n => by simp [g1])
  exact BigO.trans hadd hfold

/-- `max` of `O1` mems is `O1`. -/
theorem o1_max_o1_mem {f h : Nat → Nat}
    (hf : BigO f g1) (hh : BigO h g1) : BigO (fun n => Nat.max (f n) (h n)) g1 := by
  have hmax : BigO (fun n => Nat.max (f n) (h n)) (fun n => Nat.max (g1 n) (g1 n)) :=
    BigO.max_bound hf hh
  have hfold : BigO (fun n => Nat.max (g1 n) (g1 n)) g1 := by
    simpa [g1] using BigO.refl g1
  exact BigO.trans hmax hfold

/-- Fixed-`iters` loop of `O1` bodies stays `O1` (time, iterated `add`). -/
theorem forNTime_in_o1 (iters : Nat) (body : Nat → StepCost)
    (h : ∀ i, BigO ((body i).val) g1) :
    BigO (HasCost.cost (forNTime iters body)) g1 := by
  induction iters with
  | zero =>
    show BigO (fun n => forNTimeAux 0 body n) g1
    simpa [forNTimeAux] using (bigO_zero_le_one : BigO gZero g1)
  | succ k ih =>
    show BigO (fun n => forNTimeAux (k + 1) body n) g1
    simp only [forNTimeAux]
    exact o1_add_o1_time ih (h k)

/-- Fixed-`iters` loop of `O1` bodies stays `O1` (mem, iterated `max`). -/
theorem forNMem_in_o1 (iters : Nat) (body : Nat → CellCost)
    (h : ∀ i, BigO ((body i).val) g1) :
    BigO (HasCost.cost (forNMem iters body)) g1 := by
  induction iters with
  | zero =>
    show BigO (fun n => forNMemAux 0 body n) g1
    simpa [forNMemAux] using (bigO_zero_le_one : BigO gZero g1)
  | succ k ih =>
    show BigO (fun n => Nat.max (forNMemAux k body n) ((body k).val n)) g1
    exact o1_max_o1_mem ih (h k)

/-- `0` iters is `Zero`-exact (time). -/
theorem forNTime_zero_in_zero (body : Nat → StepCost) :
    costInZero StepCost (forNTime 0 body) :=
  rfl

/-- `0` iters is `Zero`-exact (mem). -/
theorem forNMem_zero_in_zero (body : Nat → CellCost) :
    costInZero CellCost (forNMem 0 body) :=
  rfl

/-- `0` iters is `O1` (via `Zero` promotion; the `O1` case of N4). -/
theorem forNTime_zero_in_o1 (body : Nat → StepCost) :
    costInClass (axis := .time) StepCost TimeComplexity_O1 (forNTime 0 body) :=
  costInZero_to_o1_time (forNTime_zero_in_zero body)

theorem forNMem_zero_in_o1 (body : Nat → CellCost) :
    costInClass (axis := .memory) CellCost MemoryComplexity_O1 (forNMem 0 body) :=
  costInZero_to_o1_mem (forNMem_zero_in_zero body)

/-- Diagonal (linear) time: `iters = input size`
(`forNDiagTime body n = Σ_{i<n} (body i).val n`). With uniform-`O1` body
(`∀ i m, ≤ K`) the sum is `≤ n * K`, i.e. `linear`. -/
def forNDiagTime (body : Nat → StepCost) : StepCost :=
  ⟨fun n => forNTimeAux n body n⟩

/-- Diagonal mem (stays `max`, hence `O1` under uniform bound). -/
def forNDiagMem (body : Nat → CellCost) : CellCost :=
  ⟨fun n => forNMemAux n body n⟩

/-- Uniform-bound sum lemma: `Σ_{i<iters} ≤ iters * K`. -/
theorem forNTimeAux_le {body : Nat → StepCost} {K : Nat}
    (hK : ∀ i m, (body i).val m ≤ K) (iters n : Nat) :
    forNTimeAux iters body n ≤ iters * K := by
  induction iters with
  | zero => simp [forNTimeAux]
  | succ k ih =>
    simp only [forNTimeAux]
    calc forNTimeAux k body n + (body k).val n
        ≤ k * K + K := Nat.add_le_add ih (hK k n)
      _ = (k + 1) * K := by rw [Nat.add_mul, Nat.one_mul, Nat.add_comm]

/-- Uniform-bound max lemma: `max_{i<iters} ≤ K` (for `iters = 0`, `0 ≤ max K 0`). -/
theorem forNMemAux_le {body : Nat → CellCost} {K : Nat}
    (hK : ∀ i m, (body i).val m ≤ K) (iters n : Nat) :
    forNMemAux iters body n ≤ Nat.max K 0 := by
  induction iters with
  | zero => simp [forNMemAux]
  | succ k ih =>
    simp only [forNMemAux]
    apply Nat.max_le.mpr
    constructor
    · exact ih
    · exact Nat.le_trans (hK k n) (Nat.le_max_left _ _)

/-- First non-`O1` inhabitant (N4): `n` iters of uniform-`O1` body is
`poly 1` (`O(n)`, core rep `gpoly 1 = fun n => n ^ 1`). `Tests` transports
this to `TimeComplexity_linear` (`glinear = fun n => n`) via same bound. -/
theorem forNDiag_linear {body : Nat → StepCost} {K : Nat}
    (hK : ∀ i m, (body i).val m ≤ K) :
    costInClass (axis := .time) StepCost (TimeComplexity_poly 1) (forNDiagTime body) := by
  show BigO (HasCost.cost (forNDiagTime body)) (gpoly 1)
  refine ⟨K, 0, fun n _ => ?_⟩
  show forNTimeAux n body n ≤ K * Nat.max ((gpoly 1) n) 1
  have hsum : forNTimeAux n body n ≤ n * K := forNTimeAux_le hK n n
  have hpow : (gpoly 1) n = n := by simp [gpoly, Nat.pow_one]
  rw [hpow]
  calc forNTimeAux n body n ≤ n * K := hsum
    _ = K * n := Nat.mul_comm _ _
    _ ≤ K * Nat.max n 1 := Nat.mul_le_mul_left K (Nat.le_max_left _ _)

/-- Diagonal mem stays `O1` under uniform bound (`max`, not sum). -/
theorem forNDiagMem_in_o1 {body : Nat → CellCost} {K : Nat}
    (hK : ∀ i m, (body i).val m ≤ K) :
    costInClass (axis := .memory) CellCost MemoryComplexity_O1 (forNDiagMem body) := by
  show BigO (HasCost.cost (forNDiagMem body)) g1
  exact BigO.const_le_one _ (fun n => forNMemAux_le hK n n)

/-- Const-body convenience: `n` iters of `fun _ => K` is `poly 1`. -/
def constStepBody (K : Nat) : Nat → StepCost := fun _ => ⟨fun _ => K⟩

theorem constStepBody_uniform (K : Nat) : ∀ i m, ((constStepBody K i).val m) ≤ K :=
  fun _ _ => Nat.le_refl _

theorem forNDiag_const_linear (K : Nat) :
    costInClass (axis := .time) StepCost (TimeComplexity_poly 1)
      (forNDiagTime (constStepBody K)) :=
  forNDiag_linear (constStepBody_uniform K)

/-! ## N5: unbounded loop marker (`whileTrue`, no cost inhabitant) -/

/-- Unbounded-loop marker: `while (true) { … }` (or `whileTrue` stub).
Carries `UNBOUND` time / `GROWING` mem *tags* by stipulation (see
`complexityLE_unbound_undecidable`, `complexityLE_growing_unknown`);
deliberately NO `HasCost`/`costInClass` inhabitant — under total
`Nat → Nat` costs `IsDivergentCost` is empty (`no_divergent_cost` cited
below), so true divergence awaits a partial-cost/trace model. -/
inductive UnboundedLoop where
  | whileTrue : UnboundedLoop

/-- No divergent cost exists under total costs (N5 cites this limit:
`whileTrue` has no cost-fn inhabitant). -/
theorem whileTrue_no_divergent (c : Nat → Nat) : ¬ IsDivergentCost c :=
  no_divergent_cost c

/-- Tag-level spines stay stipulated (N5 markers, no `BigO` inside). -/
example : ComplexityLE TimeComplexity_UNBOUND TimeComplexity_UNDECIDABLE :=
  complexityLE_unbound_undecidable
example : ComplexityLE MemoryComplexity_GROWING MemoryComplexity_UNKNOWN :=
  complexityLE_growing_unknown

/-! ## N6: recursion sketch (fuel, reduces to N4/N5, no fixpoint) -/

/-- Bounded recursion with `Nat` fuel = counted loop (reduces to N4).
`recWithFuel fuel body` runs `body` at most `fuel` times; `fuel = 0`
is base case (`Zero`), `fuel = n` (input size) is linear diagonal.
Self-`call` via registry: recursive `fname` resolves like any `call`
(`Program.callResolves` with `f.fname = fname`); fuel (or a
well-founded variant — N6 picks fuel, one example) ensures termination.
Unbounded recursion (no fuel) reduces to N5 (`whileTrue` marker). No
fixpoint semantics. -/
def recWithFuelTime (fuel : Nat) (body : Nat → StepCost) : StepCost :=
  forNTime fuel body

def recWithFuelMem (fuel : Nat) (body : Nat → CellCost) : CellCost :=
  forNMem fuel body

theorem recWithFuelTime_eq (fuel : Nat) (body : Nat → StepCost) :
    recWithFuelTime fuel body = forNTime fuel body := rfl

theorem recWithFuelMem_eq (fuel : Nat) (body : Nat → CellCost) :
    recWithFuelMem fuel body = forNMem fuel body := rfl

/-- Fuel `0` is `Zero`-exact (base case). -/
theorem recFuelZero_time (body : Nat → StepCost) :
    costInZero StepCost (recWithFuelTime 0 body) :=
  forNTime_zero_in_zero body

/-- One fuel example (N6): countdown with fuel `n`, uniform-`O1` step
(`K = 1`), time `poly 1` (linear diagonal), mem `O1`. -/
def countdownBody : Nat → StepCost := constStepBody 1
def countdownMemBody : Nat → CellCost := fun _ => ⟨fun _ => 0⟩

theorem countdownMem_uniform : ∀ i m, ((countdownMemBody i).val m) ≤ 0 :=
  fun _ _ => Nat.le_refl _

example : costInClass (axis := .time) StepCost (TimeComplexity_poly 1)
    (forNDiagTime countdownBody) :=
  forNDiag_const_linear 1

example : costInClass (axis := .memory) CellCost MemoryComplexity_O1
    (forNDiagMem countdownMemBody) :=
  forNDiagMem_in_o1 countdownMem_uniform

end LeanC
