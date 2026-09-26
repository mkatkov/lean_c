/-! Local Big-O + lemma kit (`refl`/`trans`/`add`/`max`).

Split out of `LeanC/Complexity.lean`: the growth foundation everything
else builds on. No dependencies on classes, tags, or graphs. -/
namespace LeanC

/-- Local Big-O on sizes: `f` is eventually bounded by `c * max (g ·) 1`.

WHY this shape: `c` is the Landau constant (absorbs codegen's
constant-factor blowup, so `O1 + O1 = O1` holds); `N₀` is "eventually"
(`atTop`); `max · 1` guards `g n = 0` (`Nat.log2 1 = 0` would
otherwise force `f = 0`). WHAT it gives: every lattice edge below is
one of these facts, and composition (sequence/branch) reuses the kit
instead of re-proving arithmetic.

This is the `Nat → Nat` restriction of `Asymptotics.IsBigO Filter.atTop`
(Mathlib): the `max · 1` guards the `g n = 0` case (`Nat.log2 1 = 0`),
the `N₀` is the `atTop` eventuality, the `c` is the Landau constant.
If Mathlib is added later, replace this def by
`Asymptotics.IsBigO Filter.atTop` behind the same `BigO` name;
all ordering proofs use only the kit below. -/
def BigO (f g : Nat → Nat) : Prop :=
  ∃ c N₀, ∀ n ≥ N₀, f n ≤ c * Nat.max (g n) 1

/-- WHY `refl`: every cost function is inside its own envelope.
Used for `X ≤ X` edges and the `O1`-guard (`1 =O 1`). -/
theorem BigO.refl (f : Nat → Nat) : BigO f f := by
  refine ⟨1, 0, fun n _ => ?_⟩
  simp only [Nat.one_mul]
  exact Nat.le_max_left _ _

/-- WHY `trans`: chains quantitative edges (`1 =O log`, `log =O n²`
gives `1 =O n²`). Takes `c₁ * (c₂ + 1)` (not `c₁ * c₂`) so the
`c₂ = 0` case still works: if `g` is constantly `0`, `max (g·) 1 = 1`
and the bound degrades gracefully instead of collapsing to `0`. -/
theorem BigO.trans {f g h : Nat → Nat} : BigO f g → BigO g h → BigO f h := by
  intro ⟨c₁, N₁, h₁⟩ ⟨c₂, N₂, h₂⟩
  refine ⟨c₁ * (c₂ + 1), Nat.max N₁ N₂, fun n hn => ?_⟩
  have hn1 : n ≥ N₁ := Nat.le_trans (Nat.le_max_left _ _) hn
  have hn2 : n ≥ N₂ := Nat.le_trans (Nat.le_max_right _ _) hn
  have e1 := h₁ n hn1
  have e2 := h₂ n hn2
  have hMH1 : 1 ≤ Nat.max (h n) 1 := Nat.le_max_right _ _
  have hMGle : Nat.max (g n) 1 ≤ (c₂ + 1) * Nat.max (h n) 1 := by
    apply Nat.max_le.mpr
    constructor
    · calc g n ≤ c₂ * Nat.max (h n) 1 := e2
        _ ≤ (c₂ + 1) * Nat.max (h n) 1 :=
          Nat.mul_le_mul_right _ (Nat.le_succ _)
    · calc (1 : Nat) ≤ Nat.max (h n) 1 := hMH1
        _ = 1 * Nat.max (h n) 1 := (Nat.one_mul _).symm
        _ ≤ (c₂ + 1) * Nat.max (h n) 1 :=
          Nat.mul_le_mul_right _ (by omega)
  calc f n ≤ c₁ * Nat.max (g n) 1 := e1
    _ ≤ c₁ * ((c₂ + 1) * Nat.max (h n) 1) := Nat.mul_le_mul_left c₁ hMGle
    _ = (c₁ * (c₂ + 1)) * Nat.max (h n) 1 := (Nat.mul_assoc _ _ _).symm

/-- WHY `const_le_one`: this is what "`O1` means `≤ K`" *means*.
Any fixed-size fragment (one assignment, one guarded dereference,
`1 + 1 = 2` after sequencing two `O1`s) folds back to `O(1)`.
Takes `c := K`: `c n ≤ K = K * max 1 1`. -/
theorem BigO.const_le_one {K : Nat} (c : Nat → Nat) (hc : ∀ n, c n ≤ K) :
    BigO c (fun _ => 1) := by
  refine ⟨K, 0, fun n _ => ?_⟩
  have h2 : K * Nat.max ((fun _ => 1) n) 1 = K := by simp
  rw [h2]
  exact hc n

/-- WHY `add`: sequential time *adds*. If `f₁ =O g₁` and `f₂ =O g₂`
then `f₁ + f₂ =O g₁ + g₂` (takes `c₁ + c₂`, `max N₁ N₂`).
Per-resource use: `TimeCost.seqCombine` is exactly this sum
(`time_seq_preserves` in `Examples/Resources.lean` is one application);
memory sequencing maxes instead (`mem_seq_preserves` via `max_bound`). -/
theorem BigO.add {f₁ g₁ f₂ g₂ : Nat → Nat} :
    BigO f₁ g₁ → BigO f₂ g₂ →
    BigO (fun n => f₁ n + f₂ n) (fun n => g₁ n + g₂ n) := by
  intro ⟨c₁, N₁, h₁⟩ ⟨c₂, N₂, h₂⟩
  refine ⟨c₁ + c₂, Nat.max N₁ N₂, fun n hn => ?_⟩
  have hn1 : n ≥ N₁ := Nat.le_trans (Nat.le_max_left _ _) hn
  have hn2 : n ≥ N₂ := Nat.le_trans (Nat.le_max_right _ _) hn
  have e1 := h₁ n hn1
  have e2 := h₂ n hn2
  have hM1 : g₁ n ≤ Nat.max (g₁ n + g₂ n) 1 :=
    Nat.le_trans (Nat.le_add_right _ _) (Nat.le_max_left _ _)
  have hM2 : g₂ n ≤ Nat.max (g₁ n + g₂ n) 1 :=
    Nat.le_trans (Nat.le_add_left _ _) (Nat.le_max_left _ _)
  have hm1 : Nat.max (g₁ n) 1 ≤ Nat.max (g₁ n + g₂ n) 1 :=
    Nat.max_le.mpr ⟨hM1, Nat.le_max_right _ _⟩
  have hm2 : Nat.max (g₂ n) 1 ≤ Nat.max (g₁ n + g₂ n) 1 :=
    Nat.max_le.mpr ⟨hM2, Nat.le_max_right _ _⟩
  have l1 : c₁ * Nat.max (g₁ n) 1 ≤ c₁ * Nat.max (g₁ n + g₂ n) 1 :=
    Nat.mul_le_mul_left c₁ hm1
  have l2 : c₂ * Nat.max (g₂ n) 1 ≤ c₂ * Nat.max (g₁ n + g₂ n) 1 :=
    Nat.mul_le_mul_left c₂ hm2
  show f₁ n + f₂ n ≤ (c₁ + c₂) * Nat.max (g₁ n + g₂ n) 1
  calc f₁ n + f₂ n
      ≤ c₁ * Nat.max (g₁ n) 1 + c₂ * Nat.max (g₂ n) 1 := Nat.add_le_add e1 e2
    _ ≤ c₁ * Nat.max (g₁ n + g₂ n) 1 + c₂ * Nat.max (g₁ n + g₂ n) 1 :=
        Nat.add_le_add l1 l2
    _ = (c₁ + c₂) * Nat.max (g₁ n + g₂ n) 1 := (Nat.add_mul _ _ _).symm

/-- WHY `max_bound`: branching and high-water marks take
`max`. If each side is bounded, their `max` is bounded by the `max`
of the envelopes (takes `c₁ + c₂`, which safely covers the zero
cases). Per-resource use: `MemCost.seqCombine`/`branchCombine` and the
max-half of `TimeCost.branchCombine` are exactly this
(`mem_seq_preserves`, `mem_branch_preserves` in
`Examples/Resources.lean`); the time-branch guard `+1` chains this with
`add` (`time_branch_preserves`). -/
theorem BigO.max_bound {f₁ g₁ f₂ g₂ : Nat → Nat} :
    BigO f₁ g₁ → BigO f₂ g₂ →
    BigO (fun n => Nat.max (f₁ n) (f₂ n)) (fun n => Nat.max (g₁ n) (g₂ n)) := by
  intro ⟨c₁, N₁, h₁⟩ ⟨c₂, N₂, h₂⟩
  refine ⟨c₁ + c₂, Nat.max N₁ N₂, fun n hn => ?_⟩
  have hn1 : n ≥ N₁ := Nat.le_trans (Nat.le_max_left _ _) hn
  have hn2 : n ≥ N₂ := Nat.le_trans (Nat.le_max_right _ _) hn
  have e1 := h₁ n hn1
  have e2 := h₂ n hn2
  have hg1 : g₁ n ≤ Nat.max (Nat.max (g₁ n) (g₂ n)) 1 :=
    Nat.le_trans (Nat.le_max_left _ _) (Nat.le_max_left _ _)
  have hg2 : g₂ n ≤ Nat.max (Nat.max (g₁ n) (g₂ n)) 1 :=
    Nat.le_trans (Nat.le_max_right _ _) (Nat.le_max_left _ _)
  have hm1 : Nat.max (g₁ n) 1 ≤ Nat.max (Nat.max (g₁ n) (g₂ n)) 1 :=
    Nat.max_le.mpr ⟨hg1, Nat.le_max_right _ _⟩
  have hm2 : Nat.max (g₂ n) 1 ≤ Nat.max (Nat.max (g₁ n) (g₂ n)) 1 :=
    Nat.max_le.mpr ⟨hg2, Nat.le_max_right _ _⟩
  have l1 : f₁ n ≤ (c₁ + c₂) * Nat.max (Nat.max (g₁ n) (g₂ n)) 1 := by
    calc f₁ n ≤ c₁ * Nat.max (g₁ n) 1 := e1
      _ ≤ c₁ * Nat.max (Nat.max (g₁ n) (g₂ n)) 1 := Nat.mul_le_mul_left c₁ hm1
      _ ≤ (c₁ + c₂) * Nat.max (Nat.max (g₁ n) (g₂ n)) 1 :=
        Nat.mul_le_mul_right _ (Nat.le_add_right _ _)
  have l2 : f₂ n ≤ (c₁ + c₂) * Nat.max (Nat.max (g₁ n) (g₂ n)) 1 := by
    calc f₂ n ≤ c₂ * Nat.max (g₂ n) 1 := e2
      _ ≤ c₂ * Nat.max (Nat.max (g₁ n) (g₂ n)) 1 := Nat.mul_le_mul_left c₂ hm2
      _ ≤ (c₁ + c₂) * Nat.max (Nat.max (g₁ n) (g₂ n)) 1 :=
        Nat.mul_le_mul_right _ (Nat.le_add_left _ _)
  show Nat.max (f₁ n) (f₂ n) ≤ (c₁ + c₂) * Nat.max (Nat.max (g₁ n) (g₂ n)) 1
  exact Nat.max_le.mpr ⟨l1, l2⟩

end LeanC
