import LeanC.Complexity.BigO

/-! Canonical representatives + growth facts (`0 =O 1 =O log =O n^k`).

Split out of `LeanC/Complexity.lean`. Depends on `BigO`; depended on
by `Lattice` (the `TagLE` table entries *are* these facts). -/
namespace LeanC

/-- Canonical representatives: WHAT a quantitative class *is*.
`Class(g) = { f | BigO f g }`, so class ordering *is* Big-O
entailment (`Class(g₁) ≤ Class(g₂) ↔ BigO g₁ g₂`).

- `gZero = 0`: empty computation (bottom, by stipulation — outside
  the `=O` machinery, every program is `≥` it).
- `g1 = 1`: `O(1)` — any fixed constant folds here (`const_le_one`).
- `glog = log2(n+1)`: `O(log n)` — the `+1` avoids the degenerate
  `log2 0 = log2 1 = 0` cases.
- `gpoly k = n^k`: `O(n^k)` nested fixed loops.
Time and memory share shapes; memory reads "live cells", and its
`within_bounds` proofs (`Arrays.lean`) are what promote an access
from `UNKNOWN` to `O1`. -/
def gZero : Nat → Nat := fun _ => 0
def g1 : Nat → Nat := fun _ => 1
def glog : Nat → Nat := fun n => Nat.log2 (n + 1)
def gpoly (k : Nat) : Nat → Nat := fun n => n ^ k

/-- WHY `0 =O 1`: the bottom edge. `0 ≤ 0 * …` holds trivially;
this is the `Zero < O1` lattice edge as a growth fact. -/
theorem bigO_zero_le_one : BigO gZero g1 :=
  ⟨0, 0, fun _ _ => Nat.zero_le _⟩

/-- WHY `1 =O log`: the `O1 < O_log` edge. `1 ≤ max (log2(n+1)) 1`
holds because the `max` is always `≥ 1` — no growth argument needed,
just the `max · 1` guard doing its job. -/
theorem bigO_one_le_log : BigO g1 glog := by
  refine ⟨1, 0, fun n _ => ?_⟩
  show (1 : Nat) ≤ 1 * Nat.max (Nat.log2 (n + 1)) 1
  rw [Nat.one_mul]
  exact Nat.le_max_right _ _

/-- WHY: `log2(n+1) ≤ n` — log grows slower than identity.
Proof idea: `log2(m) < m` for all `m ≥ 1` because `m < 2^m`
(`lt_two_pow_self`), transported through `log2_lt`
(`log2 m < k ↔ m < 2^k`). This single fact feeds both
`log =O n²` and (in the extension file) `log =O linear`. -/
theorem log_succ_le_self (n : Nat) : Nat.log2 (n + 1) ≤ n := by
  have hlt : Nat.log2 (n + 1) < n + 1 := by
    rw [Nat.log2_lt (by omega)]
    exact Nat.lt_two_pow_self
  omega

/-- WHY: `n ≤ n²` — identity is absorbed by any quadratic envelope.
Used to chain `log ≤ id ≤ sq`. The `succ` case is
`n = n*1 ≤ n*n = n²` (needs `1 ≤ n`, hence the split on zero). -/
theorem self_le_sq (n : Nat) : n ≤ n ^ 2 := by
  match n with
  | Nat.zero => simp
  | Nat.succ k =>
    calc k + 1 = (k + 1) * 1 := by simp
      _ ≤ (k + 1) * (k + 1) := Nat.mul_le_mul_left _ (Nat.le_add_left _ _)
      _ = (k + 1) ^ 2 := (Nat.pow_two _).symm

/-- WHY `log =O n²`: the `O_log < O_poly` edge (at `k = 2`).
Chain: `log2(n+1) ≤ n ≤ n² ≤ max (n²) 1`, all with constant `1`.
Together with `bigO_one_le_log` these are the two hops that insert
`log` between `O1` and `HALTS`; the same chain also gives
`log =O linear` in `Examples/ComplexityLinear.lean`. -/
theorem bigO_log_le_sq : BigO glog (fun n => n ^ 2) := by
  refine ⟨1, 0, fun n _ => ?_⟩
  show Nat.log2 (n + 1) ≤ 1 * Nat.max (n ^ 2) 1
  rw [Nat.one_mul]
  calc Nat.log2 (n + 1) ≤ n := log_succ_le_self n
    _ ≤ n ^ 2 := self_le_sq n
    _ ≤ Nat.max (n ^ 2) 1 := Nat.le_max_left _ _

/-- WHY `1 =O n^k` for every `k`: constant fragments sit below
*every* polynomial degree (including `k = 0`, where `n^0 = 1`).
Gives the `O1 ≤ poly(k)` edges for free, no per-`k` work. -/
theorem bigO_one_le_poly (k : Nat) : BigO g1 (gpoly k) := by
  refine ⟨1, 0, fun n _ => ?_⟩
  show (1 : Nat) ≤ 1 * Nat.max ((gpoly k) n) 1
  rw [Nat.one_mul]
  exact Nat.le_max_right _ _

/-- WHY `n^k¹ =O n^k²` when `k₁ ≤ k₂`: polynomial degrees are
ordered by exponent (`pow_le_pow_right`, needs `n ≥ 1`, hence
`N₀ := 1`). So `poly` is not one class but a *chain* — a new degree
slots in without disturbing the rest. -/
theorem bigO_poly_le_poly {k₁ k₂ : Nat} (h : k₁ ≤ k₂) :
    BigO (gpoly k₁) (gpoly k₂) := by
  refine ⟨1, 1, fun n hn => ?_⟩
  show n ^ k₁ ≤ 1 * Nat.max (n ^ k₂) 1
  rw [Nat.one_mul]
  calc n ^ k₁ ≤ n ^ k₂ := Nat.pow_le_pow_right (by omega) h
    _ ≤ Nat.max (n ^ k₂) 1 := Nat.le_max_left _ _

/-- WHY `log =O n^k` for `k ≥ 1`: log sits below *every*
non-trivial polynomial. Chain `log ≤ id ≤ n^k` — note this deliberately
does NOT go via `n²` (for `k = 1`, `n² ≤ n` is false). The `k = 0`
case is correctly excluded: `log =O 1` is false (see strictness
below), i.e. `log ≰ poly(0)`. -/
theorem bigO_log_le_poly {k : Nat} (hk : 1 ≤ k) : BigO glog (gpoly k) := by
  refine ⟨1, 0, fun n _ => ?_⟩
  show Nat.log2 (n + 1) ≤ 1 * Nat.max ((gpoly k) n) 1
  rw [Nat.one_mul]
  have h1 : Nat.log2 (n + 1) ≤ n := log_succ_le_self n
  have h2 : n ≤ (gpoly k) n := by
    show n ≤ n ^ k
    match n with
    | Nat.zero => exact Nat.zero_le _
    | Nat.succ m =>
      calc m + 1 = (m + 1) ^ 1 := by simp
        _ ≤ (m + 1) ^ k := Nat.pow_le_pow_right (by omega) hk
  calc Nat.log2 (n + 1) ≤ n := h1
    _ ≤ (gpoly k) n := h2
    _ ≤ Nat.max ((gpoly k) n) 1 := Nat.le_max_left _ _

/-- WHY strictness matters: `O1 < O_log` is *strict* — log is
genuinely bigger than constant. Proof idea: for any claimed constant
`c`, pick `n = max N₀ 2^(c+1)`; then `2^(c+1) ≤ n+1`, so by `le_log2`,
`c+1 ≤ log2(n+1)`, contradicting `log2(n+1) ≤ c`. Without this,
`O1` and `O_log` could collapse and the lattice would lie. -/
theorem not_bigO_log_le_one : ¬ BigO glog g1 := by
  intro ⟨c, N₀, h⟩
  let n := Nat.max N₀ (2 ^ (c + 1))
  have hn : n ≥ N₀ := Nat.le_max_left _ _
  have hle := h n hn
  simp only [glog, g1, Nat.max_self, Nat.mul_one] at hle
  -- hle : Nat.log2 (n + 1) ≤ c
  have hge : c + 1 ≤ Nat.log2 (n + 1) := by
    rw [Nat.le_log2 (by omega)]
    calc 2 ^ (c + 1) ≤ n := Nat.le_max_right _ _
      _ ≤ n + 1 := Nat.le_succ _
  omega

end LeanC
