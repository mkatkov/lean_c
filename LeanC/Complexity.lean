/-!
# Complexity classes — worst-case time & memory bounds

**What this file is:** a Lean vocabulary for stating and checking
worst-case resource bounds of programs that will be emitted as C.
Every fragment has a *cost function* `cost : Nat → Nat` (input size →
step count for time, peak live cells for memory). A *complexity class*
is the set of cost functions inside one Big-O envelope. A closed
program yields one provable `(timeClass, memClass)` pair.

**Why Big-O (not a hand-rolled lattice):** ordering classes then means
proving standard growth facts (`1 =O log =O n^k`), reusing one small
lemma kit (`refl/trans/add/max`). Inserting a new class reduces to
exhibiting two Big-O inclusions — no new metatheory per class.
The local `BigO` below is deliberately stated to match
`Asymptotics.IsBigO Filter.atTop`, so a future Mathlib dependency can
replace it behind the same name without touching lattice code.

**The two axes:** time (`Zero < O1 < log < poly < HALTS`, plus
`UNBOUND` on the side, all below `UNDECIDABLE`) and memory (mirror:
`Zero < O1 < BOUNDED`, plus `GROWING`, below `UNKNOWN`). Either axis
may independently be unknown. `O1` means "bounded by *some fixed
constant*べ (`≤ K`), never "exactly 1 step" — one Lean op may become
several C ops, and `=O` absorbs constants, which is what keeps
codegen sound.

**How ordering is stored (read this before extending):**
- `ComplexityTag` is a *closed* snapshot of the base lattice used for
  decidable checks (`decide`) and the `LE` instance. Base tags never
  change shape — new quantitative classes do NOT need new tags
  (see below).
- `HasTag` / `TagOf` map the base class *types* to those tags.
- `HasQuantRep` (open typeclass, near the end of this file) is the
  **extension point**: any new file can give a new class type a
  representative function `rep : Nat → Nat` without touching this file.
  Ordering for such classes is plain `BigO` on reps (`QuantLE`), and
  insertion is `can_insert_quant_open`. Worked example:
  `LeanC/ComplexityLinear.lean` adds `TimeComplexity_linear`
  (`rep = fun n => n`) in its own file — no base edits.
- `CComplexityRelationship` (open typeclass) records immediate
  predecessors/successors for documentation; the *evidence* for a
  quantitative edge is always the `BigO` proof, never an assertion.
- `CComplexityGraph` is just the knowledge base (list of known class
  types). `LE` on same-base graphs is trivial (`True`); the real order
  lives on tags/reps. `can_insert_complexity_class_to_grapth` is the
  legacy closed predicate for base tags; prefer
  `can_insert_quant_open` for new classes.

**How to add a new quantitative class `X` (copy-paste recipe):**
1. `inductive TimeComplexity_X where | mk` (+ `CComplexity .time`
   instance with `isComplexity := True`).
2. `instance : HasQuantRep TimeComplexity_X where rep := <your g_X>`
   (e.g. `fun n => n`, `fun n => 2 ^ n`).
3. `instance : CComplexityRelationship .time TimeComplexity_X where`
   `minimalStrictlySmaller := [<pred>]`,
   `minimalStrictlyLarger := [<succ>]`, `equalTo := []`.
4. Prove the two Big-O facts (`BigO g_pred g_X`, `BigO g_X g_succ`
   or `X ≤ HALTS` via `quant_le_halts`), then discharge
   `can_insert_quant_open` — see `ComplexityLinear.lean`.
-/

namespace LeanC
universe u

/-- Which resource a class talks about.
WHY two axes: time steps and live memory compose differently
(sequence adds time but takes max memory), so one lattice cannot
serve both; context carries one bound per axis. -/
inductive ResourceAxis where
| time : ResourceAxis
| memory : ResourceAxis
deriving DecidableEq, Repr

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
`Processes.seqBound` time is exactly this sum, so this lemma *is*
the sequence composition rule. -/
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

/-- WHY `max_bound`: branching and memory high-water marks take
`max`. If each side is bounded, their `max` is bounded by the `max`
of the envelopes (takes `c₁ + c₂`, which safely covers the zero
cases). `Processes.branchBound`/`seqBound`-memory are exactly this. -/
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
The test suite's first insertion exercise is exactly these two hops
(`1 =O log`, `log =O n²`). -/
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

/-- WHAT a complexity class is in Lean: a marker type indexed by
axis. WHY a marker (not data): classes are *static* knowledge — which
envelope a fragment claims — so each class is an empty inductive with
one constructor; the meaning lives in the representative (`g1`,
`glog`, …) and the `BigO` proofs, not in values. The `Prop` field
keeps the existing codebase style (`isCComplexity := True`).

The three non-quantitative time classes sit *outside* Big-O, wrapping it:
- `HALTS`: `∃ g computable, BigO cost g` — some finite bound exists,
  unnamed. Every quantitative class refines it by instantiation.
- `UNBOUND`: proven divergence (no finite `g` exists *plus* an
  infinite-trace witness) — hence incomparable with the finite chain.
- `UNDECIDABLE`: no claim at all — the unique top both forget to. -/
class CComplexity (axis : ResourceAxis) (α : Type u) where
  isComplexity : Prop

/-- Representative bundle (kept for interface compat; new code should
use the open `HasQuantRep` below, which carries the same `rep`). -/
structure ComplexityRep where
  rep : Nat → Nat

-- WHAT the time lattice is (WHY each member exists):
-- `Zero` = empty computation, bottom, every program is `≥` it.
-- `O1` = constant-bounded fragment (single op, guarded dereference).
-- `log` / `poly k` = first quantitative insertions (halving loops,
--   nested fixed loops); further degrees slot into the `poly` chain.
-- `HALTS` = terminates, bound unnamed (ceiling of all quantitative).
-- `UNBOUND` = provably diverges (server loop); incomparable with the
--   finite chain, since divergence is neither cheaper nor pricier.
-- `UNDECIDABLE` = unknown, default for unanalysed code, unique top.
inductive ZeroTimeComplexity where | mk
inductive TimeComplexity_O1 where | mk
inductive TimeComplexity_HALTS where | mk
inductive TimeComplexity_UNBOUND where | mk
inductive TimeComplexity_UNDECIDABLE where | mk
inductive TimeComplexity_log where | mk
inductive TimeComplexity_poly (k : Nat) where | mk

instance : CComplexity .time ZeroTimeComplexity where isComplexity := True
instance : CComplexity .time TimeComplexity_O1 where isComplexity := True
instance : CComplexity .time TimeComplexity_HALTS where isComplexity := True
instance : CComplexity .time TimeComplexity_UNBOUND where isComplexity := True
instance : CComplexity .time TimeComplexity_UNDECIDABLE where isComplexity := True
instance : CComplexity .time TimeComplexity_log where isComplexity := True
instance (k : Nat) : CComplexity .time (TimeComplexity_poly k) where isComplexity := True

-- Deprecated abbrevs (one-commit compat).
abbrev CZeroComplexity := ZeroTimeComplexity
abbrev CTerminates := TimeComplexity_HALTS
abbrev CUndecidable := TimeComplexity_UNDECIDABLE

example : CComplexity .time ZeroTimeComplexity := inferInstance
example : CComplexity .time TimeComplexity_O1 := inferInstance
example : CComplexity .time TimeComplexity_HALTS := inferInstance
example : CComplexity .time TimeComplexity_UNBOUND := inferInstance
example : CComplexity .time TimeComplexity_UNDECIDABLE := inferInstance
example (k : Nat) : CComplexity .time (TimeComplexity_poly k) := inferInstance

-- WHAT the memory lattice is (mirror of time, WHY separate: memory
-- composes by high-water mark, not addition, so it needs its own
-- chain with the same shape):
-- `Zero` = no allocation beyond the ambient frame (bottom).
-- `O1` = one scalar local / one fixed block (`≤ K` cells).
-- `BOUNDED` = finite but unnamed (analogue of `HALTS`).
-- `GROWING` = grows without proven ceiling, e.g. unbounded append
--   (analogue of `UNBOUND`, incomparable with the finite chain).
-- `UNKNOWN` = unknown allocation, default/top.
inductive ZeroMemoryComplexity where | mk
inductive MemoryComplexity_O1 where | mk
inductive MemoryComplexity_BOUNDED where | mk
inductive MemoryComplexity_GROWING where | mk
inductive MemoryComplexity_UNKNOWN where | mk

instance : CComplexity .memory ZeroMemoryComplexity where isComplexity := True
instance : CComplexity .memory MemoryComplexity_O1 where isComplexity := True
instance : CComplexity .memory MemoryComplexity_BOUNDED where isComplexity := True
instance : CComplexity .memory MemoryComplexity_GROWING where isComplexity := True
instance : CComplexity .memory MemoryComplexity_UNKNOWN where isComplexity := True

example : CComplexity .memory ZeroMemoryComplexity := inferInstance
example : CComplexity .memory MemoryComplexity_O1 := inferInstance
example : CComplexity .memory MemoryComplexity_BOUNDED := inferInstance
example : CComplexity .memory MemoryComplexity_GROWING := inferInstance
example : CComplexity .memory MemoryComplexity_UNKNOWN := inferInstance

/-- WHAT the relationship tables are: each class declares its
immediate predecessors / successors / equals. WHY honest (never `[]`
by default): these lists *are* the lattice edges the graph reasons
about — an empty list claims "no neighbour", which must be true
(`UNBOUND` really has no finite predecessor). `equalTo` fixes the old
`eqialTo` typo; no alias is kept (no external uses).

Intended time shape: `Zero < O1 < HALTS < UNDECIDABLE` with
`UNBOUND < UNDECIDABLE` and `UNBOUND` otherwise incomparable; every
later quantitative class (`log`, `poly k`, and anything added via
`HasQuantRep`) sits between `O1` and `HALTS`. Memory mirrors it. -/
class CComplexityRelationship (axis : ResourceAxis) (α : Type u)
    [CComplexity axis α] where
  minimalStrictlySmaller : List Type
  minimalStrictlyLarger : List Type
  equalTo : List Type

instance : CComplexityRelationship .time ZeroTimeComplexity where
  minimalStrictlySmaller := []
  minimalStrictlyLarger := [TimeComplexity_O1]
  equalTo := []
instance : CComplexityRelationship .time TimeComplexity_O1 where
  minimalStrictlySmaller := [ZeroTimeComplexity]
  minimalStrictlyLarger := [TimeComplexity_HALTS]
  equalTo := []
instance : CComplexityRelationship .time TimeComplexity_log where
  minimalStrictlySmaller := [TimeComplexity_O1]
  minimalStrictlyLarger := [TimeComplexity_HALTS]
  equalTo := []
instance (k : Nat) : CComplexityRelationship .time (TimeComplexity_poly k) where
  minimalStrictlySmaller := [TimeComplexity_O1]
  minimalStrictlyLarger := [TimeComplexity_HALTS]
  equalTo := []
instance : CComplexityRelationship .time TimeComplexity_HALTS where
  minimalStrictlySmaller := [TimeComplexity_O1]
  minimalStrictlyLarger := [TimeComplexity_UNDECIDABLE]
  equalTo := []
instance : CComplexityRelationship .time TimeComplexity_UNBOUND where
  minimalStrictlySmaller := []
  minimalStrictlyLarger := [TimeComplexity_UNDECIDABLE]
  equalTo := []
instance : CComplexityRelationship .time TimeComplexity_UNDECIDABLE where
  minimalStrictlySmaller := [TimeComplexity_HALTS, TimeComplexity_UNBOUND]
  minimalStrictlyLarger := []
  equalTo := []

instance : CComplexityRelationship .memory ZeroMemoryComplexity where
  minimalStrictlySmaller := []
  minimalStrictlyLarger := [MemoryComplexity_O1]
  equalTo := []
instance : CComplexityRelationship .memory MemoryComplexity_O1 where
  minimalStrictlySmaller := [ZeroMemoryComplexity]
  minimalStrictlyLarger := [MemoryComplexity_BOUNDED]
  equalTo := []
instance : CComplexityRelationship .memory MemoryComplexity_BOUNDED where
  minimalStrictlySmaller := [MemoryComplexity_O1]
  minimalStrictlyLarger := [MemoryComplexity_UNKNOWN]
  equalTo := []
instance : CComplexityRelationship .memory MemoryComplexity_GROWING where
  minimalStrictlySmaller := []
  minimalStrictlyLarger := [MemoryComplexity_UNKNOWN]
  equalTo := []
instance : CComplexityRelationship .memory MemoryComplexity_UNKNOWN where
  minimalStrictlySmaller := [MemoryComplexity_BOUNDED, MemoryComplexity_GROWING]
  minimalStrictlyLarger := []
  equalTo := []

/-- WHAT tags are: a closed, decidable snapshot of the *base*
lattice, one constructor per base class. WHY closed: `decide` needs
`DecidableEq`, so freshness checks like `o1Time ≠ logTime` compute.
WHY still extensible despite being closed: new quantitative classes
do NOT need new tags — they use the open `HasQuantRep` + `QuantLE`
below (plain `BigO` on reps). Tags cover the base vocabulary the
tests close over; `polyTime k` already shows the pattern (one
constructor covers infinitely many degrees via its `k` parameter). -/
inductive ComplexityTag where
| zeroTime | o1Time | logTime | polyTime (k : Nat)
| haltsTime | unboundTime | undecidableTime
| zeroMem | o1Mem | boundedMem | growingMem | unknownMem
deriving DecidableEq, Repr

/-- WHAT `TagLE` is: the lattice order, read off as a table.
WHY each row has the shape it does:
- quant–quant (`Zero/O1/log/poly`, both memories): the entry *is* the
  `BigO` fact, so `≤` over classes is sound by construction w.r.t. `=O`
  inclusion — no edge by fiat.
- quant → `HALTS`/`BOUNDED`/`UNDECIDABLE`/`UNKNOWN`: `True` — every
  quantitative bound refines "some finite bound exists" / "unknown"
  by forgetting detail (existential instantiation).
- `HALTS`/`UNBOUND → UNDECIDABLE`, `BOUNDED`/`GROWING → UNKNOWN`,
  reflexives: `True` — the qualitative spine.
- everything else: `False` — incomparability (`HALTS ≰ UNBOUND`,
  `UNBOUND ≰ HALTS`), cross-axis (`time ≰ mem`), qual → quant
  (forgetting never runs backwards). The catch-all `_, _ => False`
  is load-bearing: it is what makes `¬ (HALTS ≤ UNBOUND)` provable
  (`¬ False`).
WHY no shadow nodes: the order is derived on the fly from this table
+ `BigO.trans` (`TagLE_trans` checks all 12³ cases), so splitting
`A < B` into `A < X < B` only *adds* rows — existing types are
untouched. -/
def TagLE : ComplexityTag → ComplexityTag → Prop
| .zeroTime, .zeroTime => BigO gZero gZero
| .zeroTime, .o1Time => BigO gZero g1
| .zeroTime, .logTime => BigO gZero glog
| .zeroTime, .polyTime k => BigO gZero (gpoly k)
| .zeroTime, .haltsTime => True
| .zeroTime, .undecidableTime => True
| .o1Time, .o1Time => BigO g1 g1
| .o1Time, .logTime => BigO g1 glog
| .o1Time, .polyTime k => BigO g1 (gpoly k)
| .o1Time, .haltsTime => True
| .o1Time, .undecidableTime => True
| .logTime, .logTime => BigO glog glog
| .logTime, .polyTime k => BigO glog (gpoly k)
| .logTime, .haltsTime => True
| .logTime, .undecidableTime => True
| .polyTime k₁, .polyTime k₂ => BigO (gpoly k₁) (gpoly k₂)
| .polyTime _, .haltsTime => True
| .polyTime _, .undecidableTime => True
| .haltsTime, .haltsTime => True
| .haltsTime, .undecidableTime => True
| .unboundTime, .unboundTime => True
| .unboundTime, .undecidableTime => True
| .undecidableTime, .undecidableTime => True
| .zeroMem, .zeroMem => BigO gZero gZero
| .zeroMem, .o1Mem => BigO gZero g1
| .zeroMem, .boundedMem => True
| .zeroMem, .unknownMem => True
| .o1Mem, .o1Mem => BigO g1 g1
| .o1Mem, .boundedMem => True
| .o1Mem, .unknownMem => True
| .boundedMem, .boundedMem => True
| .boundedMem, .unknownMem => True
| .growingMem, .growingMem => True
| .growingMem, .unknownMem => True
| .unknownMem, .unknownMem => True
| _, _ => False

instance : LE ComplexityTag where
  le := TagLE

theorem TagLE_refl (t : ComplexityTag) : TagLE t t := by
  match t with
  | .zeroTime => show BigO gZero gZero; exact BigO.refl _
  | .o1Time => show BigO g1 g1; exact BigO.refl _
  | .logTime => show BigO glog glog; exact BigO.refl _
  | .polyTime _ => show BigO _ _; exact BigO.refl _
  | .haltsTime => trivial
  | .unboundTime => trivial
  | .undecidableTime => trivial
  | .zeroMem => show BigO gZero gZero; exact BigO.refl _
  | .o1Mem => show BigO g1 g1; exact BigO.refl _
  | .boundedMem => trivial
  | .growingMem => trivial
  | .unknownMem => trivial

/-- WHY `TagLE_trans` is proved by brute force (`cases` × 1728 +
`first | trivial | BigO.trans | False.elim`): the table mixes three
kinds of entries (`BigO`, `True`, `False`), and each triple falls into
exactly one bucket — `True` goal (`trivial`), all-`BigO`
(`BigO.trans`), or contradictory hypothesis (`False.elim`). No clever
rank function needed; the case split *is* the proof that the table is
a preorder. -/
theorem TagLE_trans {a b c : ComplexityTag} :
    TagLE a b → TagLE b c → TagLE a c := by
  intro h1 h2
  cases a <;> cases b <;> cases c <;> simp only [TagLE] at h1 h2 ⊢
  all_goals (first | trivial | exact BigO.trans h1 h2 | exact False.elim h1 | exact False.elim h2)

/-- WHAT `HasTag` is vs `TagOf` (read carefully — the split is
deliberate and load-bearing):
- `HasTag` is an *open typeclass*: `ComplexityLE a b` for concrete `a b`
  reduces definitionally to `TagLE tag_a tag_b`, so `¬ (HALTS ≤ UNBOUND)`
  is just `¬ False` — no inversion, no type-equality proofs. New files
  *could* add `HasTag` instances, but new tags need a closed inductive,
  so quantitative extension uses `HasQuantRep` instead (below).
- `TagOf` is a *closed inductive relation* used *existentially* inside
  `can_insert`: insertion proofs *construct* canonical witnesses
  (`⟨.o1Time, .o1Time, bigO_one_le_log, by decide⟩`) and never
  *destruct* them, which is exactly what dodges the unprovable
  `ZeroTime ≠ O1`-as-`Type`s problem (distinct single-constructor
  inductives have no distinguishing property for `cases` to exploit).
WHY two mechanisms, not one: `HasTag` gives clean ordering statements;
`TagOf` gives constructible insertion evidence. -/
class HasTag (α : Type) where
  tag : ComplexityTag

instance : HasTag ZeroTimeComplexity where tag := .zeroTime
instance : HasTag TimeComplexity_O1 where tag := .o1Time
instance : HasTag TimeComplexity_log where tag := .logTime
instance (k : Nat) : HasTag (TimeComplexity_poly k) where tag := .polyTime k
instance : HasTag TimeComplexity_HALTS where tag := .haltsTime
instance : HasTag TimeComplexity_UNBOUND where tag := .unboundTime
instance : HasTag TimeComplexity_UNDECIDABLE where tag := .undecidableTime
instance : HasTag ZeroMemoryComplexity where tag := .zeroMem
instance : HasTag MemoryComplexity_O1 where tag := .o1Mem
instance : HasTag MemoryComplexity_BOUNDED where tag := .boundedMem
instance : HasTag MemoryComplexity_GROWING where tag := .growingMem
instance : HasTag MemoryComplexity_UNKNOWN where tag := .unknownMem

/-- WHAT `TagOf` is: see `HasTag` above — the closed inductive side
of the pair, kept so `can_insert` witnesses are *constructed*
(`TagOf.o1Time`, …) rather than inferred. One constructor per base
class; the `k` parameter on `polyTime` means all degrees are covered
by one constructor. -/
inductive TagOf : Type → ComplexityTag → Prop where
| zeroTime : TagOf ZeroTimeComplexity .zeroTime
| o1Time : TagOf TimeComplexity_O1 .o1Time
| logTime : TagOf TimeComplexity_log .logTime
| polyTime (k : Nat) : TagOf (TimeComplexity_poly k) (.polyTime k)
| haltsTime : TagOf TimeComplexity_HALTS .haltsTime
| unboundTime : TagOf TimeComplexity_UNBOUND .unboundTime
| undecidableTime : TagOf TimeComplexity_UNDECIDABLE .undecidableTime
| zeroMem : TagOf ZeroMemoryComplexity .zeroMem
| o1Mem : TagOf MemoryComplexity_O1 .o1Mem
| boundedMem : TagOf MemoryComplexity_BOUNDED .boundedMem
| growingMem : TagOf MemoryComplexity_GROWING .growingMem
| unknownMem : TagOf MemoryComplexity_UNKNOWN .unknownMem

/-- WHAT class ordering is for users: `ComplexityLE a b` with the two
`HasTag` instances inferred. WHY instances, not explicit tags: call
sites write `ComplexityLE O1 HALTS` and get `TagLE o1Time haltsTime`
(= `True`, i.e. `trivial`) or `TagLE o1Time logTime`
(= `BigO g1 glog`, i.e. `bigO_one_le_log`) definitionally. Types
*without* a `HasTag` instance simply have no ordering statement —
incomparable by absence, which is the safe default for unknown code. -/
def ComplexityLE (a b : Type) [HasTag a] [HasTag b] : Prop :=
  TagLE (HasTag.tag a) (HasTag.tag b)

/-- WHAT follows: each base edge as a checked theorem (not a comment).
WHY theorems, not comments: `example`/`theorem` is machine-checked —
if someone edits `TagLE` inconsistently, these fail to compile.
Quantitative edges carry their `BigO` proof term (`bigO_zero_le_one`,
`bigO_one_le_log`); qualitative edges are `trivial` (`True` by
forgetting). The `¬ (HALTS ≤ UNBOUND)` proof is `simp` reducing both
sides to `¬ False` — the catch-all `_, _ => False` doing real work. -/
theorem complexityLE_zero_o1 : ComplexityLE ZeroTimeComplexity TimeComplexity_O1 :=
  bigO_zero_le_one

theorem complexityLE_o1_halts : ComplexityLE TimeComplexity_O1 TimeComplexity_HALTS :=
  trivial

theorem complexityLE_halts_undecidable :
    ComplexityLE TimeComplexity_HALTS TimeComplexity_UNDECIDABLE :=
  trivial

theorem complexityLE_unbound_undecidable :
    ComplexityLE TimeComplexity_UNBOUND TimeComplexity_UNDECIDABLE :=
  trivial

theorem complexityLE_o1_log : ComplexityLE TimeComplexity_O1 TimeComplexity_log :=
  bigO_one_le_log

theorem complexityLE_log_halts : ComplexityLE TimeComplexity_log TimeComplexity_HALTS :=
  trivial

theorem complexityLE_zeroMem_o1Mem :
    ComplexityLE ZeroMemoryComplexity MemoryComplexity_O1 :=
  bigO_zero_le_one

theorem complexityLE_o1Mem_bounded :
    ComplexityLE MemoryComplexity_O1 MemoryComplexity_BOUNDED :=
  trivial

theorem complexityLE_bounded_unknown :
    ComplexityLE MemoryComplexity_BOUNDED MemoryComplexity_UNKNOWN :=
  trivial

theorem complexityLE_growing_unknown :
    ComplexityLE MemoryComplexity_GROWING MemoryComplexity_UNKNOWN :=
  trivial

theorem complexityLE_halts_not_unbound :
    ¬ ComplexityLE TimeComplexity_HALTS TimeComplexity_UNBOUND := by
  simp [ComplexityLE, TagLE]

/-- WHAT the graph is: just the knowledge base — the *list* of known
class types. WHY a bare list (not a rich structure): all meaning lives
in the relationship tables + `BigO` proofs; the graph only records
*which* classes exist, so insertion is *additive* (cons one more type)
and existing entries are never rewritten — that is the whole answer
to the old `CComplexityShadowNode` sketch (list + derived order on
the fly; the shadow-node comment it replaced is gone, exactly one
mechanism remains). `insert` takes its `{axis}` implicitly so one graph
mixes both axes (each entry brings its own `CComplexity axis` proof). -/
inductive CComplexityGraph : (List (Type u)) -> Type (u+1) where
  | nil : CComplexityGraph []
  | insert {τs : List (Type u)} {axis : ResourceAxis}
     (τ : Type u ) [CComplexity axis τ]
     (_: CComplexityGraph τs ): CComplexityGraph (τ :: τs)

/-- WHAT graph `LE` means: graphs with a *fixed* base list are all
equivalent (same knowledge — there is essentially one value per list),
so `le _ _ := True`. WHY not `False` (the old stub): `False` claimed
no graph is below another, not even itself, which killed `le_refl`.
The REAL lattice order is `TagLE`/`ComplexityLE`/`QuantLE`; inclusion
*across* different bases is `graphInclusion` below. -/
instance {α : List (Type u)} : LE (CComplexityGraph α) where
  le _ _ := True

/-- WHAT cross-base inclusion means: every class known in `l₁` is known
in `l₂`. WHY: extending knowledge (`base → base + log`) is list
growth; this is the Prop that growth respects. -/
def graphInclusion (l₁ l₂ : List (Type u)) : Prop :=
  ∀ τ ∈ l₁, τ ∈ l₂

/-- WHAT insertion acceptance means (closed, base-tag version): `τ` may
join knowledge `τs` iff (1) every declared predecessor already sits
*strictly* below it, (2) it sits strictly below every declared
successor, (3) its tag is fresh in `τs`, (4–5) preds/succs are already
known. WHY each conjunct:
- (1–2) are the two `=O` facts from the proposal §4 — the *witness*,
  not an assertion. Quantitative–quantitative (`O1 ≤ log`) is a `BigO`
  proof term; quantitative→qualitative (`log ≤ HALTS`) is `True`
  (forgetting needs no numbers). Strictness (`tp ≠ tτ`, `decide`d) is
  what keeps `O1` and `O_log` from collapsing.
- (3) is tag-level freshness (`∃ tp … tp ≠ tτ`, *constructed* per
  element) — deliberately NOT `τ ∉ τs`: `¬ (A = B)` for distinct
  single-constructor inductives is unprovable in Lean (no distinguishing
  property for `cases`/`decide` to exploit; `DecidableEq (Type)` does
  not exist), so type-level non-membership cannot be discharged and is
  not asked for. Tags are data (`DecidableEq`), so tag freshness is.
- (4–5) need only `Mem.head`/`Mem.tail` constructors (`rfl`-style, no
  disequalities) — that is WHY they are provable at all.
For NEW classes prefer `can_insert_quant_open` below (no tags needed);
this closed predicate stays for the base vocabulary the tests close over. -/
def can_insert_complexity_class_to_grapth {τs : List Type} {axis : ResourceAxis}
  (τ : Type) [CComplexity axis τ] [CComplexityRelationship axis τ]
  (_ : CComplexityGraph τs) : Prop :=
  ∃ tτ, TagOf τ tτ ∧
    (∀ p : Type, p ∈ CComplexityRelationship.minimalStrictlySmaller (axis := axis) (α := τ) →
      ∃ tp, TagOf p tp ∧ TagLE tp tτ ∧ tp ≠ tτ) ∧
    (∀ s : Type, s ∈ CComplexityRelationship.minimalStrictlyLarger (axis := axis) (α := τ) →
      ∃ ts, TagOf s ts ∧ TagLE tτ ts ∧ tτ ≠ ts) ∧
    (∀ p, p ∈ τs → ∃ tp, TagOf p tp ∧ tp ≠ tτ) ∧
    (∀ p : Type, p ∈ CComplexityRelationship.minimalStrictlySmaller (axis := axis) (α := τ) → p ∈ τs) ∧
    (∀ s : Type, s ∈ CComplexityRelationship.minimalStrictlyLarger (axis := axis) (α := τ) → s ∈ τs)

/-- WHAT `baseTimeMem`/`baseGraph` are: the 10 base classes as a list
plus its knowledge-base value (nested `insert`s, axis given explicitly
so typeclass search never sees a stuck metavariable). WHY separate
`log`/`poly` from base: they are the *first insertion exercises* —
`can_insert_log` below shows the general mechanism working once, end
to end, with the two `BigO` proofs as evidence. -/
def baseTimeMem : List Type :=
  [ZeroTimeComplexity, TimeComplexity_O1, TimeComplexity_HALTS,
   TimeComplexity_UNBOUND, TimeComplexity_UNDECIDABLE,
   ZeroMemoryComplexity, MemoryComplexity_O1, MemoryComplexity_BOUNDED,
   MemoryComplexity_GROWING, MemoryComplexity_UNKNOWN]

def baseGraph : CComplexityGraph baseTimeMem :=
  CComplexityGraph.insert (axis := .time) ZeroTimeComplexity
    (CComplexityGraph.insert (axis := .time) TimeComplexity_O1
      (CComplexityGraph.insert (axis := .time) TimeComplexity_HALTS
        (CComplexityGraph.insert (axis := .time) TimeComplexity_UNBOUND
          (CComplexityGraph.insert (axis := .time) TimeComplexity_UNDECIDABLE
            (CComplexityGraph.insert (axis := .memory) ZeroMemoryComplexity
              (CComplexityGraph.insert (axis := .memory) MemoryComplexity_O1
                (CComplexityGraph.insert (axis := .memory) MemoryComplexity_BOUNDED
                  (CComplexityGraph.insert (axis := .memory) MemoryComplexity_GROWING
                    (CComplexityGraph.insert (axis := .memory) MemoryComplexity_UNKNOWN
                      CComplexityGraph.nil)))))))))

/-- WHAT `can_insert_log` shows: `O_log` splitting `O1 < HALTS` into
`O1 < O_log < HALTS`. WHY each bullet: preds `[O1]` → witness
`⟨o1Time, bigO_one_le_log, o1≠log⟩` (the growth fact); succs `[HALTS]`
→ `⟨haltsTime, trivial, log≠halts⟩` (forgetting); freshness → one
canonical tag per base element, each `≠ logTime` by `decide`;
memberships → `Mem.head`/`Mem.tail` constructors. No step asserts an
edge — every `≤` is a proof term. Copy this shape for your own class
(see the open recipe at the end of the file). -/
theorem can_insert_log :
    can_insert_complexity_class_to_grapth (axis := .time) (τ := TimeComplexity_log) baseGraph := by
  refine ⟨.logTime, .logTime, ?preds, ?succs, ?fresh, ?predMem, ?succMem⟩
  · intro p hp
    cases hp with
    | head as =>
      refine ⟨.o1Time, .o1Time, bigO_one_le_log, by decide⟩
    | tail b h =>
      cases h
  · intro s hs
    cases hs with
    | head as =>
      refine ⟨.haltsTime, .haltsTime, trivial, by decide⟩
    | tail b h =>
      cases h
  · intro p hp
    cases hp with
    | head as =>
      exact ⟨.zeroTime, .zeroTime, by decide⟩
    | tail b h =>
      cases h with
      | head as =>
        exact ⟨.o1Time, .o1Time, by decide⟩
      | tail b h =>
        cases h with
        | head as =>
          exact ⟨.haltsTime, .haltsTime, by decide⟩
        | tail b h =>
          cases h with
          | head as =>
            exact ⟨.unboundTime, .unboundTime, by decide⟩
          | tail b h =>
            cases h with
            | head as =>
              exact ⟨.undecidableTime, .undecidableTime, by decide⟩
            | tail b h =>
              cases h with
              | head as =>
                exact ⟨.zeroMem, .zeroMem, by decide⟩
              | tail b h =>
                cases h with
                | head as =>
                  exact ⟨.o1Mem, .o1Mem, by decide⟩
                | tail b h =>
                  cases h with
                  | head as =>
                    exact ⟨.boundedMem, .boundedMem, by decide⟩
                  | tail b h =>
                    cases h with
                    | head as =>
                      exact ⟨.growingMem, .growingMem, by decide⟩
                    | tail b h =>
                      cases h with
                      | head as =>
                        exact ⟨.unknownMem, .unknownMem, by decide⟩
                      | tail b h =>
                        cases h
  · intro p hp
    cases hp with
    | head as =>
      exact List.Mem.tail _ (List.Mem.head _)
    | tail b h =>
      cases h
  · intro s hs
    cases hs with
    | head as =>
      exact List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _))
    | tail b h =>
      cases h

theorem complexityLE_o1_log_halts :
    ComplexityLE TimeComplexity_O1 TimeComplexity_log ∧
    ComplexityLE TimeComplexity_log TimeComplexity_HALTS :=
  ⟨complexityLE_o1_log, complexityLE_log_halts⟩

/-! ## Open extension — YES, classes are extendable without touching this file

Short answer to "are complexity classes extendable?": **yes**.
`log` and `poly` above were added *inside* this file as the first
exercises, but every mechanism a new class needs is an *open
typeclass* (`CComplexity`, `CComplexityRelationship`, `HasQuantRep`
below): any new file can add instances for its own types. The only
closed items (`ComplexityTag`, `TagOf`, `TagLE`) cover the base
vocabulary — and new quantitative classes deliberately do NOT need new
tags: their order is plain `BigO` on representatives (`QuantLE`).

Concrete proof: `LeanC/ComplexityLinear.lean` adds `TimeComplexity_linear`
(`O(n)`, rep `fun n => n`) in its own file — zero edits here — with both
`BigO` edges and graph memberships. Read it as the worked example.

### Recipe (copy into your file; 6 steps, ~30 lines)

```lean
-- 1. The marker type (empty inductive: classes are static knowledge).
inductive TimeComplexity_X where | mk
-- 2. Register the axis (time shown; memory uses `.memory`).
instance : CComplexity .time TimeComplexity_X where isComplexity := True
-- 3. Give the envelope (the mathematical content of your class).
instance : HasQuantRep .time TimeComplexity_X where rep := g_X
-- 4. Declare intent: immediate preds/succs (must match step 5's proofs).
instance : CComplexityRelationship .time TimeComplexity_X where
  minimalStrictlySmaller := [<pred>]   -- e.g. [TimeComplexity_O1]
  minimalStrictlyLarger := [<succ>]    -- e.g. [TimeComplexity_HALTS]
  equalTo := []
-- 5. Prove the two growth facts on EXPLICIT rep functions
--    (no typeclasses on variables, so no coherence trap):
--    `BigO g_pred g_X` and (`BigO g_X g_succ`, or: any `BigO g_X g`
--    already places X below HALTS by forgetting — say so in a comment).
-- 6. Prove memberships `pred ∈ yourList`, `succ ∈ yourList` with
--    `List.Mem.head` / `List.Mem.tail` constructors.
```

### WHY the recipe is shaped this way (three traps it dodges)

1. **Why explicit rep functions in step 5, not
   `∀ p [HasQuantRep p], BigO (rep p) …`?** An instance-implicit over a
   *variable* `p` hands you an *arbitrary* instance transported to the
   concrete predecessor — with no uniqueness guarantee it equals the
   canonical one, so `rep_p = g_pred` is unprovable. `∃`-witnesses and
   explicit functions (`glog`, `glinear`) keep every proof about
   canonical, named terms.
2. **Why memberships via `Mem.head`/`Mem.tail`, never `decide`/`rfl` on
   types?** `a ∈ b :: l` splits into `head` (unifies the variable with
   the head — no equality to discharge) or `tail` (recurse). Proving
   `A ≠ B` for distinct single-constructor inductives is *unprovable*
   (`DecidableEq (Type)` does not exist; there is no distinguishing
   property), so anything needing type disequality — including a
   `τ ∉ τs` freshness clause — is deliberately absent from the open
   path. Freshness holds by construction: the new inductive is a new
   constant postdating every list it joins.
3. **Why is there no `τ ∉ τs` check?** Same reason: unstatable without
   type disequalities. The closed base predicate checks freshness at
   *tag* level (`DecidableEq` on data); the open path covers genuinely
   new growth rates whose reps differ, and documents equality cases
   (`equalTo`) in the relationship table instead of rejecting them.
-/

/-- WHAT `HasQuantRep` is: THE extension point. An open typeclass giving
a class type its representative envelope `rep : Nat → Nat`, i.e. the
`g` in `Class(g) = { f | BigO f g }`. WHY open (class, not inductive):
any file can add instances for new types — no base edits. WHY axis is
explicit: time and memory envelopes compose differently, so a rep is
meaningless without saying which resource it bounds. Base quantitative
classes get instances below; qualitative ones (`HALTS`, `UNBOUND`,
`UNDECIDABLE`, …) deliberately have NONE — they sit outside Big-O. -/
class HasQuantRep (axis : ResourceAxis) (α : Type) where
  rep : Nat → Nat

instance : HasQuantRep .time ZeroTimeComplexity where rep := gZero
instance : HasQuantRep .time TimeComplexity_O1 where rep := g1
instance : HasQuantRep .time TimeComplexity_log where rep := glog
instance (k : Nat) : HasQuantRep .time (TimeComplexity_poly k) where
  rep := gpoly k
instance : HasQuantRep .memory ZeroMemoryComplexity where rep := gZero
instance : HasQuantRep .memory MemoryComplexity_O1 where rep := g1

/-- WHAT `QuantLE` is: ordering for the open world — plain `BigO` on
canonical reps, no tags. WHY: a new file's `exact bigO_…` proof term
*is* the edge; nothing to look up, nothing closed to extend.
`QuantLE .time O1 X` unfolds to `BigO g1 g_X` — prove it, done. -/
def QuantLE {axis : ResourceAxis} (a b : Type)
    [HasQuantRep axis a] [HasQuantRep axis b] : Prop :=
  BigO (HasQuantRep.rep (axis := axis) (α := a))
    (HasQuantRep.rep (axis := axis) (α := b))

/-- WHY: every `QuantLE` is reflexive — `X ≤ X` needs no growth fact,
just `BigO.refl`. Used for the guard `1 =O 1` and reflexive closes. -/
theorem QuantLE.refl {axis : ResourceAxis} (a : Type) [HasQuantRep axis a] :
    QuantLE (axis := axis) a a :=
  BigO.refl _

/-- WHY: `QuantLE` chains — the open-world counterpart of `TagLE_trans`
for the quantitative fragment. Inserting `X` between `P < S` and later
`Y` between `X < S` composes via this, never touching base rows. -/
theorem QuantLE.trans {axis : ResourceAxis} {a b c : Type}
    [HasQuantRep axis a] [HasQuantRep axis b] [HasQuantRep axis c] :
    QuantLE (axis := axis) a b → QuantLE (axis := axis) b c →
      QuantLE (axis := axis) a c :=
  BigO.trans

end LeanC
