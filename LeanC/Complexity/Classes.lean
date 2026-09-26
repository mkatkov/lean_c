/-! Class markers: axes, `CComplexity`, base time/memory inductives,
and `CComplexityRelationship` tables.

Split out of `LeanC/Complexity.lean`. Pure declarations — no `BigO`
dependency. `Lattice` maps these to tags; `Graph` reads the
relationship tables as insertion obligations. -/
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

/-- WHAT a complexity class is in Lean: a marker type indexed by
axis. WHY a marker (not data): classes are *static* knowledge — which
envelope a fragment claims — so each class is an empty inductive with
one constructor; the meaning lives in the representative (`g1`,
`glog`, …) and the `BigO` proofs, not in values. The `Prop` field
is set to `True` on every instance (`isComplexity := True`), matching
the marker-field style of the other typeclasses in this codebase.

The three non-quantitative time classes sit *outside* Big-O, wrapping it:
- `HALTS`: `∃ g computable, BigO cost g` — some finite bound exists,
  unnamed. Every quantitative class refines it by instantiation.
- `UNBOUND`: proven divergence (no finite `g` exists *plus* an
  infinite-trace witness) — hence incomparable with the finite chain.
- `UNDECIDABLE`: no claim at all — the unique top both forget to. -/
class CComplexity (axis : ResourceAxis) (α : Type u) where
  isComplexity : Prop

-- WHAT the time lattice is (WHY each member exists):
-- `Zero` = empty computation, bottom, every program is `≥` it.
-- `O1` = constant-bounded fragment (single op, guarded dereference).
-- `log` / `poly k` = quantitative insertions (halving loops, nested
--   fixed loops); further degrees slot into the `poly` chain.
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
-- `log` / `poly k` = quantitative insertions (same reps as time, read as
--   live cells; e.g. divide-and-conquer scratch, nested fixed buffers).
-- `BOUNDED` = finite but unnamed (analogue of `HALTS`).
-- `GROWING` = grows without proven ceiling, e.g. unbounded append
--   (analogue of `UNBOUND`, incomparable with the finite chain).
-- `UNKNOWN` = unknown allocation, default/top.
inductive ZeroMemoryComplexity where | mk
inductive MemoryComplexity_O1 where | mk
inductive MemoryComplexity_log where | mk
inductive MemoryComplexity_poly (k : Nat) where | mk
inductive MemoryComplexity_BOUNDED where | mk
inductive MemoryComplexity_GROWING where | mk
inductive MemoryComplexity_UNKNOWN where | mk

instance : CComplexity .memory ZeroMemoryComplexity where isComplexity := True
instance : CComplexity .memory MemoryComplexity_O1 where isComplexity := True
instance : CComplexity .memory MemoryComplexity_log where isComplexity := True
instance (k : Nat) : CComplexity .memory (MemoryComplexity_poly k) where isComplexity := True
instance : CComplexity .memory MemoryComplexity_BOUNDED where isComplexity := True
instance : CComplexity .memory MemoryComplexity_GROWING where isComplexity := True
instance : CComplexity .memory MemoryComplexity_UNKNOWN where isComplexity := True

example : CComplexity .memory ZeroMemoryComplexity := inferInstance
example : CComplexity .memory MemoryComplexity_O1 := inferInstance
example : CComplexity .memory MemoryComplexity_log := inferInstance
example (k : Nat) : CComplexity .memory (MemoryComplexity_poly k) := inferInstance
example : CComplexity .memory MemoryComplexity_BOUNDED := inferInstance
example : CComplexity .memory MemoryComplexity_GROWING := inferInstance
example : CComplexity .memory MemoryComplexity_UNKNOWN := inferInstance

/-- WHAT the relationship tables are: each class declares its
immediate predecessors / successors / equals. WHY honest (never `[]`
by default): these lists *are* the lattice edges the graph reasons
about — an empty list claims "no neighbour", which must be true
(`UNBOUND` really has no finite predecessor). `equalTo` lists the
classes semantically equal to this one (same `BigO` envelope both
ways); the base equalities are `poly 0 = O1` on both axes
(`gpoly 0 = fun _ => 1`, see `gpoly_zero_eq_one`), so every other base
`equalTo` is `[]`.

Intended time shape: `Zero < O1 < HALTS < UNDECIDABLE` with
`UNBOUND < UNDECIDABLE` and `UNBOUND` otherwise incomparable; every
later quantitative class (`log`, `poly k` with `1 ≤ k`, and anything
added via `HasQuantRep`) sits between `O1` and `HALTS`. `poly 0` sits
IN `O1` (equal, not below). Memory mirrors it with
`Zero < O1 < BOUNDED < UNKNOWN`, `GROWING < UNKNOWN`, and
`log`/`poly k` between `O1` and `BOUNDED`.

NOTE on staleness: after inserting `log`/`poly`, `O1`'s
`minimalStrictlyLarger` still lists only the qualitative ceiling
(`HALTS`/`BOUNDED`). The tables record *admissible* neighbours for the
insertion check, not the transitively-closed minimal cover — the real
order is `TagLE`/`QuantLE` (`BigO` entailment), which stays consistent
via `BigO.trans` without rewriting existing rows (see `Lattice`). -/
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
/-- `poly k`: for `1 ≤ k` a strict quantitative rung (`O1 < poly k <
HALTS`); for `k = 0` degenerate — `gpoly 0 = 1`, so it IS `O1`
(`equalTo := [O1]`, no strict neighbours). The `if` needs only
`Decidable (k = 0)` (`Nat.decEq`), no base edits per degree. -/
instance (k : Nat) : CComplexityRelationship .time (TimeComplexity_poly k) where
  minimalStrictlySmaller := if k = 0 then [] else [TimeComplexity_O1]
  minimalStrictlyLarger := if k = 0 then [] else [TimeComplexity_HALTS]
  equalTo := if k = 0 then [TimeComplexity_O1] else []

/-- Unfolding helpers: the `if k = 0` in the `poly` relationship is
definitional (instance projection reduces to the field value), so `show`
exposes it and `if_neg`/`if_pos` discharge it. `Graph` proofs rewrite with
these instead of `simp`-through-instances (fragile). -/
theorem poly_small_eq (k : Nat) (hk : k ≠ 0) :
    CComplexityRelationship.minimalStrictlySmaller (axis := .time)
      (α := TimeComplexity_poly k) = [TimeComplexity_O1] := by
  show (if k = 0 then ([] : List Type) else [TimeComplexity_O1]) = _
  exact if_neg hk

theorem poly_large_eq (k : Nat) (hk : k ≠ 0) :
    CComplexityRelationship.minimalStrictlyLarger (axis := .time)
      (α := TimeComplexity_poly k) = [TimeComplexity_HALTS] := by
  show (if k = 0 then ([] : List Type) else [TimeComplexity_HALTS]) = _
  exact if_neg hk

theorem poly_equal_eq (k : Nat) (hk : k ≠ 0) :
    CComplexityRelationship.equalTo (axis := .time)
      (α := TimeComplexity_poly k) = ([] : List Type) := by
  show (if k = 0 then [TimeComplexity_O1] else ([] : List Type)) = _
  exact if_neg hk

theorem poly_small_zero :
    CComplexityRelationship.minimalStrictlySmaller (axis := .time)
      (α := TimeComplexity_poly 0) = ([] : List Type) := by
  show (if (0 : Nat) = 0 then ([] : List Type) else [TimeComplexity_O1]) = _
  exact if_pos rfl

theorem poly_large_zero :
    CComplexityRelationship.minimalStrictlyLarger (axis := .time)
      (α := TimeComplexity_poly 0) = ([] : List Type) := by
  show (if (0 : Nat) = 0 then ([] : List Type) else [TimeComplexity_HALTS]) = _
  exact if_pos rfl

theorem poly_equal_zero :
    CComplexityRelationship.equalTo (axis := .time)
      (α := TimeComplexity_poly 0) = [TimeComplexity_O1] := by
  show (if (0 : Nat) = 0 then [TimeComplexity_O1] else ([] : List Type)) = _
  exact if_pos rfl
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
instance : CComplexityRelationship .memory MemoryComplexity_log where
  minimalStrictlySmaller := [MemoryComplexity_O1]
  minimalStrictlyLarger := [MemoryComplexity_BOUNDED]
  equalTo := []
/-- `MemoryComplexity_poly k`: mirror of the time `poly` rung
(`O1 < poly k < BOUNDED` for `1 ≤ k`; `poly 0 = O1` via `equalTo`). -/
instance (k : Nat) : CComplexityRelationship .memory (MemoryComplexity_poly k) where
  minimalStrictlySmaller := if k = 0 then [] else [MemoryComplexity_O1]
  minimalStrictlyLarger := if k = 0 then [] else [MemoryComplexity_BOUNDED]
  equalTo := if k = 0 then [MemoryComplexity_O1] else []

theorem mem_poly_small_eq (k : Nat) (hk : k ≠ 0) :
    CComplexityRelationship.minimalStrictlySmaller (axis := .memory)
      (α := MemoryComplexity_poly k) = [MemoryComplexity_O1] := by
  show (if k = 0 then ([] : List Type) else [MemoryComplexity_O1]) = _
  exact if_neg hk

theorem mem_poly_large_eq (k : Nat) (hk : k ≠ 0) :
    CComplexityRelationship.minimalStrictlyLarger (axis := .memory)
      (α := MemoryComplexity_poly k) = [MemoryComplexity_BOUNDED] := by
  show (if k = 0 then ([] : List Type) else [MemoryComplexity_BOUNDED]) = _
  exact if_neg hk

theorem mem_poly_equal_eq (k : Nat) (hk : k ≠ 0) :
    CComplexityRelationship.equalTo (axis := .memory)
      (α := MemoryComplexity_poly k) = ([] : List Type) := by
  show (if k = 0 then [MemoryComplexity_O1] else ([] : List Type)) = _
  exact if_neg hk

theorem mem_poly_small_zero :
    CComplexityRelationship.minimalStrictlySmaller (axis := .memory)
      (α := MemoryComplexity_poly 0) = ([] : List Type) := by
  show (if (0 : Nat) = 0 then ([] : List Type) else [MemoryComplexity_O1]) = _
  exact if_pos rfl

theorem mem_poly_large_zero :
    CComplexityRelationship.minimalStrictlyLarger (axis := .memory)
      (α := MemoryComplexity_poly 0) = ([] : List Type) := by
  show (if (0 : Nat) = 0 then ([] : List Type) else [MemoryComplexity_BOUNDED]) = _
  exact if_pos rfl

theorem mem_poly_equal_zero :
    CComplexityRelationship.equalTo (axis := .memory)
      (α := MemoryComplexity_poly 0) = [MemoryComplexity_O1] := by
  show (if (0 : Nat) = 0 then [MemoryComplexity_O1] else ([] : List Type)) = _
  exact if_pos rfl
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

end LeanC
