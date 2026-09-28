import LeanC.Complexity.BigO
import LeanC.Complexity.Growth
import LeanC.Complexity.Classes

/-! Closed base order: `ComplexityTag`, `TagLE`, `HasTag`/`TagOf`,
and `ComplexityLE` edge theorems.

Split out of `LeanC/Complexity.lean`. The `TagLE` table entries *are*
the `BigO`/`Growth` facts, so this imports `BigO`, `Growth`, and
`Classes`. `Graph` builds on this; the open `Quant` path does not. -/
namespace LeanC

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
- `Zero` edges are `True` BY STIPULATION, never `BigO` (see below).
- quant–quant (`O1/log/poly` on both axes): the entry *is* the
  `BigO` fact, so `≤` over classes is sound by construction w.r.t. `=O`
  inclusion — no edge by fiat. Memory reuses the same reps as time.
- quant → `HALTS`/`BOUNDED`/`UNDECIDABLE`/`UNKNOWN`: `True` — every
  quantitative bound refines "some finite bound exists" / "unknown"
  by forgetting detail. At value level this forgetting is the
  existential `costInClass → IsFiniteCost` (`Bridge`: exhibiting the
  class rep as the witness); the tag table records the class-level
  shadow of that implication.
- `HALTS`/`UNBOUND → UNDECIDABLE`, `BOUNDED`/`GROWING → UNKNOWN`,
  reflexives: `True` — the qualitative spine. These are STIPULATED:
  under total `Nat → Nat` costs every cost is finite
  (`every_cost_finite` in `Bridge`, via `BigO.refl`), so `UNBOUND`
  (divergence) has no cost-function inhabitants yet — it awaits a
  partial-cost/trace model. Tag-level `UNBOUND ≤ UNDECIDABLE` records
  the intended forgetting; value-level divergence is `IsDivergentCost`,
  currently empty by `no_divergent_cost`.
- everything else: `False` — incomparability (`HALTS ≰ UNBOUND`,
  `UNBOUND ≰ HALTS`), cross-axis (`time ≰ mem`), qual → quant
  (forgetting never runs backwards). The catch-all `_, _ => False`
  is load-bearing: it is what makes `¬ (HALTS ≤ UNBOUND)` provable
  (`¬ False`).

WHY `Zero` is separated from `BigO`: the `max · 1` guard in `BigO`
equates `0` and `1` — both `bigO_zero_le_one : BigO gZero g1` AND
`bigO_one_le_zero : BigO g1 gZero` hold (see `Growth`). Ordering `Zero`
via `BigO` would therefore collapse the bottom (`O1 ≤ Zero` would be
provable). `TagLE` stores `Zero ≤ X` as `True` (empty computation is
below everything finite by definition) and exposes NO `X ≤ Zero` row
for quantitative `X` (catch-all `False`), so `O1 ≤ Zero` stays
unprovable even though `BigO g1 gZero` holds. `Zero` has NO
`HasQuantRep` (see `Quant`): there is no `QuantLE` involving `Zero` at
all — lattice strictness `Zero < O1` lives ONLY in tags, never in reps.
WHY no shadow nodes: the order is derived on the fly from this table
+ `BigO.trans` (`TagLE_trans` checks all cases by brute force), so
splitting `A < B` into `A < X < B` only *adds* rows — existing types are
untouched. Relationship tables (`Classes`) keep listing the qualitative
ceiling as the declared successor; the real order chains through the
new rows. -/
def TagLE : ComplexityTag → ComplexityTag → Prop
| .zeroTime, .zeroTime => True
| .zeroTime, .o1Time => True
| .zeroTime, .logTime => True
| .zeroTime, .polyTime _ => True
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
| .zeroMem, .zeroMem => True
| .zeroMem, .o1Mem => True
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
  | .zeroTime => trivial
  | .o1Time => show BigO g1 g1; exact BigO.refl _
  | .logTime => show BigO glog glog; exact BigO.refl _
  | .polyTime _ => show BigO _ _; exact BigO.refl _
  | .haltsTime => trivial
  | .unboundTime => trivial
  | .undecidableTime => trivial
  | .zeroMem => trivial
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
a preorder. Memory quantitative classes (`log`/`poly` on the memory
axis) deliberately have NO tags — they use the open `HasQuantRep` path
(see `Quant`), so the closed tag count stays at 12 and this proof stays
tractable. -/
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
by one constructor. Memory quantitative classes (`log`/`poly`) have no
tags by design — they insert via the open `HasQuantRep` path (see
`Quant`), which needs no closed inductive. -/
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
`Zero` edges are `trivial` (bottom by stipulation — deliberately NOT
`bigO_zero_le_one`, since `bigO_one_le_zero` also holds and would
collapse the bottom; see `TagLE` docs). Quantitative edges carry their
`BigO` proof term (`bigO_one_le_log`); qualitative edges are `trivial`
(`True` by forgetting — the value-level shadow is
`costInClass → IsFiniteCost` in `Bridge`, while `UNBOUND`-involving
edges remain stipulated pending a partial-cost model). Incomparability
proofs are `simp` reducing both sides to `¬ False` — the catch-all
`_, _ => False` doing real work. So is `¬ (O1 ≤ Zero)`: no `o1 → zero`
row exists even though `BigO g1 gZero` holds — the separation made
visible. -/
theorem complexityLE_zero_o1 : ComplexityLE ZeroTimeComplexity TimeComplexity_O1 :=
  trivial

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
  trivial

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
    ¬ ComplexityLE TimeComplexity_HALTS TimeComplexity_UNBOUND :=
  fun h => h

/-- Symmetric incomparability: `UNBOUND ≰ HALTS` (divergence is not a
finite bound). Both directions are `¬ False`; the pair locks the
"side branch" shape of the time lattice. -/
theorem complexityLE_unbound_not_halts :
    ¬ ComplexityLE TimeComplexity_UNBOUND TimeComplexity_HALTS :=
  fun h => h

/-- Memory mirror of time incomparability: `BOUNDED` (finite, unnamed)
and `GROWING` (known-unbounded) are mutually incomparable, both below
`UNKNOWN`. -/
theorem complexityLE_bounded_not_growing :
    ¬ ComplexityLE MemoryComplexity_BOUNDED MemoryComplexity_GROWING :=
  fun h => h

theorem complexityLE_growing_not_bounded :
    ¬ ComplexityLE MemoryComplexity_GROWING MemoryComplexity_BOUNDED :=
  fun h => h

/-- `Zero` is incomparable with the divergent branch: no
`zero → unbound` / `zero → growing` rows exist (catch-all `False`).
An empty computation is below the finite chain by stipulation, but it
is neither above nor below known-divergence. -/
theorem complexityLE_zero_not_unbound :
    ¬ ComplexityLE ZeroTimeComplexity TimeComplexity_UNBOUND :=
  fun h => h

theorem complexityLE_unbound_not_zero :
    ¬ ComplexityLE TimeComplexity_UNBOUND ZeroTimeComplexity :=
  fun h => h

theorem complexityLE_zeroMem_not_growing :
    ¬ ComplexityLE ZeroMemoryComplexity MemoryComplexity_GROWING :=
  fun h => h

/-- `O1 ≰ Zero`: no `o1 → zero` row exists (catch-all `False`), even
though `bigO_one_le_zero : BigO g1 gZero` holds. This theorem is the
machine-checked witness that `Zero` separation works: the `BigO`
collapse does NOT leak into the lattice. -/
theorem complexityLE_o1_not_zero :
    ¬ ComplexityLE TimeComplexity_O1 ZeroTimeComplexity :=
  fun h => h

theorem complexityLE_o1Mem_not_zeroMem :
    ¬ ComplexityLE MemoryComplexity_O1 ZeroMemoryComplexity :=
  fun h => h

/-! ## Fix 4 — `poly 0 = O1`: coherence between `TagLE` and `QuantLE`

`QuantLE` equates them (`quant_poly0_le_o1` / `quant_o1_le_poly0`:
mutual `BigO`, since `gpoly 0 = fun _ => 1`). `TagLE` deliberately
does NOT: `TagLE (.polyTime 0) .o1Time` is catch-all `False`
(intensional tags distinguish `polyTime 0` from `o1Time`; equality
lives in reps, not tags). This is by design — tags are syntactic
freshness (`DecidableEq` on constructors), growth equality is semantic
(mutual `BigO`) — but it must be stated, not discovered. Rule: use
`QuantLE` for quantitative reasoning about `poly 0` (costs, `CostSpec`,
insertion); use `TagLE`/`ComplexityLE` only for the qualitative spine
(`Zero < O1 < HALTS < UNDECIDABLE`, incomparabilities). The theorems
below machine-check both sides of the discrepancy.

Base-count reconciliation: `baseTimeMem` has length 10 (the qualitative
spine on both axes: 5 time + 5 memory, NO `log`/`poly` — they are
quantitative insertions, not base vocabulary). `ComplexityTag` has 12
constructors (10 + `logTime` + `polyTime k` for the TIME quantitative
rungs that predate the open path; memory `log`/`poly` deliberately
have NO tags and insert via `HasQuantRep`). Only the four centralized
helpers (`o1_mem_base`, `halts_mem_base`, `o1Mem_mem_base`,
`bounded_mem_base` in `Graph`) break on `baseTimeMem` reorder — callers
stay intact. -/

/-- `poly 0 ≰ O1` in tags (intensional distinction), even though
`QuantLE` equates them both ways. -/
theorem complexityLE_poly0_not_o1 :
    ¬ ComplexityLE (TimeComplexity_poly 0) TimeComplexity_O1 :=
  fun h => h

/-- `O1 ≤ poly 0` in tags DOES hold (via the general
`o1 → poly k` `BigO` row at `k = 0`: `bigO_one_le_poly 0`). -/
theorem complexityLE_o1_poly0 :
    ComplexityLE TimeComplexity_O1 (TimeComplexity_poly 0) :=
  bigO_one_le_poly 0

end LeanC
