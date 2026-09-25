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

end LeanC
