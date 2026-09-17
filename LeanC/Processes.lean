import LeanC.Context
import LeanC.Complexity

/-!
# Abstract process tree for lean_c

This module defines core process related types:

we can have sequencial process, branching process, loop process, and demonic loop process.
Every statement carries a worst resource bound (`stmtBound`, linked to
`CComplexity` via `BigO`); sequential time adds, memory takes the
high-water mark, branching takes max (+ `O1` guard).

Loops: stub only — unbounded/`UNBOUND` loops propagate `UNBOUND` time
(see `loopBoundStub` comment); full variant rules are future work.
-/

namespace LeanC
universe u v

/-- WHAT a statement is: soundness plus a worst bound. WHY the bound
is a field (not a separate table): every process combinator below
(`nil`/`cons`/branch) must produce a bound for its composite from the
bounds of its parts — the field forces each new statement form to say
what it costs at introduction time. The proof obligation behind the
field: with `cost : Nat → Nat` the statement's size-indexed
step/allocation function, `BigO cost stmtBound.timeRep` (and
symmetrically for memory) — i.e. membership in the claimed class. -/
class CStatement (Γ : Type v) [CContext Γ] (α : Type u) where
  isStatementSound : Prop
  stmtBound : ResourceBound

/-- WHAT sequential composition costs: time ADDS (run one fragment,
then the next — steps accumulate, and `BigO.add` is exactly the proof
that the sum stays inside the summed envelope); memory takes the
HIGH-WATER mark (`max`, via `BigO.max_bound`) since stack slots are
reused while heap blocks persist per `CArray.isAllocated`. WHY the
asymmetry: it is the whole reason time and memory are separate axes. -/
def seqBound (b₁ b₂ : ResourceBound) : ResourceBound :=
  { timeRep := fun n => b₁.timeRep n + b₂.timeRep n,
    memRep := fun n => Nat.max (b₁.memRep n) (b₂.memRep n) }

/-- WHAT branching costs: BOTH components take the `max` of the two
sides (worst branch wins), and time adds one `O1` guard sequenced first
(`+ 1`). WHY the `+ 1`: the guard test itself executes (one abstract
step per the cost semantics); it is absorbed by `O(1)` folding
(`const_le_one`), so two `O1` branches still bound `O1`
(`max(1,1)+1 = 2 =O 1`) — while two `Zero` branches correctly bound
`O1`, not `Zero` (the guard ran). Forgetting the `+1` would
under-approximate; the lemmas below show the `+1` never escapes `O`. -/
def branchBound (b₁ b₂ : ResourceBound) : ResourceBound :=
  { timeRep := fun n => Nat.max (b₁.timeRep n) (b₂.timeRep n) + 1,
    memRep := fun n => Nat.max (b₁.memRep n) (b₂.memRep n) }

/-- WHY `seq_bound_add`: this theorem *is* the sequence-time rule —
`seqBound`'s time field is definitionally the sum `BigO.add` concludes
about, so the proof is one application. -/
theorem seq_bound_add {b₁ b₂ : ResourceBound} {g₁ g₂ : Nat → Nat}
    (h₁ : BigO b₁.timeRep g₁) (h₂ : BigO b₂.timeRep g₂) :
    BigO (seqBound b₁ b₂).timeRep (fun n => g₁ n + g₂ n) :=
  BigO.add h₁ h₂

/-- WHY `seq_bound_max_mem`: the sequence-memory rule — `seqBound`'s
memory field is definitionally the `max` `BigO.max_bound` concludes
about. Together with `seq_bound_add` this says: walk the `cons` chain
and time accumulates while memory peaks — the two axes diverging from
one definition. -/
theorem seq_bound_max_mem {b₁ b₂ : ResourceBound} {g₁ g₂ : Nat → Nat}
    (h₁ : BigO b₁.memRep g₁) (h₂ : BigO b₂.memRep g₂) :
    BigO (seqBound b₁ b₂).memRep (fun n => Nat.max (g₁ n) (g₂ n)) :=
  BigO.max_bound h₁ h₂

/-- WHY `branch_bound_max_mem`: the branch-memory rule — worst side
wins, no guard cost in memory (testing a guard allocates nothing). -/
theorem branch_bound_max_mem {b₁ b₂ : ResourceBound} {g₁ g₂ : Nat → Nat}
    (h₁ : BigO b₁.memRep g₁) (h₂ : BigO b₂.memRep g₂) :
    BigO (branchBound b₁ b₂).memRep (fun n => Nat.max (g₁ n) (g₂ n)) :=
  BigO.max_bound h₁ h₂

/-- WHY `branch_bound_max_time`: the branch-time rule *including* the
guard. Proof shape: `max` the sides (`max_bound`), then `add` the `1`
(`refl` says `1 =O 1`) — the `+ 1` in the envelope is discharged by the
`+ 1` in the bound, so the guard provably never escapes the claimed
class. `simpa [branchBound]` only unfolds the definition. -/
theorem branch_bound_max_time {b₁ b₂ : ResourceBound} {g₁ g₂ : Nat → Nat}
    (h₁ : BigO b₁.timeRep g₁) (h₂ : BigO b₂.timeRep g₂) :
    BigO (branchBound b₁ b₂).timeRep
      (fun n => Nat.max (g₁ n) (g₂ n) + 1) := by
  have hmax : BigO (fun n => Nat.max (b₁.timeRep n) (b₂.timeRep n))
      (fun n => Nat.max (g₁ n) (g₂ n)) :=
    BigO.max_bound h₁ h₂
  have hone : BigO (fun _ : Nat => 1) (fun _ : Nat => 1) :=
    BigO.refl _
  have hadd := BigO.add hmax hone
  simpa [branchBound] using hadd

/-- WHAT `loopBoundStub` is: a placeholder bound so loop code has a
`ResourceBound` to carry while the real rule does not exist yet. WHY a
stub (not omitted): every `CStatement` MUST supply `stmtBound` — the
field forces loop authors to confront cost instead of silently having
none. The real rule needs a variant (termination measure) plus a
per-iteration envelope, i.e. iterated addition (`∑`-of-`=O`); until
then, an unbounded loop propagates `UNBOUND` time by convention (see
`UNBOUND` in `Complexity.lean`), and this `(0,0)` stub must NOT be used
for one — it would claim zero cost. -/
def loopBoundStub : ResourceBound :=
  { timeRep := fun _ => 0, memRep := fun _ => 0 }

/-- WHAT a sequential process is: the statement list executed in order
(`nil` = empty program, `cons` = head statement then the rest). WHY the
`CStatement` constraints on `cons`: each element (and, inductively, the
tail) must already be sound *and bounded* — so `cons` below can add
their bounds instead of trusting them. -/
inductive SequentialProcess : (List (Type u)) -> Type (u+1) where
| nil : SequentialProcess []
| cons {Γ : Type u} [CContext Γ] {τs : List (Type u)} (τ : Type u ) [CStatement Γ τ] (_: SequentialProcess τs) : SequentialProcess (τ :: τs)


-- WHAT the two `SequentialProcess` instances say: `nil` costs
-- `(Zero, Zero)` (empty computation — matches `ZeroTime/ZERO_MEM`
-- bottoms); `cons` costs `seqBound head tail` (time adds, memory peaks).
-- WHY the tail needs its own `CStatement` instance: the bound is
-- structural recursion — `stmtBound (τ :: τs)` is built from
-- `stmtBound τ` and `stmtBound τs`, so a 3-statement program folds to
-- `t₁ + (t₂ + (t₃ + 0))` with zero-cost `nil` as the identity.
instance {Γ : Type u} [CContext Γ] : CStatement Γ (SequentialProcess []) where
  isStatementSound := True
  stmtBound := { timeRep := fun _ => 0, memRep := fun _ => 0 }

instance {Γ : Type v} [CContext Γ] {τ : Type u} [CStatement Γ τ] {τs : List (Type u)} [CStatement Γ (SequentialProcess τs)] :
    CStatement Γ (SequentialProcess (τ :: τs)) where
  isStatementSound :=
    (CStatement.isStatementSound Γ (α := τ)) ∧
      (CStatement.isStatementSound Γ (α := SequentialProcess τs))
  stmtBound :=
    seqBound (CStatement.stmtBound (Γ := Γ) (α := τ))
      (CStatement.stmtBound (Γ := Γ) (α := SequentialProcess τs))

/-- WHAT binary branching is: `if α then TrueBranch else FalseBranch`
(`α` the guard proposition — `True` in the then-context, `¬True`… i.e.
`False`-of-`α` in the else-context per `Context.lean`). WHY only binary:
`if/else` covers every finite branch (`switch` desugars to nested
binary); n-ary would duplicate the `max` lemmas per arity for no gain.
The instance below costs it `branchBound` (worst side + guard). -/
inductive BinaryBranchingProcess (Γ : Type v) [CContext Γ] (α : Prop)
  ( TrueBranch : Type u) [CStatement Γ TrueBranch]
  (FalseBranch : Type u) [CStatement Γ FalseBranch] where
| mk : BinaryBranchingProcess Γ α TrueBranch FalseBranch

instance {α : Prop} {Γ : Type v} [CContext Γ]
  { TrueBranch : Type u } [CStatement Γ TrueBranch]
  {FalseBranch : Type u} [CStatement Γ FalseBranch] : CStatement Γ (BinaryBranchingProcess Γ α TrueBranch FalseBranch ) where
  isStatementSound :=
    (CStatement.isStatementSound Γ (α := TrueBranch)) ∧
      (CStatement.isStatementSound Γ (α := FalseBranch))
  stmtBound :=
    branchBound (CStatement.stmtBound (Γ := Γ) (α := TrueBranch))
      (CStatement.stmtBound (Γ := Γ) (α := FalseBranch))

end LeanC
