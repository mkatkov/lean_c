namespace LeanC
universe u v

/-
WHAT a context is: the information carried along one program path —
which variables are in scope, which branch propositions hold (guard in
the then-branch, its negation in the else-branch), and the accumulated
worst resource bound so far. WHY bounds live here (not just on
statements): sequencing needs "what did the path cost up to here" to
add the next statement's cost; branching needs "what did each side
cost" to join them on exit. See `Processes.seqBound`/`branchBound`.
-/

/-- WHAT a worst bound is: two cost functions, one per axis —
`timeRep : Nat → Nat` maps input size to step count, `memRep` maps it
to peak live cells (`CTypeSize.size_of` units). WHY functions, not
numbers: bounds must grow with input (`log`, `n`, `n^k`); a bare number
could only say `O(1)`. WHY a pair, not two separate fields on the
context: time and memory compose differently (see `seqBound`), so they
must travel together through every combinator. -/
structure ResourceBound where
  timeRep : Nat → Nat
  memRep : Nat → Nat

class CContext (Γ : Type v) where
  isCContext : Prop
  /-- WHAT `extend` does: records an additional worst bound into the
  context (the path's accumulated `(timeRep, memRep)` grows). WHY it
  replaced the old `appendContext {Γ1 Γ2} : Γ1 → Γ2`: that signature
  could turn ANY type into ANY other type — unusable as typed, it never
  constrained anything. `extend` is explicit: same context type in,
  same type out, bound carried along. -/
  extend : Γ → ResourceBound → Γ


end LeanC
