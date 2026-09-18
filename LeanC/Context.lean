import LeanC.TypeClasses
import LeanC.Complexity

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

/-! ## Context pool (T3) — literal statics carried by the context.

Invariant: `acc` = `seqBound`-fold of prior bounds (time adds, memory
maxes); `pool` = live global-statics (each a `CGlobalStaticMemoryBlock`
lowering — the "extra slot in references of memory objects" per D2).
Branching joins via `branchBound` (T4/T5). `extendWithLit` conses `pool`
and leaves `acc`/`scope` untouched — execution cost of the literal itself
is already in `litBound`; the pool is codegen accounting (what `.data`
to emit), queried via `poolBound`.
-/

/-- WHAT a pool entry is: one live global-static object — its C type plus
`cells` (static footprint in `size_of` units). -/
structure LiteralPoolEntry where
  (ty : Type)
  [h : IsCType ty]
  (cells : Nat)

/-- WHAT `poolBound` costs: time `0` (statics are loaded with the program,
no per-run step), memory `sum cells` (all statics live simultaneously).
Still `=O 1` for fixed pools via `const_le_one`. -/
def poolBound (pool : List LiteralPoolEntry) : ResourceBound :=
  { timeRep := gZero,
    memRep := fun _ => pool.foldl (fun acc e => acc + e.cells) 0 }

/-- WHAT `DraftCtx` is: minimal context implementation — `scope` (types in
scope, producer-maintained uniqueness), `pool` (live statics), `acc`
(accumulated worst bound). -/
structure DraftCtx where
  (scope : List Type)
  (pool : List LiteralPoolEntry)
  (acc : ResourceBound)

/-- Empty draft context: no scope, no pool, zero cost. -/
def emptyDraft : DraftCtx :=
  { scope := [], pool := [], acc := { timeRep := gZero, memRep := gZero } }

/-- `CContext DraftCtx`: `extend` folds `acc` via `seqBound`'s rule (time
adds, memory maxes). NOTE: `seqBound` itself lives in `Processes` (which
imports `Context`), so this instance inlines its two lines instead of
importing `Processes` — otherwise `Context ↔ Processes` would cycle.
`Program → … → Expr → Processes/Context` stays acyclic. -/
instance : CContext DraftCtx where
  isCContext := True
  extend ctx b :=
    { scope := ctx.scope,
      pool := ctx.pool,
      acc :=
        { timeRep := fun n => ctx.acc.timeRep n + b.timeRep n,
          memRep := fun n => Nat.max (ctx.acc.memRep n) (b.memRep n) } }

/-- WHAT `extendWithLit` does: records one more live static (cons `pool`).
Leaves `scope`/`acc` untouched (see invariant above). -/
def extendWithLit (ctx : DraftCtx) (e : LiteralPoolEntry) : DraftCtx :=
  { scope := ctx.scope, pool := e :: ctx.pool, acc := ctx.acc }

end LeanC
