import LeanC.TypeClasses

namespace LeanC
universe u v

/-
WHAT a context is: the information carried along one program path —
which variables are in scope, which branch propositions hold, and which
resources have been assigned. WHY resources live here: sequencing and
branching must combine "what did each side cost" on exit. Keys are
types distinguished by `CResourceType` — no strings. See `RStore`.
-/

/-- Combining operations per resource type. Each resource says how it
composes; sequencing/branching/looping combine existing resources via
these instances (`seqCombine`/`branchCombine`). Just an instance per
resource type — no edits here for new resources. -/
class CResource (R : Type 0) [CResourceType R] where
  zero : R
  seqCombine : R → R → R
  branchCombine : R → R → R

/-- WHAT `HasCost` is: THE bridge hook from the extended resource model
to `Complexity.BigO`. A resource value (e.g. `TimeCost ⟨fun n => …⟩`)
extracts to a cost function `Nat → Nat` (input size → steps / peak live
cells). WHY in core (`Context`, not `Examples`): every resource defines
its own extraction once; the generic membership predicate
(`costInClass` in `Complexity/Bridge.lean`) and the per-resource
`BigO`-preservation lemmas (in the resource's own file) then speak one
language. `HasCost` itself has no `BigO` dependency (keeps
`Context` import-free of `Complexity`); the `BigO` statements live in
`Bridge` + the resource files. -/
class HasCost (R : Type 0) where
  cost : R → (Nat → Nat)

/-- Membership witness in `Type` (not `Prop`): `head` selects the newest
entry (shadowing), `tail` selects an older one. Lives in `Type` (not
`Prop` like `List.Mem`) precisely so `RStore.get` can eliminate it to
return data (`R : Type 0`) — `Prop` elimination into `Type` is
disallowed, and `Option` would force a `none` case that kills proofs.
The witness itself selects old vs new — no type disequality needed. -/
inductive ResMem : List (Type 0) → Type 0 → Type 1 where
| head {R : Type 0} {Rs : List (Type 0)} : ResMem (R :: Rs) R
| tail {R S : Type 0} {Rs : List (Type 0)} : ResMem Rs R → ResMem (S :: Rs) R

/-- Heterogeneous resource store indexed by the list of present resource
types. The type itself is the key. Lookup takes a `ResMem` witness
(never runtime type comparison). No strings, no `Option`, no axioms.
Discipline (only `CResourceType` types) is enforced by
`CContext.setResource`, which requires the marker — the store itself is
unconstrained to keep matching proof-free. -/
inductive RStore : List (Type 0) → Type 1 where
| nil : RStore []
| cons {R : Type 0} {Rs : List (Type 0)} (v : R)
    (s : RStore Rs) : RStore (R :: Rs)

/-- WHAT `get` does: return the stored value of type `R`, guided by the
witness `h : ResMem Rs R`. `head` returns the newest shadowing entry;
`tail` recurses. Returns the correct type `R` directly — no `Option`,
so callers with `h` prove properties about the value itself. -/
def RStore.get {Rs : List (Type 0)} (s : RStore Rs) {R : Type 0}
    (h : ResMem Rs R) : R :=
  match s, h with
  | .cons v _, .head => v
  | .cons _ tl, .tail h' => get tl h'

/-- WHAT a context provides: existence witness + typed retrieval +
assignment. `resourceExists R ctx` is `ResMem ctx.rs R` for `DraftCtx`
(inhabited = present, in `Type` so it can drive `get`); `getResource`
needs its witness `h` and returns the correct type `R` (no `Option`);
`setResource` conses (old entries shadowed, still reachable via `tail`
witnesses). -/
class CContext (Γ : Type v) where
  isCContext : Prop
  resourceExists : {R : Type 0} → [CResourceType R] → Γ → Type 1
  getResource : {R : Type 0} → [CResourceType R] → (ctx : Γ) →
    (h : resourceExists (R := R) ctx) → R
  setResource : {R : Type 0} → [CResourceType R] → Γ → R → Γ

/-- WHAT a pool entry is: one live global-static object — its C type plus
`cells` (static footprint in `size_of` units). Pool is data, not a
bound; costs (if any) are assigned per resource type via `setResource`. -/
structure LiteralPoolEntry where
  (ty : Type)
  [h : IsCType ty]
  (cells : Nat)

/-- WHAT `DraftCtx` is: minimal context — `scope` (types in scope,
producer-maintained uniqueness), `pool` (live statics), `rs`/`store`
(generic type-keyed resources, empty by default; new resources cons via
`setResource` without touching this file). No hardwired axes. -/
structure DraftCtx where
  (scope : List Type)
  (pool : List LiteralPoolEntry)
  (rs : List (Type 0))
  (store : RStore rs)

/-- Empty draft context: no scope, no pool, no resources. -/
def emptyDraft : DraftCtx :=
  { scope := [], pool := [], rs := [], store := .nil }

/-- `CContext DraftCtx`: existence is `ResMem` witness; retrieval
delegates to `RStore.get`; assignment conses. -/
instance : CContext DraftCtx where
  isCContext := True
  resourceExists := (fun {R} [_] ctx => ResMem ctx.rs R)
  getResource := (fun {_R} [_] ctx h => ctx.store.get h)
  setResource := (fun {R} [_] ctx v =>
    { scope := ctx.scope, pool := ctx.pool, rs := R :: ctx.rs,
      store := .cons v ctx.store })

/-- WHAT `extendWithLit` does: records one more live static (cons `pool`).
Leaves `scope`/`rs`/`store` untouched. -/
def extendWithLit (ctx : DraftCtx) (e : LiteralPoolEntry) : DraftCtx :=
  { scope := ctx.scope, pool := e :: ctx.pool, rs := ctx.rs, store := ctx.store }

/-- Set-then-get with the head witness returns what was set (by `rfl`). -/
theorem draft_get_set_same (ctx : DraftCtx) (R : Type 0) [CResourceType R]
    (v : R) :
    CContext.getResource (CContext.setResource ctx v) (ResMem.head) = v := rfl

/-- Setting `R1` preserves lookup of `R2` via an old `tail` witness (by
`rfl`). No type disequality needed — the witness selects old vs new. -/
theorem draft_get_set_other {R1 R2 : Type 0} [CResourceType R1]
    [CResourceType R2] (ctx : DraftCtx) (v : R1) (h : ResMem ctx.rs R2) :
    CContext.getResource (CContext.setResource ctx v) (ResMem.tail h) =
      CContext.getResource ctx h := rfl

/-- Freshly set resources exist (head witness, as data). -/
def draft_set_exists (ctx : DraftCtx) (R : Type 0) [CResourceType R]
    (v : R) :
    CContext.resourceExists (R := R) (CContext.setResource (R := R) ctx v) :=
  ResMem.head

end LeanC
