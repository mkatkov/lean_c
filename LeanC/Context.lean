


namespace LeanC
universe u v

/--
Context provides information about program components relationships.

For example, 'if' statement branches the program into 2 processes with
additional information in each branch in the form of proposition in the first branch
(if condition) and its negation for the second branch.

List of variable and list of available statements, together with proofs of their complexity
(performance).
-/

class CContext (Γ : Type v) where
  isCContext : Prop
  appendContext {Γ1: Type v} {Γ2 :Type v} : Γ1 -> Γ2


end LeanC
