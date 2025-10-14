namespace LeanC

/--
Resolver class for resolving values within a given context
it actually provides some flexibility in defining production

Conseptually resolver should know what it is resolving for in what context,
what value should be resolved into specific type. In a sense it is a contextual coercion or casting.
-/
class CValueResolver (who_t : Type u ) ( context_t : Type v) ( value_t :Type w) ( result_t : Type) where
  get_value : ( who : who_t ) → ( context : context_t ) → (value: value_t ) → result_t

class CTypeResolver (who_t : Type u ) ( context_t : Type v) ( result_t : Type) where
  get_type : ( who : who_t ) → ( context : context_t ) → result_t

/--
Production class for generating code
At some points we need to have context (e.g. for resolving identifiers)

The main question here is to whether we need a resolver type class for prod function?
The point is that we can define a resolver type for a (production type, context type, and a value type)
-/
class CProduction (α : Type ) ( β : Type) (resolver_t : Type) where
  /-- prod will produce output to the given stream for x in context -/
  prod : IO.FS.Stream -> ( x: α  ) ->  (context :β  ) → resolver_t -> EIO IO.Error Unit

end LeanC
