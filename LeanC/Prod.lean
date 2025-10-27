namespace LeanC

/--
Production class for generating code
At some points we need to have context (e.g. for resolving identifiers)

The main question here is to whether we need a resolver type class for prod function?
The point is that we can define a resolver type for a (production type, context type, and a value type)
-/
class CProduction (α : Type ) ( β : Type) where
  /-- prod will produce output to the given stream for x in context -/
  prod : IO.FS.Stream → ( x: α  ) →  (context :β  ) →  EIO IO.Error Unit
  correctness_proof : ( x: α ) → (context :β ) → Prop

end LeanC
