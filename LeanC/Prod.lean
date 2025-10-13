namespace LeanC

class CResolver (α : Type u ) ( β : Type v) (γ :Type w) where
  resolve : ( context : α ) -> (value: β) → γ

class CProduction (α : Type ) ( β : Type) (γ :Type)  where
  prod : ( x: β ) -> IO.FS.Stream -> CResolver α β γ -> (context :α ) -> EIO IO.Error Unit

end LeanC
