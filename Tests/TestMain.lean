open Lean IO

namespace TestMain

def assertEq (expected actual : String) : IO Bool :=
  if expected == actual then
    IO.println s!"OK: {actual}" *> pure true
  else
    IO.eprintln s!"FAIL: expected {expected} but got {actual}" *> pure false



def test : IO UInt32 := do
  let ok ← assertEq "world" "world"
  if ok then
    IO.println "All tests passed." *> pure 0
  else
    pure 1

end TestMain
