/-  This file acts as a small test runner that imports every Lean file in the
  `tests/` directory and executes their `main` entrypoints (which should
  return an `IO UInt32` exit code where `0` means success).
-/


import Tests.TestMain

open Lean IO

-- Helper: run a list of test mains sequentially and return the first non-zero exit code
def runAll (runners : List (IO UInt32)) : IO UInt32 := do
  let mut code := 0
  for runner in runners do
    if code == 0 then
      code := (← runner)
    else
      -- skip remaining if already non-zero (keeps first failure)
      pure ()
  pure code

def main : IO UInt32 := do
  -- Add each test module's `main` here in the same order as imports above.
  -- Expand this list when new tests are added under `tests/`.
  runAll [TestMain.test]
