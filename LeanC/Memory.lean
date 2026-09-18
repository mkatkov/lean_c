import LeanC.Arrays

/-!
# Memory — re-export of array/global-static blocks (T5 stub)

No new model: the draft reuses `Arrays.lean` (`CGlobalStaticMemoryBlock` /
`CArrayType` + `isAllocated`/`within_bounds`). Full block memory model
(allocation, load/store, aliasing) is future work (`doc/roadmap.md` §8).
-/

namespace LeanC
end LeanC
