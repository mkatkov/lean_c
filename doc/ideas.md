

## Proofs

In the spirit of Lean it is reasonable to have the compiled program to produce correct C program. It is easy to require that Abstract Syntax Tree (AST) is correct by imposing correct types. However, there are some subtleties that does not have to be encoded purely in types. For example, the identifier within the scope, say, the name of the field in the structure. Such name can potentially be arbitrary encoded (say, meaningfull name), and we need to ensure that the names within structure is unique. Conceptually we need a proof that the name is unique. Additionally, when referring to the identifier within the structure we also have to be sure it exists withihn structure scope.

Currently, the idea is to have all C language concepts as Lean types, and restrictions applied by the classes. In this way the Lean compiler complains that the type cannot be synthesized. We need to check whether this can be implemented cleaner.

## Program is a proof of specs

The program IS the proof of specs, and the proof is what gets emitted (say,
in C). A `CProgram` value alone is data; `ProgramMeetsSpec p` is the
certificate (main resolves, every body fits its declaration, every call site
resolves). From P1 on, call sites are computed by traversal of stored bodies
(`CFunc.calls`/`nested` set by `mkFuncWithBody`, program lists are `flatMap`
defs) — there is no producer-supplied list to omit from, so an unresolved
call has no certificate by construction. The emitter takes the proof
(`emitProgram (p) (_ : ProgramMeetsSpec p)`), so uncertified programs are
unemittable by type. See `doc/next_task.md` §0 (binding where older text
conflicts).


## Scopes and types

we have a block of file scope similar to C structure or union, where we can have ordered list of types. The order of appearance is the identity of identifier. In terms of production one may have a simple rule like "id_#" or formally just a list of identifier strings. The two approaches require a specific types that can potentially store different information. So we should have a (CScope Flavor) as a class with Flavor type being responsible for actual production and storage of additinal information.