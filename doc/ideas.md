

## Proofs

In the spirit of Lean it is reasonable to have the compiled program to produce correct C program. It is easy to require that Abstract Syntax Tree (AST) is correct by imposing correct types. However, there are some subtleties that does not have to be encoded purely in types. For example, the identifier within the scope, say, the name of the field in the structure. Such name can potentially be arbitrary encoded (say, meaningfull name), and we need to ensure that the names within structure is unique. Conceptually we need a proof that the name is unique. Additionally, when referring to the identifier within the structure we also have to be sure it exists withihn structure scope.

Currently, the idea is to have all C language concepts as Lean types, and restrictions applied by the classes. In this way the Lean compiler complains that the type cannot be synthesized. We need to check whether this can be implemented cleaner.


## Scopes and types

we have a block of file scope similar to C structure or union, where we can have ordered list of types. The order of appearance is the identity of identifier. In terms of production one may have a simple rule like "id_#" or formally just a list of identifier strings. The two approaches require a specific types that can potentially store different information. So we should have a (CScope Flavor) as a class with Flavor type being responsible for actual production and storage of additinal information.