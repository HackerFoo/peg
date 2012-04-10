Peg
===

Peg is a lazy non-deterministic concatenative programming language.

In contrast to most concatenative programming languages, Peg starts evaluation from the right, evaluating arguments to the left as needed.

For example, even though the word 'no' can never be resolved:

    no 1 2 + --> no 3

This is because `+` requires only two arguments on the stack.

Branching is acomplished with the choice operator `\/`. Both paths are followed non-deterministically.  Paths are terminated when a word cannot be resolved.

A word can only be resolved if the word can operate on its arguments.  The builtin words `assert` and `deny` can only be resolved on the arguments `True` and `False` respectively.  `ifte` is provided as part of the library.

    [ 1 ] 2 \/ popr --> [ ] 1

The basic types are integers, floats, characters, words, and stacks.  As with the top level stack, these stacks are evaluated lazily.  Stacks are 'live', and will be evaluated as demanded, by such words as `popr`.  There is no way to directly extract an item from the left, and there is no way to extract an item without evaluation.

    [1 2 3 +] popr --> [1] 5

Peg is flat, in that any expression can be divided on whitespace (except inside a literal), the pieces evaluated independently, and when the results are concatenated, evaluate to an equivalent expression to the original expression.

Example:

    [ 1 2 + ] popr --> [ ] 3
    [ 1 2 +        --> [ 3
    ] popr         --> ] popr
    [ 3 ] popr     --> [ ] 3
