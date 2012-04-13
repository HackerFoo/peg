Peg Programming Language
========================

Overview
--------

Peg is a lazy non-deterministic concatenative programming language inspired by Haskell, Joy, and Prolog.

In contrast to most concatenative programming languages, Peg starts evaluation from the right, evaluating arguments to the left as needed.

For example, even though the word `no` can never be resolved:

    no 1 2 + --> no 3

This is because `+` requires only two arguments on the stack.

Branching is accomplished with the choice operator `\/`. Both paths are followed non-deterministically.  Paths are terminated when a word cannot be resolved.

Multiple definitions for a word cause the definitions to be substituted non-deterministically.  This allows words (even built-in words) to be extended for new types.

For example, to extend `sqrt` to operate on the word `Four`:

    [Four eq? Two swap assert] "sqrt" :def

A word can only be resolved if the word can operate on its arguments.  The built-in words `assert` and `deny` can only be resolved on the arguments `True` and `False` respectively.  `ifte` is provided as part of the library.

    [ 1 ] 2 \/ popr --> [ ] 1

The basic types are integers, floats, characters, words, and stacks.  As with the top level stack, these stacks are evaluated lazily.  Stacks are 'live', and will be evaluated as demanded, by such words as `popr`.  There is no way to directly extract an item from the left, and there is no way to extract an item without evaluation.

    [1 2 3 +] popr --> [1] 5

Peg is flat, in that any expression can be divided on white space (except inside a literal), the pieces evaluated independently, and when the results are concatenated, evaluate to an equivalent expression to the original expression.

Example:

    [ 1 2 + ] popr --> [ ] 3
    [ 1 2 +        --> [ 3
    ] popr         --> ] popr
    [ 3 ] popr     --> [ ] 3

Built-in Words
--------------

The format below is:

`input stack` `word` --> `output stack` \/ `alternate output stack` -- notes

--------------------------------

`x` `pop` --> 

`x` `y` `swap` --> `y` `x`

`x` `dup` --> `x` `x`

`[` `xn` .. `x0` `]` --> `[xn .. x0]` -- gathers stack items into a list until `[` if possible

`[ .. ]` `x` `pushr` --> `[ .. x]`

`[ .. x]` `popr` --> `[ .. ]` `x` -- forces evaluation of `x`

`[ .. x]` `[y ..]` `.` --> `[ .. x y .. ]` -- concatenates stacks without evaluating anything

`[ .. ]` `dupnull?` --> `[ .. ]` (`True` \/ `False`) -- indicates if the stack is empty, works on partial stacks

`True` `assert` --> -- only resolves if the argument is `True`

`False` `deny` --> -- opposite of assert

`x` `y` `\/` --> 'x' \/ 'y' -- continues execution non-deterministically with `x` and `y`

`int?`, `float?`, `word?`, `list?`, `char?`, `string?` -- test type of argument, returning `True` or `False`
 
`x` `y` `eq?` --> `True` \/ `False` -- is `x` a primitive type (non-stack) equal to `y` (also not a stack)

`[ .. ]` `"word-name"` `:def` --> -- define the word `word-name` to be equivalent to the stack argument

`"word-name"` `:undef` --> -- undefine the word `word-name`

`[ .. ]` `$` --> ` .. ` -- append stack argument to upper level stack and execute

`x` `y` `seq` --> `x` `y` -- force evaluation of `x`

`x` `show` --> `"x"` -- convert `x` into string representation

`"x"` `read` --> `x` -- convert string representation of `x` into `x`, opposite of `show`

`+`, `-`, `*`, `div`, `^`, `^^`, `**`, `exp`, `sqrt`, `log`, `logBase`, `sin`, `tan`, `cos`, `asin`, `atan`, `acos`, `sinh`, `tanh`, `cosh`, `asinh`, `atanh`, `acosh`, `<`, `<=`, `>`, `>=`, `realToFrac`, `round`, `floor`, `ceiling` -- numeric and comparison words defined as in Haskell Prelude

Library: lib.peg
----------------

Most words are based on the Haskell Prelude, some stack combinators are taken from Joy.

`foldr` and `foldl` are swapped from the Haskell definitions, because "lists" are stacks, and elements are added to the right side of a stack.  Similarly for `scanr` and `scanl`.

Most of the Haskell Prelude is implemented, except words that aren't very useful or are replaced by a built-in word.  I'm still working on IO.

Running the Peg Interpreter
---------------------------

Build the interpreter using Cabal (cabal configure; cabal build)

Just call the `peg` executable with source files to be loaded (such as lib.peg) as arguments.

The interpreter evaluates the input after pressing `Enter`.  The results will be printed after the next prompt, allowing you to edit the results.  If the cursor is not on the right, a word did not have enough arguments to be evaluated; the cursor will be placed so that you can provide the missing arguments.  If there are multiple results, up to 8 results will be printed, but only the first will appear at the prompt.  If there are no results, the result `no` is shown, which is equivalent (defined in `lib.peg`).

[Haskeline](http://hackage.haskell.org/package/haskeline) provides the line editing interface.  Clearing the input and pressing `Enter` will exit the interpreter.
