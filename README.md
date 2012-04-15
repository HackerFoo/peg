Peg Programming Language
========================

Overview
--------

Peg is a lazy non-deterministic concatenative programming language inspired by Haskell, Joy, and Prolog.

In contrast to most concatenative programming languages, Peg starts evaluation from the right, evaluating arguments to the left as needed.

For example, even though the word `no` can never be resolved:

    no 1 2 + --> no 3

This is because `+` requires only two arguments on the stack.

Another demonstration of laziness:

    0 [1+] iterate

This creates the infinite stack `.. 3 2 1 0`.  Try `pop`ing a few times to see how it works.

Branching is accomplished with the choice operator `\/`. Both paths are followed non-deterministically.  Paths are terminated when a word cannot be resolved.

Multiple definitions for a word cause the definitions to be substituted non-deterministically.  This allows words (even built-in words) to be extended for new types.

For example, to extend `sqrt` to operate on the word `Four`:

    [Four eq? Two swap assert] "sqrt" :def

A word can only be resolved if the word can operate on its arguments.  The built-in words `assert` and `deny` can only be resolved on the arguments `True` and `False` respectively.  `ifte` is provided as part of the library.

    [ 1 ] 2 \/ popr --> [ ] 1

The basic types are integers, floats, characters, words, and stacks.  As with the top level stack, these stacks are evaluated lazily.  Stacks are 'live', and will be evaluated as demanded, by such words as `popr`.  There is no way to directly extract an item from the left, and there is no way to extract an item without evaluation.

    [1 2 3 +] popr --> [1] 5

A string literal is a stack consisting only of characters.  They are read and displayed backwards from stacks, to make them readable.

    ['o' 'l' 'l' 'e' 'h'] --> "hello"
    "hello" 0 pushr --> ['o' 'l' 'l' 'e' 'h' 0]

Peg is flat, in that any expression can be divided on white space (except inside a literal), the pieces evaluated independently, and when the results are concatenated, evaluate to an equivalent expression to the original expression.

Example:

    [ 1 2 + ] popr --> [ ] 3
    [ 1 2 +        --> [ 3
    ] popr         --> ] popr
    [ 3 ] popr     --> [ ] 3

Instead of using a monad to implement pure functional I/O, Peg simply uses a token representing the state of the world, `IO`.  Words that perform I/O must require `IO` as an argument.  If the word does not put it back, it will destroy the world.

`IO` can only be introduced from the top-level, by typing `IO`.  In other places, such as definitions and `read`, `IO` is parsed as a word with no special meaning.

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

`x` `y` `\/` --> `x` \/ `y` -- continues execution non-deterministically with `x` and `y`

`int?`, `float?`, `word?`, `list?`, `char?`, `string?` -- test type of argument, returning `True` or `False`
 
`x` `y` `eq?` --> `True` \/ `False` -- is `x` a primitive type (non-stack) equal to `y` (also not a stack)

`[ .. ]` `"word-name"` `:def` --> -- define the word `word-name` to be equivalent to the stack argument

`"word-name"` `:undef` --> -- undefine the word `word-name`

`[ .. ]` `$` --> ` .. ` -- append stack argument to upper level stack and execute

`x` `y` `seq` --> `x` `y` -- force evaluation of `x`

`x` `show` --> `"x"` -- convert `x` into string representation

`"x"` `read` --> `x` -- convert string representation of `x` into `x`, opposite of `show`

`+`, `-`, `*`, `div`, `^`, `^^`, `**`, `exp`, `sqrt`, `log`, `logBase`, `sin`, `tan`, `cos`, `asin`, `atan`, `acos`, `sinh`, `tanh`, `cosh`, `asinh`, `atanh`, `acosh`, `<`, `<=`, `>`, `>=`, `realToFrac`, `round`, `floor`, `ceiling` -- numeric and comparison words defined as in Haskell Prelude

`getChar`, `putChar`, `getLine`, `putStr`, `putStrLn` -- similar to Haskell Prelude.  Instead of running in IO monad, they require `IO` as the first argument, putting it back after executing

A simple IO example:

    IO "What's your name?" putStrLn getLine "!" "Hello " splice putStrLn

Peg supports a curly bracket notation to allow for case statements and do-notation.  Curly braces trivially reduce to a nested stack.

`{` --> `[` `[`

`;` --> `]` `[`

`}` --> `]` `]`

Usage with `case`:

    b {1 a; 2 b} case --> 2

Library: lib.peg
----------------

Most words are based on the Haskell Prelude, some stack combinators are taken from Joy.

`foldr` and `foldl` are swapped from the Haskell definitions, because "lists" are stacks, and elements are added to the right side of a stack.  Similarly for `scanr` and `scanl`.

Most of the Haskell Prelude is implemented, except words that aren't very useful or are replaced by a built-in word.

Running the Peg Interpreter
---------------------------

Build the interpreter using Cabal (cabal configure; cabal build)

Just call the `peg` executable with source files to be loaded (such as lib.peg) as arguments.

The interpreter evaluates the input after pressing `Enter`.  The results will be printed after the next prompt, allowing you to edit the results.  If the cursor is not on the right, a word did not have enough arguments to be evaluated; the cursor will be placed so that you can provide the missing arguments.  If there are multiple results, up to 8 results will be printed, but only the first will appear at the prompt.  If there are no results, the result `no` is shown, which is equivalent (defined in `lib.peg`).

[Haskeline](http://hackage.haskell.org/package/haskeline) provides the line editing interface.  Clearing the input and pressing `Enter` will exit the interpreter.

Future
------

### I/O

I have tried modeling I/O after Haskell's monad approach, but monads seem to be better suited to applicative languages, despite being possible in a concatenative language.

I have implemented a different method of performing I/O in a pure functional way, as described above (I/O words require an `IO` token.)  This may allow more flexible use of I/O than a monad; threads could be spawned with `IO dup` and ended with `IO pop`.  Also, more complex operations may be possible, requiring multiple `IO`s.

### Type System

The current idea is to use explicit type checks (such as `int?`) instead of introducing a different syntax for type annotations.  This would allow the type system to be extended using the base language, and support dependent typing.  It would also allow optional run time typing.

The interpreter is currently dynamically typed, but I would like to make the compiler support static type checking, by proving that the result of a computation cannot be `no`.  The compiler could also optimize away types and most non-determinism.  I do realize that, in general, static type checking will be undecidable.  The compiler will be designed to resolve undecidable types interactively with the user.

The language would not change significantly.  Product types are built from stacks, such as `[1 2 Ratio]`, and sum types are created using `\/`, such as `[1 Left] \/ ['a' Right]`, using undefined words at the top of the stack as tags.  Constructors can be created as the matching lowercase word, such as `x left --> [x Left]`.  Using the same word as the tag results in an infinitely nested stack, so the values can never be retrieved.

I have done some work on types in `types.peg`, which extends some words to operate on type tags.

### Modules

I will need to add a module system to allow encapsulation.

### Compiler

The compiler will first target C, to allow easy portability.  I am interested in running Peg code in embedded systems, especially because it is difficult to use other high-level languages such as Haskell on most microcontrollers.
