# Mink

Mink is an esoteric programming language built around the concept of an infinite global stack. It is based on a similar language called Clink, but introduces the concept of loops and macros.

## Dependencies

Mink is built using `sbt`. To build Mink, run:

`sbt assembly`

and find the .jar file in `./target/scala-2.13`.

Mink also requires `clang` to be installed.

## Usage

To compile `input.mink` file, run:

```bash
mink input.mink
```

Mink searches for local packages from the working directory.

To compile to a specific location, run:

```bash
mink input.mink -o output
```

You can run `output` as a regular executable.

## Tutorial

### Pushing

Mink uses an infinite global stack of `!`s and `?`s. Initally, it is set to an infinite number of `?`s.

You can push directly to the stack using `!` and `?`:

```mink
! ?
```

Mink reads operators from left to right, so `!` is pushed before `?`.

There are a few aliases for a large string of `!`s and `?`s:

- `%n` will push `?`, followed by n `!`s, to the stack
- `"Hello world!"` will push `?`, followed by the ASCII representations of each character in the string in reverse, to the stack

Unsigned integers are typically represented by a string of `!`s followed by a terminating `?`. The compiler is optimised to represent integers this way.

### Printing

`#` is used to print the null-terminated string at the top of the stack.

```mink
"Hello world!\n" #
```

$\Rightarrow$

```txt
Hello world!
```

You could also write the ASCII characters directly:

```mink
%0 %10 %105 %72 #
```

$\Rightarrow$

```txt
Hi
```

### Popping/Matching

`:` is used to pop off of the stack, and branch depending on whether the popped item was `!` or `?`.

```mink
! ("Left!\n" # : "Right!\n" #)
```

$\Rightarrow$

```txt
Left!
```

```mink
? ("Left!\n" # : "Right!\n" #)
```

$\Rightarrow$

```txt
Right!
```

`:` has a higher precedence than unary operators, so `()` can be used to enclose a match statement.

### Functions

Functions are defined with `=`, and their names can contain any unused non-whitespace character:

```mink
+ = + ! :
```

This is the "add" function:

- First, it pops off the stack.
- If it is `?`, then it does nothing and returns
- If it is `!`, then it calls itself, and then pushes `!` to the stack.

This is effectively removing the first `?` on the stack, which is equivalent to adding the top two "integers" on the stack.

Another similar function is the "pop" integer function:

```mink
\ = \ :
```

which removes all units on the stack until it has popped a `?`, which is equivalent to popping the top "integer" off the stack.

### Loops

The add function can also be written as:

```mink
{, ! :}
```

A loop is defined with `{}`. It can be thought of as similar to an anonymous function. Whenever `,` is seen, it calls the surrounding loop like a function, before continuing when the function returns.

Similarly, the pop function can also be written as:

```mink
{, :}
```

Mink tries to represent all recursive functions as loops, as it makes it easier to optimise. Optimisation is discussed later.

If a function can be represented as a loop, it can be inlined using `$`.

```mink
%2 %3 +
+ $ = + ! :
```

compiles to the equivalent of

```mink
?!! ?!!! {; ! :}
```

### Macros

Let's say you wanted to add `!` to the second item on the stack. You would write:

```mink
& = & ! : ! ?
```

This pops all `!`s until it sees a `?`, pops it, pushes `!`, and readds the `?`, before pushing back all of the popped `!`s. This effectively "skips" the first integer on the stack.

Let's say you want to now pop the second item off the stack. You would write:

```mink
& = & ! : \ ?
```

These two functions only differ by the operators before the `?` on the right hand side. You could represent both by using a macro:

```mink
& $ a = & a ! : a ?
```

and then calling one of `& !` or `& \`, which replaces each instance of `a` with `!` or `\`.

Macros must always be inlined, hence the use of `$`.

### Packages

To define a package, write

```mink
= my.package
```

at the top of the file, replacing `my.package` with your package name.

Packages can be imported with

```mink
$ my.package
```

at the top of the file.

To export a function from a package, write `?=` instead of `=`:

```mink
= std.op

+ ?= + ! :
```

Packages can be spread across multiple files.

### Optimisation

#### Stack Representation

One way of storing items on the stack is as a list of booleans. However, this would be very space inefficient, and larger integer representation would take up an unreasonable amount of space.

Another way to represent the stack is to use run-length encoding - each stack frame stores the number of `!`s preceding the next `?` as an integer. This is optimised for Mink's typical representation of integers on the stack.

#### Recursion

Since functions and loops are inherently recursive, some optimisations can be made to reduce the number of stack frames:

Avoiding any operations after a recursive call in a function or loop can enable tail call optimisation, and the recursive call can be treated as a `goto`:

```mink
{, :}
```

There are some cases where `!`s and `?`s need to appear after a recursive call - there is still some optimisation that can occur in this case, through the use of a temporary stack to represent deferred pushes:

```mink
& $ a = {, ! : a ?}
```

=>

```mink
& $ a = {defer(!) , : a ?}
```

Each deferred push is performed when the loop exits.

#### Common Operations

This is not yet implemented, but functions such as `&`, `+` and `\` could operate on the stack frames themselves, rather than having to operate on each unit on the stack. This would help speed up programs, as these are commonly used operations.

## Known Issues

- Loops that cannot be optimised are not currently representable in the compuled code - there are surprisingly few cases where a loop cannot be optimised. Immediate plans to patch this.
- A run-length encoding representation of the stack puts a limit on the size of each run of `!`s. There are long term plans to patch this.
