# Mink

<img style="float: left; width: 96px; margin-right: 20px;" src="mink-logo.png" />

Mink is an esoteric programming language built around the concept of an infinite global stack. It aims to be as minimalistic, but extensible, as possible.

It is based on a similar language called Clink, but introduces the concept of loops and macros.

## Dependencies

Mink requires these packages:

```txt
sbt clang java
```

`clang` and `java` should be able to be found in your package manager. Instructions on how to install `sbt` can be found [here](https://www.scala-sbt.org/1.x/docs/Setup.html).

## Installation

To install Mink globally, run

```bash
./install.sh
```

To install locally, run

```bash
./install.sh -l
```

## Usage

To compile a file `input.mink`, run:

```bash
mink input.mink
```

To compile to a specific location, run:

```bash
mink input.mink -o output
```

You can run `output` as a regular executable.

## Tutorial

### Pushing

Mink uses an infinite global stack of `!`s and `?`s.

Initally, it is set to an infinite number of `?`s:

```txt
????????????????????...
```

You can push directly to the stack using `!` and `?`:

```mink
! ? ! !
```

Mink reads operators from left to right, so it pushes `!`, then `?`, then `!`, then `!`.

The stack now looks like this:

```txt
!!?!????????????????...
```

There are a few aliases for a large string of `!`s and `?`s:

#### Integers

`%n` will push `?`, followed by n `!`s, to the stack. Performing:

```mink
%3 %8 %4
```

will make the stack look like this:

```txt
!!!!?!!!!!!!!?!!!???...
```

Unsigned integers are typically represented this way (pushing a terminating `?`, followed by a string of `!`s). The compiler is optimised to represent integers this way.

#### Strings

`"Hello world!"` will push `%0` (null terminator), followed by the ASCII representations of each character in the string in reverse, to the stack.

Performing

```mink
"Hi!"
```

is equivalent to

```mink
%0 %33 %105 %72
```

### Printing

`#` is used to print the null-terminated string at the top of the stack.

Performing

```mink
"Hello world!\n" #
```

will print

```txt
Hello world!
```

You could also write the ASCII characters directly. Perfoming

```mink
%0 %33 %105 %72 #
```

will print

```txt
Hi!
```

### Popping/Matching

`:` is used to pop off of the stack, and branch depending on whether the popped item was `!` or `?`.

Performing

```mink
! ("Left!\n" # : "Right!\n" #)
```

will print

```txt
Left!
```

and performing

```mink
? ("Left!\n" # : "Right!\n" #)
```

will print

```txt
Right!
```

`:` has a lower precedence than unary operators, so `()` can be used to enclose a match statement.

### Functions

Functions are defined with `=`, and their names can contain any unused non-whitespace character:

```mink
+ = + ! :
```

This is the "add" function:

- First, it pops off the stack.
- If it is `?`, then it does nothing and returns.
- If it is `!`, then it calls itself, and then pushes `!` to the stack.

This is effectively removing the first `?` on the stack, which is equivalent to adding the top two integers on the stack.

Another similar function is the "pop" integer function:

```mink
\ = \ :
```

which removes all units on the stack until it has popped a `?`, which is equivalent to popping the top integer off the stack.

### Loops

The add function can also be written as:

```mink
{, ! :}
```

A loop is defined with `{}`. Loops in Mink can be thought of as similar to an anonymous function.

Whenever `,` is seen, it calls the surrounding loop like a function, before continuing when the function returns.

Similarly, the pop function can also be written as:

```mink
{, :}
```

(Mink tries to represent all recursive functions as loops, as it makes it easier to optimise. Optimisation is discussed later.)

#### Inlining

Functions can be inlined using `$`. Writing

```mink
%2 %3 +
+ $ = + ! :
```

is equivalent to

```mink
?!! ?!!! {, ! :}
```

### Macros

Let's say you wanted to add `!` to the second integer on the stack. You would write:

```mink
& = & ! : ! ?
```

This pops all `!`s until it sees a `?`, pops it, pushes `!`, and readds the `?`, before pushing back all of the popped `!`s. This has the effect of skipping the first integer on the stack.

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

### Standard Library

`std` and its subpackages do not need to be manually added to the working directory to be imported.

Here are the functions included in the standard library, and their corresponding package:

| Package    | Name    | Description                                                                     | Definition                                                                |
| ---------- | ------- | ------------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| `std`      | `& a`   | Skip the first integer, and perform `a`                                         | `& $ a ?= & a ! : a ?`                                                    |
| `std`      | `\`     | Pop the first integer                                                           | `\ $ ?= \ :`                                                              |
| `std`      | `+`     | Add the top two integers                                                        | `+ $ ?= + ! :`                                                            |
| `std`      | `*`     | Clone the top integer                                                           | `* $ ?= & (? ?) {& (& ! !) , :}`                                          |
| `std`      | `-`     | Subtract the top two integers                                                   | `- $ ?= & (: ?) - :`                                                      |
| `std`      | `/`     | Swap the top two integers                                                       | `/ $ ?= & & ? {& & ! , :}`                                                |
| `std.math` | `**`    | Multiply the top two integers                                                   | `** ?= & & ? {& (* & +) , : \}`                                           |
| `std.math` | `//`    | Divide and modulo the top two integers                                          | `// ?= & & ? {& * * & (& ! - (!?! & (: ?) : ?)) / ((:) & & (\ !) , : \)}` |
| `std.fmt`  | `\|`    | Convert an integer to its string representation of base at the top of the stack | `\| ?= & & ? {* & (// %48 + /) / (! / , : \)}`                            |
| `std.fmt`  | `` ` `` | Print each non-zero integer on the stack, for debugging                         | `` ` ?= ! * %10\|#"\n"# & ` : ? ``                                        |

### Optimisation

#### Stack Representation

One way of storing items on the stack is as a list of booleans. However, this would be very space inefficient, and larger integer representation would take up an unreasonable amount of space.

Another way to represent the stack is to use run-length encoding - each stack frame stores the number of `!`s preceding the next `?` as an integer. This is optimised for Mink's typical representation of unsigned integers on the stack.

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

Tail call optimisation is enabled with `-O 1` and above. It is enabled by default.

#### Common Operations

Common functions such as `&`, `+` and `\` can be optimised to operate on the stack frames themselves in just a few instructions, rather than having to repeatedly deconstruct and reconstruct the stack.

This can help speed up programs significantly, particularly for larger integer representations.

Furthermore, some complex functions such as `**` (multiplication) and `//` (division/modulo) can also be optimised to just a few CPU instructions.

Here is a table of which standard library functions each optimisation flag optimises:

|             | `-O 2` | `-O 3` | `-O 4` |
| ----------- | ------ | ------ | ------ |
| `&`,`+`,`\` | ✅     | ✅     | ✅     |
| `-`,`*`,`/` | ❌     | ✅     | ✅     |
| `**`,`//`   | ❌     | ❌     | ✅     |

All of these optimisations are enabled by default.

## Known Issues

- Multi-file packages bundle all of their imports together, which is possibly a mistake. Plans to fix this.
- A run-length encoding representation of the stack puts a limit on the size of each run of `!`s. Long term plans to patch this.
