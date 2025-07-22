# Etcetera
A programming language written in Rust using LLVM.

## System Requirements
This project depends on the following external tools:
- [`llc`](https://llvm.org/docs/CommandGuide/llc.html) — part of the LLVM toolchain
- [`clang`](https://clang.llvm.org/) — a C language family frontend for LLVM

## Run
#### Build yourself and run
```bash
cargo run -- example.etc --run
```

#### Or get it from Cargo
```bash
cargo install etcetera-compiler
```
Then use using the `etc` command.

## Features
- **Ahead-of-Time (AOT) Compilation:** Code is fully compiled and optimized before execution, not interpreted on the fly. The compiler toolchain (`llc`, `clang`) assembles a standalone binary.
- **Variable Declaration:** Loosely typed with optional type anotation (`my_var: int = 5`).
- **Data Types:** `int`, `float`, `char`, `string`, `bool` (Currently only numerical operations are supported).
- **Automatic Type Coersion**: Allows for arithmetic operations on mixed (compatible) data types like `int` and `float`.
- **Rich set of operators:**
    - Arithmetic: `+`, `-`, `*`, `/`, `//` (floor division), `%` (modulo).
    - Comparison: `==`, `!=`, `>`, `>=`, `<`, `<=`.
    - Floating point arithmetic also supported.
- **Control Flow:** `if then else` statements and `loop from to` loops.
- **Input/Output:** Writing to the shell with `print`.
- **Comments:** Single-line `!` and multi-line `!! ... !!` comments.
- **Flexible Syntax:** Newlines and indentation are not syntactically significant.

## Syntax
`example.etc` file:
```
! Single line comment
!! Multiline comment
   like this !!

! Variables can be initialised with a type
a: int = 5
! Or auto inferred based on expression
b = 10.5

! Re-assignment also possible
b = 1.5

! Use variables in expressions with $
sum = $a + $b
! Print doesn't require $
print sum

!! Newlines and tabs are completely optional
but may be used for style !!
if a > b then
    print $a
else
    print $b
end if

! Loops are inclusive and auto-initialise the loop variable
loop from 1 to 5 in i
    print $i
end loop
```

More programs are available in [`/programs`](./programs).
