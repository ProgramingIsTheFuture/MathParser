# Basic Parser

### Description

This is a basic parser for simple math expressions

### About the project

This project is built with OCaml and Dune. (No external dependencies for now)

You can build this using:

 ```bash
 $ dune build
 ```

This software support expressions like:
 - `32 + 5`
 - `6 - 23`
 - `4 / 2`
 - `8 * 3`

And, also expressions more complex like:
 - `32 / 2 + 4`

### Basic usage

Note: *The name of the binary is math and is in your PATH*

If no arguments are passed, it will just interpret one line at a time.
```bash
$ math
|> 2 + 5
Result: 7
|> 
```

If a file is passed, it will parse all the lines inside it 
and give us an output about each one.

File: `main.math`
```math
5 + 10 + 1 / 4

10 + 12
6
```

Output:
```bash
$ math main.math
Result: 4
Result: 0
Result: 22
Result: 6
```

This `Result: 0` is not supposed to appear, this should be fixed.

### To-do:
  - [ ] Output specify the line of the file
  - [ ] Skip empty lines
  - [ ] No negative numbers yet
  - [ ] No decimal numbers
  - [ ] No priority (`PEDMAS` -- Parenthesis, Exponents, Divisions, Multiplications, Add, Subtractions)
