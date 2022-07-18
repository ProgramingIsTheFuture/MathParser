# Basic Parser

This is a basic parser for simple math expressions

Basic usage

(The name of the binary is math and is in your PATH)

If no arguments are passed it will just interpret one line.
```bash
$ math
|> 2 + 5
Result: 7
```

If a file is passed it will parse all the lines inside it
Not yet implemented:
```bash
$ math file.math
```

This software support expressions like:
 - `32 + 5`
 - `6 - 23`
 - `4 / 2`
 - `8 * 3`

And also expressions more complex like:
 - `32 / 2 + 4`

Note:
  * No negative numbers yet
  * No decimal numbers
  * No priority (`PEDMAS` - Parenthesis, Exponents, Divisions, Multiplications, Add, Subtractions)
