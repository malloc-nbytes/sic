# sic
Sample Input Creator

# Description
`sic` is a program which takes a list of *functions* and *arguments* which then produces a text file based on the functions called.

# Requirements
- Haskell
  `https://www.haskell.org/downloads/`

# Usage

## Syntax
As stated previously, everything is a function that contains some number of arguments. For the most part, functions take any number of arguments (except a few). Arguments to a function are separated by a space (not a comma). Strings are surrounded in quotes and integer literals are just numbers that you provide. Functions must start with their name followed by `(`, list of arguments, then `)`. Comments start with `//` and will skip until the end of the line. You can also use the escape character `\` to have quotes inside of a string literal.

There are two different types of functions, _inner_ and _outer_. Inner functions can only be called inside of the following functions:
1. `write()`
2. `var()`
3. `output()`

Outer functions are the opposite. They must not be called inside of another function.

Valid function call:
```write("foo" n() "bar" r(3 "hello" r(2 "baz") 54321) w(4) " " n() #someVariable)```.

Invalid function call:
```r(3 var(#x "hello") write(" "))```

### Outer Functions

[NOTE]: `~` denotes *optional*, `Z` denotes an integer, `...` denotes variadic parameters separated by a space.

1. `write(...)` - Takes in any number of arguments and writes them to a file (the default file path is `out.txt`, but can be changed with `output()`).
2. `var(#ID ...)` - The first argument is the identifier which *must* come after `#`. It then takes any number of arguments and binds it to that ID.
3. `output(...)` - Takes at least 1 argument and sets the file to write the result to to whatever is provided.
4. `limitWildCard(Z)` - Takes a single integer and limits the number of characters that `w()` will produce.

### Inner Functions

1. `r(Z ...)` - Takes a single integer and repeats the pattern in the rest of the arguments.
2. `n(~Z)` - Takes 0 or 1 integers and inserts a newline based on the number provided (or 1 if no number is given).
3. `w(~Z)` - Takes 0 or 1 integers and inserts `Z` characters (or 5 if no number is provided). Currently it takes letters in-order, but in the future it will be random.
4. `iota(~Z)` - Produces an integer that is incremented every time it is called (or will increment by `Z` if provided).

See `./examples/` for examples.

## Building
To build, do the following:
```
git clone https://www.github.com/malloc-nbytes/sic.git/
cd ./sic/src/
./build.sh
```

## Running it
`./sic <input_filepath>`

## Installation
```
git clone https://github.com/malloc-nbytes/sic.git
cd ./sic/src/
./build.sh clean (if built once already)
./build.sh
./install.sh
```

# Issues
1. Variable names with `_` in the name results in an infinite loop.
2. A string literal missing the first quote but has the ending will result in an infinite loop.
3. A string literal missing the closing quote will result in a not-so-helpful error message.
