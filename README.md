#  ProErl
**Simple Erlang Interpreter written in Prolog**

## TODO:
- [ ] Error handling
- [ ] No strings
- [ ] No chars
- [ ] YES comments
- [ ] only simple arithmetic expressions

## Limitations

This Erlang ""interpreter"" can only interpret simple Erlang codes, according to the following rules:

- The file should be an Erlang module, consisting of a sequence of attributes and function declarations, each terminated by period `.`. [See Erlang Module Syntax](https://www.erlang.org/doc/reference_manual/modules.html#module-syntax). **Limnitation:** only the following attributes are accepted, any other attributes will be raised as an error.
  - `-module(...)`
  - `-export([...])`
- There should be an empty line at the end of the file
- Functions with Guard (TODO:) expression cannot be evaluated at the time.
- Only atoms that start with small charcters, can be accepted, so those that would be an atom without the `'` wrapping
- No inline functions and lambdas
  - `fun(...) -> ... end` or `fun foo/n`

## Possibilities
- 

## How to section

### How to parse function body

A function body is list of terms (Erlang terms) separated by comas (`,`). Let's see what are the possible options:
- atom
  - can be a simple atom
  - or a function call
- variable
- literals
  - charcter
  - number
- basic arithmethic expressions (`+`, `-`, `/`, `*`)
  - will trested as syntactic sugars
- basic logic expressions (`and`, `or`)