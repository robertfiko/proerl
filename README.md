#  ProErl
**Simple Erlang Interpreter written in Prolog**

For some simple explanation on theoritical sections please check [THEORITICAL.md](THEORITICAL.md)


TODO: http://aszt.inf.elte.hu/~asvanyi/pl/cm/pas/scan.pl

## Prerequisites
- Sicstus 4.7.1
- Docker
- GNU Make

## Limitations and rules on the accepted syntax

This Erlang ""interpreter"" can only interpret simple Erlang codes, according to the following rules:

- The file should be an Erlang module, consisting of a sequence of attributes and function declarations, each terminated by period `.`. [See Erlang Module Syntax](https://www.erlang.org/doc/reference_manual/modules.html#module-syntax). **Limnitation:** only the following attributes are accepted, any other attributes will be raised as an error.
  - `-module(...)`
  - `-export([...])`
  - Attributes can only be defined in one line
- There should be an empty line at the end of the file
- Functions with [Guard](https://www.erlang.org/doc/reference_manual/expressions.html#guard-sequences) expression cannot be evaluated at the time.
- Only atoms that start with small charcters, can be accepted, so those that would be an atom without the `'` wrapping
- No inline functions and lambdas
  - `fun(...) -> ... end` or `fun foo/n`
- No dots (`.`) anywhere else besides marking the end of the function and attributes.
  - So no floats
- Function closing dot should be at the end of the last term of the function 
- Each term/statement in function should be in new line
- Strings are not allowed 
- Variables cannot be statements alone, like: `... Variable, ...`
- Cannot prefix variables, that you want no binding: `_Var`, or use the discard simbol: `_` in bindings or function headers, calls
- No multiple function clauses, so no recursion
- Functions should be only called with atoms or variables.
- Parameter bindings in function calls are implemented, but variable names should be globally unique
- Keep in mind that variables are immutable
- Variable bindings are posibble for arithmetic expressions, atoms, another variables, binding chains: (`A = B = c`)
- Comments and empty lines are recognised and skipped 



## Opportunities for further development
- Cross-module function calls
- Complete testing on SWI Prolog and/or Sicstus image (legal issues)
- Intorducing basic logic expressions (`and`, `or`) in function body.
- Right now, each term/statement should be in new line; it should allow to have multiple ones in one line
- Allow that function closing dot to be in a new line
- List operations
- Clear boudn variables in function scope (right now after each run the whole "BEAM" is resetted)
- Implement characters, strings (list of characters)
- Interactive shell
- Checking for the export list 


## Testing 
Due to differences in SWI and Sicstus Prolog, testing has two sets. Smaller
unit-like tests can be run in SWI prolog, but complex integration-like and 
system-wide tests can only be run in Sicstus, as the projects supports only
Sicstus Prolog.

Since SWI offers Docker image, the CI testing is based on that, but due to the above
mentioned reasons, and the fact that Sicstus is closed-source that is not complete.

If you have Sicstus installed locally, you can run a wider set of tests using:
`make local`

To create the SWI based testing image, run: `make docker` 
To run the tests inside the SWI based image, run: `make test`

## How to section

A few how-tos or thinking written down.

### How to parse function body

A function body is list of terms (Erlang terms) separated by comas (`,`). Let's see what are the possible options:
- atom
  - can be a simple atom
  - or a function call
- variable
- other literals
  - charcter
  - number
- basic arithmethic expressions (`+`, `-`, `/`, `*`)

### Skipping comments

Comments can occour either on top-level (in the level of function definitions and attributes)
or in function bodies. Comments are just skipped in syntactical layer.