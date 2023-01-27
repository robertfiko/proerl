#  ???

This Erlang ""interpreter"" can only interpret simple Erlang codes, according to the following rules:

- The file should be an Erlang module, consisting of a sequence of attributes and function declarations, each terminated by period `.`. [See Erlang Module Syntax](https://www.erlang.org/doc/reference_manual/modules.html#module-syntax). **Limnitation:** only the following attributes are accepted, any other attributes will be raised as an error.
  - `-module(...)`
  - `-export([...])`

- Functions with Guard (TODO:) expression cannot be evaluated at the time.
- 