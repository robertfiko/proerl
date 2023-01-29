# Theoritical explanations for ProErl

## Layers

Usuially compilers have three layers of analysis: lexical, syntactical and semantical.

The **lexical** part is where the tokenization takes place. You can think of this section 
as forming the words in a natural language. This is mainly done by the `scan.pl` module.
TODO: kié?

The **syntactical** part is where from the list of tokens, syntactical nodes are formed,
and of course the correct order is checked. You can think of this, like forming the
structure of sentences in a natural language. You put comas, dots in correct positions.
This is mainly done by the `syntax.pl` module.

The **semantical** part is when the meaning is checked. Can we add a number and an atom
together? Maybe, maybe not. This is the part when the sentence gets its meaning.
This is mainly done by the `semantics.pl` module.

**Disclaimer:** since this project is mainly for education pruposes on how to write
simple interpreters in Prolog, these layers may not have been that strictly divided.

## Nodes and AST

Usually behind the scenes compilers, interpreters use Abstarct Syntax Trees (AST).
In this project I usually use nodes. A simple node may just be a Prolog atom or 
any other prolog term. For complex nodes, I introduced different kind of notations.

These notations usually represented by Prolog predicates, with not so usual names.
The arugments of these predicates can be either simple or complex nodes.

I used the following conventions on naming these nodes:
- `<NODE>(...)`: syntactical node
- `$NODE(...)`: semantical node
- `!NODE(...)`: temporal node, this needs attention and **should not present for long**