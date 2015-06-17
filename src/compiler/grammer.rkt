#lang ragg

app: expr+

expr: calc
    | bind
    | assign
    | call

bind: IDENTIFIER "=" (function | call | calc)

assign: IDENTIFIER ":=" (function | call | calc)

call: IDENTIFIER "(" calc* ")"

function: "(" IDENTIFIER* ")" "->" expr+ "end"

calc: BOOLEAN
    | term (("+" | "-") term)*

term: factor (("*" | "/") factor)*

factor: INTEGER
      | IDENTIFIER
      | "(" calc ")"
      | call
