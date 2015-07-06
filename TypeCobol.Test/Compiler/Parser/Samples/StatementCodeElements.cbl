*   /////////
*  // ADD //
* /////////

* FORMAT 1
* IDENTIFIERS
ADD a TO x.
ADD titi tata TO toto.
ADD a b ab TO x toto.
ADD 
          a 
    TO                   x
.
* LITERALS
ADD 1 TO x.
ADD 1 2 TO toto.
ADD 1 2 3 TO x toto.
* SYNTAX ERRORS
* litterals not allowed as 2nd operand
*ADD 1 TO 1
*ADD 1 2 TO 1 2
*ADD x TO 1
*ADD 1 x TO 1
*ADD 1 x TO 1 2
* incomplete statement
*ADD
*ADD.
*ADD a TO
*ADD 1 2 TO.
*ADD 1 TO .x

* FORMAT 2
* IDENTIFIERS
ADD a TO m GIVING x.
ADD titi TO tata GIVING toto.
ADD a b ab TO m GIVING x toto.
* LITERALS
ADD 1 TO m GIVING x.
ADD 1 TO m GIVING x y.
ADD a 1 TO 2 GIVING x.
ADD a b ab TO 1 GIVING x toto.
* SYNTAX ERRORS
* only 1 identifier as 2nd operand
*ADD a b ab TO x y xy GIVING titi toto.
* incomplete statement
*ADD a TO m GIVING  .
*ADD a TO GIVING x.

* FORMAT 3
* IDENTIFIERS
ADD CORRESPONDING a TO x.
ADD CORR a TO x.
* SYNTAX ERRORS
* only 1 identifier as operand
*ADD CORRESPONDING a TO x y.
*ADD CORRESPONDING a b TO x.
*ADD CORRESPONDING a b TO x y.