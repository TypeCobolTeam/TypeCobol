*   //////////////
*  // SUBTRACT //
* //////////////

* FORMAT 1
* IDENTIFIERS
SUBTRACT a FROM x.
SUBTRACT titi tata FROM toto.
SUBTRACT a b ab FROM x toto.
SUBTRACT 
          a 
    FROM                   x
.
* LITERALS
SUBTRACT 1 FROM x.
SUBTRACT 1 2 FROM toto.
SUBTRACT 1 2 3 FROM x toto.

* Invalid
SUBTRACT .
SUBTRACT 1 TO x.