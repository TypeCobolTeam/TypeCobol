* missing tokens
CALL USING x
CALL f USING
CALL f USING BY a
CALL f USING a RETURNING
* too many tokens
CALL f g
CALL f RETURNING x y
* unknown token
CALL f USING NY VALUE i
CALL f USING BY MALUS z
* illegal usages
CALL f USING FUNCTION RANDOM
CALL f USING LENGTH OF x
CALL f USING                        a
             by content   length of b
             by reference length of c.
CALL f USING LINAGE-COUNTER OF x
CALL f USING BY VALUE OMITTED