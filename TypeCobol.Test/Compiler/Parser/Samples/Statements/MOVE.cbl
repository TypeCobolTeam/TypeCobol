* OK
MOVE CORRESPONDING a to x
* KO: too much elements
MOVE CORRESPONDING a to x y
* KO: no literal with CORRESPONDING
MOVE CORRESPONDING 1 TO x
* OK: intrinsic function
MOVE FUNCTION RANDOM TO x.
MOVE FUNCTION LENGTH (x) TO x.
* KO: illegal intrinsic function after TO
MOVE x TO FUNCTION RANDOM
MOVE x TO FUNCTION LENGTH (x)
* OK: special registers
MOVE LENGTH OF x TO x.
MOVE LENGTH x TO x.
MOVE x TO LENGTH OF x.
MOVE x TO LENGTH x.
* OK: continuations
                 MOVE 'Lorem ipsum dolor sit amet, consectetur adi
-               'piscing elit, sed do eiusmod tempor incididunt ut
-               'labore et dolore magna aliqua                 '  
                                         TO  SOMEWHERE           