string "toto" "titi" delimited by size into toto
string "toto2" "titi2" delimited by size into toto end-string
string "toto3" "titi3" delimited by "bob3" into toto
string "toto4" "titi4" delimited by "bob4" into toto end-string
string "a" 'b' delimited by SIZE "C" "D" 'e' delimited "delimiter" into toto
string "a" 'b' delimited by SIZE "C" "D" 'e' delimited "delimiter" into toto end-string
string "a" 'b' delimited by SIZE "C" "D" 'e' delimited by "delimiter" into toto
string "a" 'b' delimited by SIZE "C" "D" 'e' delimited by "delimiter" into toto end-string
string "a" 'b' delimited by SIZE "C" "D" 'e' delimited by "delimiter" into toto POINTER myPointer
string "a" 'b' delimited by SIZE "C" "D" 'e' delimited by "delimiter" into toto POINTER-32 myPointer
string "a" 'b' delimited by SIZE "C" "D" 'e' delimited by "delimiter" into toto POINTER myPointer end-string
string "a" 'b' delimited by SIZE "C" "D" 'e' delimited by "delimiter" into toto with POINTER myPointer
string "a" 'b' delimited by SIZE "C" "D" 'e' delimited by "delimiter" into toto with POINTER myPointer end-string
string "date compilation=" when-compiled delimited by size into result
string var1(1:5) var1 delimited by "term" into result
string "testOverFlow" delimited by size into result on overflow display "marche pas"
string "testOverFlow" delimited by size into result on overflow display "marche pas" not on overflow display "ça marche"