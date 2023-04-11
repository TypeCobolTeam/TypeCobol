000010 Display toto
000020 display "toto"
000020 Display 'toto'
000030 display "toto" no advancing
003360     DISPLAY '*** WHATEVER *** : ' DEBUG-ITEM '.'
000040 display no advancing with no advancing
000040 display "no advancing" with no advancing
000040 display 'no advancing' with no advancing
000040 display var1 'lit1' var2 with no advancing
000040 display var1 'lit1' var2 upon SYSIN with no advancing
000040 display var1 'lit1' var2 upon SYSIN no advancing
000040 display var1 'lit1' var2 upon toto with no advancing
000040 display var1 'lit1' var2 upon toto no advancing
000040 display "C'est à dire"
000040 display 'C"est à dire'
000040 display var1 'litError' var2 upon SYSIN toto with no advancing
000040 display var1 'lit2' var2 upon var2 with no advancing
000000 display when-compiled
000000 display function max(1 5)
000000 display toto(1:5) toto toto of mycopy
000000 display high-value
000000 display spaces
000000 display all 'a'
000000 display all 'a' upon SYSIN with no advancing
000000 display all X'40'
000000 display all HIGH-VALUE
000000 display all HIGH-VALUES
000000 display all LOW-VALUE
000000 display all LOW-VALUES
000000 display all QUOTE
000000 display all QUOTES
000000 display all SPACE
000000 display all SPACES
000000 display all ZERO
000000 display all ZEROS
000000 display all ZEROES
000000 display all "test\0"
000000 display all "a" "b"
000000 display "a" all "b" "c"
000000 display "a" all "b" 9
000000 display "a" all "b" Var1
000000 display 9 all 'a'
000000 display Var1 all 'a'
000000 display 9 all 'a' 9 all 'a' Var1 "test" all LOW-VALUE
000000 display all 9
000000 display all Var1