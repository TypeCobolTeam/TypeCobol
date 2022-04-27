000010        Display toto
000020        display "toto"
000020        Display 'toto'
000030        display "toto" no advancing
003360        DISPLAY '*** WHATEVER *** : ' DEBUG-ITEM '.'
000040        display no advancing with no advancing
000040        display "no advancing" with no advancing
000040        display 'no advancing' with no advancing
000040        display var1 'lit1' var2 with no advancing
000040        display var1 'lit1' var2 upon SYSIN with no advancing
000040        display var1 'lit1' var2 upon SYSIN no advancing
000040        display var1 'lit1' var2 upon toto with no advancing
000040        display var1 'lit1' var2 upon toto no advancing
000040        display "C'est à dire"
000040        display 'C"est à dire'
000040        display var1 'litError' var2 upon SYSIN toto with no advancing
000040        display var1 'lit2' var2 upon var2 with no advancing
000000        display when-compiled
000000        display function max(1 5)
000000        display toto(1:5) toto toto of mycopy
000000        display high-value
000000        display spaces
