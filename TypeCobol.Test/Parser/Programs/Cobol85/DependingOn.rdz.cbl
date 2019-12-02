﻿000000 IDENTIFICATION DIVISION.
000000 PROGRAM-ID. StringStatement.
000000 ENVIRONMENT DIVISION.
000000 CONFIGURATION SECTION.
000000 SOURCE-COMPUTER. IBM-370.
       special-names. decimal-point is comma.
000000 DATA DIVISION.
000000 working-storage section.
000000 01 MyGroup.
000000    05 MyCounter pic 999 comp-5.
      *Ok MyCounter is unique in the source
000000    05 MyTab     occurs 99 depending on MyCounter.


       01 MyAmbiguousGroup.
      *OK you can declare 2 variables with the same name
          05 MyAmbiguousCounter  pic 999 comp-5.
      *OK you can declare 2 variables with the same name
          05 MyAmbiguousCounter  pic 999 comp-5.
      *KO reference to MyAmbiguousCounter is ... Ambiguous
          05 MyAmbiguousTab     occurs 99 
                                depending on MyAmbiguousCounter.

       01 MyGroup3.
          05 MyCounter3  pic 999 comp-5.
      *KO depending on clause cannot be a numeric
          05 MyTab3     occurs 99 depending on 999.
 
       01 MyGroup4.
      *OK MyCounter is declared in another group but it works
          05 MyTab3     occurs 99 depending on MyCounter.


000000 01 MyGroup5.
000000    05 MySubGroup5.
000000       10 MySubSubGroup51.
000000          15 MyCounter5 pic 999 comp-5.
000000       10 MySubSubGroup52.
000000          10 MyCounter5 pic 999 comp-5.
      *Ok use operator OF
000000    05 MyTab5     occurs 99 
                        depending on MyCounter5 of MySubSubGroup51. 
      *Ok use operator IN
000000    05 MyTab51     occurs 99 
                         depending on MyCounter5 IN MySubSubGroup51.

      *KO
000000    05 MyTab5     occurs 99 
                        depending on MyCounter5 of MyGroup5. 
      *KO
000000    05 MyTab5     occurs 99 depending on 
                        MyCounter5 of MySubGroup5 of MyGroup5.
      *Ok
000000    05 MyTab5     occurs 99 depending on 
                        MyCounter5 of MySubSubGroup51 of 
                        MySubGroup5 of MyGroup5.
000000 PROCEDURE DIVISION.
000000     move 1 to MyCounter
000000     .
000000 END PROGRAM MYPGM.