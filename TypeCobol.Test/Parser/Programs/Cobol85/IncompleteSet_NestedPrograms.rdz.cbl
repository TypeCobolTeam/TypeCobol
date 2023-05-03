       IDENTIFICATION DIVISION.
       PROGRAM-ID. MainPgm.
       procedure division.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested1.
       data division.
       local-storage section.
       01 Var1             pic X.

       procedure division.
           if ( Var1 = "A" )
                set

       end program Nested1.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested2.
       procedure division.
           goback
           .
       end program Nested2.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested3.

       END PROGRAM Nested3.

       END PROGRAM MainPgm.