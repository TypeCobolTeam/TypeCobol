      *TypeCobol_Version:v0.0.0-local
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TC-Pgm2 pic X(08) value 'PGM2'.
       01 TC-Call          PIC X VALUE 'T'.
           88 TC-FirstCall  VALUE 'T'.
           88 TC-NthCall    VALUE 'F'
                            X'00' thru 'S'
                            'U' thru X'FF'.

                     
       LOCAL-STORAGE SECTION.
       01 txt   pic X(100).
       01 l     pic S9(5) comp-5.
       01 RC    pic 99.
       LINKAGE SECTION.
      *Common to all librairies used by the program.
       01 TC-Library-PntTab.
          05 TC-Library-PntNbr          PIC S9(04) COMP.
          05 TC-Library-Item OCCURS 1000
                               DEPENDING ON TC-Library-PntNbr
                               INDEXED   BY TC-Library-Idx.
              10 TC-Library-Item-Idt      PIC X(08).
              10 TC-Library-Item-Pnt      PROCEDURE-POINTER.

      *To call program a0263d55 in module Pgm2
      *Which is generated code for Pgm2.Foo
      *Declared in source file Dependency.tcbl
       01 TC-Pgm2-a0263d55-Item.
          05 TC-Pgm2-a0263d55-Idt PIC X(08).
          05 TC-Pgm2-a0263d55 PROCEDURE-POINTER.

       PROCEDURE DIVISION.
      *
           PERFORM TC-INITIALIZATIONS

                          
      *All 4 calls should resolve to the same overload
      *    Call Pgm2::Foo Input txt
      *                         l
      *                  Output RC.
           
           IF ADDRESS OF TC-Pgm2-a0263d55-Item = NULL
             OR TC-Pgm2-a0263d55-Idt not = 'a0263d55'
               PERFORM TC-LOAD-POINTERS-Pgm2
           END-IF
      *    Equivalent to call a0263d55 in module Pgm2
           CALL TC-Pgm2-a0263d55 USING
                                 txt
                                 l
                    by reference RC
           end-call
                                  .

      *    Call Pgm2::Foo Input txt
      *              by content l
      *                  Output RC.
           
           IF ADDRESS OF TC-Pgm2-a0263d55-Item = NULL
             OR TC-Pgm2-a0263d55-Idt not = 'a0263d55'
               PERFORM TC-LOAD-POINTERS-Pgm2
           END-IF
      *    Equivalent to call a0263d55 in module Pgm2
           CALL TC-Pgm2-a0263d55 USING
                                 txt
                    by content   l
                    by reference RC
           end-call
                                  .

      *    Call Pgm2::Foo Input txt
      *               length of txt
      *                  Output RC.
           
           IF ADDRESS OF TC-Pgm2-a0263d55-Item = NULL
             OR TC-Pgm2-a0263d55-Idt not = 'a0263d55'
               PERFORM TC-LOAD-POINTERS-Pgm2
           END-IF
      *    Equivalent to call a0263d55 in module Pgm2
           CALL TC-Pgm2-a0263d55 USING
                                 txt
                                 length of txt
                    by reference RC
           end-call
                                  .

      *    Call Pgm2::Foo Input txt
      *    by content length of txt
      *                  Output RC.
           
           IF ADDRESS OF TC-Pgm2-a0263d55-Item = NULL
             OR TC-Pgm2-a0263d55-Idt not = 'a0263d55'
               PERFORM TC-LOAD-POINTERS-Pgm2
           END-IF
      *    Equivalent to call a0263d55 in module Pgm2
           CALL TC-Pgm2-a0263d55 USING
                                 txt
                    by content   length of txt
                    by reference RC
           end-call
                                  .
      *=================================================================
       TC-INITIALIZATIONS.
      *=================================================================
            IF TC-FirstCall
                 SET TC-NthCall TO TRUE
                 SET ADDRESS OF TC-Pgm2-a0263d55-Item  TO NULL
            END-IF
            .
      *=================================================================
        TC-LOAD-POINTERS-Pgm2.
      *=================================================================
            CALL 'ZCALLPGM' USING TC-Pgm2
            ADDRESS OF TC-Library-PntTab
            PERFORM VARYING TC-Library-Idx FROM 1 BY 1
            UNTIL TC-Library-Idx > TC-Library-PntNbr
                EVALUATE TC-Library-Item-Idt (TC-Library-Idx)
                WHEN 'a0263d55'
                     SET ADDRESS OF
                     TC-Pgm2-a0263d55-Item
                     TO ADDRESS OF
                     TC-Library-Item(TC-Library-Idx)
                WHEN OTHER
                     CONTINUE
                END-EVALUATE
            END-PERFORM
            .


       END PROGRAM Pgm1.
