      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TC-DPDCY01 pic X(08) value 'DPDCY01'.
       01 TC-Call          PIC X VALUE 'T'.
           88 TC-FirstCall  VALUE 'T'.
           88 TC-NthCall    VALUE 'F'
                            X'00' thru 'S'
                            'U' thru X'FF'.

                               
       01 alpha     pic X.
      *01 myVar     type DPDCY01::GroupType.
       01 myVar.
           02 subType.
             03 myPic pic X.
                                            
       LINKAGE SECTION.
      *Common to all librairies used by the program.
       01 TC-Library-PntTab.
          05 TC-Library-PntNbr          PIC S9(04) COMP.
          05 TC-Library-Item OCCURS 1000
                               DEPENDING ON TC-Library-PntNbr
                               INDEXED   BY TC-Library-Idx.
              10 TC-Library-Item-Idt      PIC X(08).
              10 TC-Library-Item-Pnt      PROCEDURE-POINTER.

      *To call program b20bd03f in module DPDCY01
      *Which is generated code for DPDCY01.FunTest
      *Declared in source file Dependency.tcbl
       01 TC-DPDCY01-b20bd03f-Item.
          05 TC-DPDCY01-b20bd03f-Idt PIC X(08).
          05 TC-DPDCY01-b20bd03f PROCEDURE-POINTER.
      *To call program fa50eaf6 in module DPDCY01
      *Which is generated code for DPDCY01.FunTest
      *Declared in source file Dependency.tcbl
       01 TC-DPDCY01-fa50eaf6-Item.
          05 TC-DPDCY01-fa50eaf6-Idt PIC X(08).
          05 TC-DPDCY01-fa50eaf6 PROCEDURE-POINTER.

       PROCEDURE DIVISION.
       INIT-LIBRARY.
      *
           PERFORM TC-INITIALIZATIONS

                    
           Continue.
      *Must resolve dependency function acepting alphanumeric
      *     call DPDCY01::FunTest input alpha.
            
           IF ADDRESS OF TC-DPDCY01-b20bd03f-Item = NULL
             OR TC-DPDCY01-b20bd03f-Idt not = 'b20bd03f'
               PERFORM TC-LOAD-POINTERS-DPDCY01
           END-IF
      *    Equivalent to call b20bd03f in module DPDCY01
           CALL TC-DPDCY01-b20bd03f USING
                                 alpha
           end-call
                                             .
      *Must resolve dependency function acepting Type01
      *     call DPDCY01::FunTest input myVar::subType.
            
           IF ADDRESS OF TC-DPDCY01-fa50eaf6-Item = NULL
             OR TC-DPDCY01-fa50eaf6-Idt not = 'fa50eaf6'
               PERFORM TC-LOAD-POINTERS-DPDCY01
           END-IF
      *    Equivalent to call fa50eaf6 in module DPDCY01
           CALL TC-DPDCY01-fa50eaf6 USING
                                 subType IN myVar
           end-call
                                                      .
      *=================================================================
       TC-INITIALIZATIONS.
      *=================================================================
            IF TC-FirstCall
                 SET TC-NthCall TO TRUE
                 SET ADDRESS OF TC-DPDCY01-b20bd03f-Item  TO NULL
                 SET ADDRESS OF TC-DPDCY01-fa50eaf6-Item  TO NULL
            END-IF
            .
      *=================================================================
        TC-LOAD-POINTERS-DPDCY01.
      *=================================================================
            CALL 'ZCALLPGM' USING TC-DPDCY01
            ADDRESS OF TC-Library-PntTab
            PERFORM VARYING TC-Library-Idx FROM 1 BY 1
            UNTIL TC-Library-Idx > TC-Library-PntNbr
                EVALUATE TC-Library-Item-Idt (TC-Library-Idx)
                WHEN 'b20bd03f'
                     SET ADDRESS OF
                     TC-DPDCY01-b20bd03f-Item
                     TO ADDRESS OF
                     TC-Library-Item(TC-Library-Idx)
                WHEN 'fa50eaf6'
                     SET ADDRESS OF
                     TC-DPDCY01-fa50eaf6-Item
                     TO ADDRESS OF
                     TC-Library-Item(TC-Library-Idx)
                WHEN OTHER
                     CONTINUE
                END-EVALUATE
            END-PERFORM
            .

       end program PGMTEST.
