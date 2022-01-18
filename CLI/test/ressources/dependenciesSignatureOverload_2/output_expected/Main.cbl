      *TypeCobol_Version:v0.0.0-local
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TC-DPDCY01 pic X(08) value 'DPDCY01'.
       01 TC-Call          PIC X VALUE 'T'.
           88 TC-FirstCall  VALUE 'T'.
           88 TC-NthCall    VALUE 'F'
                            X'00' thru 'S'
                            'U' thru X'FF'.

                               
      *01  ThisReturnCode               TYPE ReturnCode.
       01 ThisReturnCode.
           02 Cod1 pic X(04).
              88 Ok value '0000'.
              88 Ko value '0001'.
           02 Cod2 pic X(04).
                                                        
      *01  W-SystemDateDB2              TYPE DateDB2.
       01 W-SystemDateDB2.
           02 YYYY PIC 9(04).
           02 filler PIC X value '-'.
           02 MM PIC 9(02).
           02 filler PIC X value '-'.
           02 DD PIC 9(02).
                                                     
      *01  W-Date                       TYPE Date.
       01 W-Date.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                                  
      *01  W-Type1                      TYPE DPDCY01::Type1.
       01 W-Type1.
           02 var1.
             03 YYYY PIC 9(4).
             03 MM PIC 9(2).
             03 DD PIC 9(2).
                                                            
       LINKAGE SECTION.
      *Common to all librairies used by the program.
       01 TC-Library-PntTab.
          05 TC-Library-PntNbr          PIC S9(04) COMP.
          05 TC-Library-Item OCCURS 1000
                               DEPENDING ON TC-Library-PntNbr
                               INDEXED   BY TC-Library-Idx.
              10 TC-Library-Item-Idt      PIC X(08).
              10 TC-Library-Item-Pnt      PROCEDURE-POINTER.

      *To call program a980c11f in module DPDCY01
      *Which is generated code for DPDCY01.Foo
      *Declared in source file Dependency.tcbl
       01 TC-DPDCY01-a980c11f-Item.
          05 TC-DPDCY01-a980c11f-Idt PIC X(08).
          05 TC-DPDCY01-a980c11f PROCEDURE-POINTER.
      *To call program eef83fbd in module DPDCY01
      *Which is generated code for DPDCY01.Foo
      *Declared in source file Dependency.tcbl
       01 TC-DPDCY01-eef83fbd-Item.
          05 TC-DPDCY01-eef83fbd-Idt PIC X(08).
          05 TC-DPDCY01-eef83fbd PROCEDURE-POINTER.

       PROCEDURE DIVISION.
      *
           PERFORM TC-INITIALIZATIONS

                          
      *Calling first overload of DPDCY01::Foo
      *    CALL DPDCY01::Foo  INPUT W-Date
      *                       OUTPUT W-SystemDateDB2
      *                              ThisReturnCode
           
           IF ADDRESS OF TC-DPDCY01-a980c11f-Item = NULL
             OR TC-DPDCY01-a980c11f-Idt not = 'a980c11f'
               PERFORM TC-LOAD-POINTERS-DPDCY01
           END-IF
      *    Equivalent to call a980c11f in module DPDCY01
           CALL TC-DPDCY01-a980c11f USING
                                 W-Date
                    by reference W-SystemDateDB2
                                 ThisReturnCode
           end-call
                                                   
      *Calling second overload of DPDCY01::Foo
      *    CALL DPDCY01::Foo  INPUT W-SystemDateDB2
      *                       OUTPUT W-Type1::var1
      *                              ThisReturnCode
           
           IF ADDRESS OF TC-DPDCY01-eef83fbd-Item = NULL
             OR TC-DPDCY01-eef83fbd-Idt not = 'eef83fbd'
               PERFORM TC-LOAD-POINTERS-DPDCY01
           END-IF
      *    Equivalent to call eef83fbd in module DPDCY01
           CALL TC-DPDCY01-eef83fbd USING
                                 W-SystemDateDB2
                    by reference var1 IN W-Type1
                                 ThisReturnCode
           end-call
                                                   
           goback
           .
      *=================================================================
       TC-INITIALIZATIONS.
      *=================================================================
            IF TC-FirstCall
                 SET TC-NthCall TO TRUE
                 SET ADDRESS OF TC-DPDCY01-a980c11f-Item  TO NULL
                 SET ADDRESS OF TC-DPDCY01-eef83fbd-Item  TO NULL
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
                WHEN 'a980c11f'
                     SET ADDRESS OF
                     TC-DPDCY01-a980c11f-Item
                     TO ADDRESS OF
                     TC-Library-Item(TC-Library-Idx)
                WHEN 'eef83fbd'
                     SET ADDRESS OF
                     TC-DPDCY01-eef83fbd-Item
                     TO ADDRESS OF
                     TC-Library-Item(TC-Library-Idx)
                WHEN OTHER
                     CONTINUE
                END-EVALUATE
            END-PERFORM
            .

       END PROGRAM Main.
