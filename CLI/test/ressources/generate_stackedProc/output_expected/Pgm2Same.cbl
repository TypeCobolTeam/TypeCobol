      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2.
       DATA DIVISION.
                                                         
       WORKING-STORAGE SECTION.
       01  TC-PGM2-FctList-Loaded PIC X(02).
           88 TC-PGM2-FctList-IsLoaded      VALUE 'OK'.

       01 TC-PGM2-PntTab.
           05 TC-PGM2-PntNbr         PIC S9(04) COMP VALUE 1.
      *To call program a4ee502d
      *Which is generated code for PGM2.Proc1
      *Declared in source file Pgm2Same.tcbl
           05 TC-PGM2-a4ee502d-Idt   PIC X(08) VALUE 'a4ee502d'.
           05 TC-PGM2-a4ee502d PROCEDURE-POINTER.

       
       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.
       01 TC-A1 PIC X.

      * same name PGM2
       PROCEDURE DIVISION USING PntTab-Pnt.
                          
      *
      *    IF CallIsCopy
      *      PERFORM Copy-Process-Mode
      *    ELSE
           PERFORM FctList-Process-Mode
           perform INIT-LIBRARY
      *    END-IF

           GOBACK.

        FctList-Process-Mode.
            IF NOT TC-PGM2-FctList-IsLoaded
              SET TC-PGM2-a4ee502d   TO ENTRY 'a4ee502d'
              SET TC-PGM2-FctList-IsLoaded TO TRUE
            END-IF
               .

            set PntTab-Pnt TO ADDRESS OF TC-PGM2-PntTab

           .
                          
       INIT-LIBRARY.
           continue.
           .
       PA-ALL-ENTRIES.
           ENTRY 'a4ee502d' USING TC-A1
               CALL "a4ee502d" USING TC-A1
               GOBACK.


      *declare procedure Proc1 public
      *    input t pic x.
      
      *
      *declare procedure Proc1 public
      *    input t pic x.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. a4ee502d IS COMMON.
       DATA DIVISION.
       LINKAGE SECTION.
      *PGM2.Proc1 - Params :
      *     input(t: pic x)
       01 t pic x.
       PROCEDURE DIVISION
             USING BY REFERENCE t
           .
      *PGM2.Proc1 - Params :
      *     input(t: pic x)
           .
       END PROGRAM a4ee502d.
       END PROGRAM PGM2.

