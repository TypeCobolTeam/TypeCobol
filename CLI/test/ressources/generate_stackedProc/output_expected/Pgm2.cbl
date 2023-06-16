      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2.
       data division.
       working-storage section.
       01  TC-PGM2-FctList-Loaded PIC X(02) VALUE space.
           88 TC-PGM2-FctList-IsLoaded      VALUE 'OK'.

       01 TC-PGM2-PntTab.
           05 TC-PGM2-PntNbr         PIC S9(04) COMP VALUE 2.
      *To call program b49bb8ce
      *Which is generated code for PGM2.GetTechnicalContext
      *Declared in source file Pgm2.tcbl
           05 TC-PGM2-b49bb8ce-Idt   PIC X(08) VALUE 'b49bb8ce'.
           05 TC-PGM2-b49bb8ce PROCEDURE-POINTER.
      *To call program a4ee502d
      *Which is generated code for PGM2.Proc1
      *Declared in source file Pgm2.tcbl
           05 TC-PGM2-a4ee502d-Idt   PIC X(08) VALUE 'a4ee502d'.
           05 TC-PGM2-a4ee502d PROCEDURE-POINTER.

       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.
       01 TC-A1 PIC X.

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
              SET TC-PGM2-b49bb8ce   TO ENTRY 'b49bb8ce'
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
           ENTRY 'b49bb8ce' USING TC-A1
               CALL "b49bb8ce" USING TC-A1
               GOBACK.

           ENTRY 'a4ee502d' USING TC-A1
               CALL "a4ee502d" USING TC-A1
               GOBACK.


      *declare procedure GetTechnicalContext public
      *    input myname1 pic X.

      *declare procedure Proc1 public
      *    input t pic x.
      *
      *declare procedure GetTechnicalContext public
      *    input myname1 pic X.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. b49bb8ce IS COMMON.
       END PROGRAM b49bb8ce.
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
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STACKED.
       DATA DIVISION.
                                                            
       WORKING-STORAGE SECTION.
       01  TC-STACKED-FctList-Loaded PIC X(02) VALUE space.
           88 TC-STACKED-FctList-IsLoaded      VALUE 'OK'.

       01 TC-STACKED-PntTab.
           05 TC-STACKED-PntNbr         PIC S9(04) COMP VALUE 2.
      *To call program c420cf71
      *Which is generated code for STACKED.Foo
      *Declared in source file Pgm2.tcbl
           05 TC-STACKED-c420cf71-Idt   PIC X(08) VALUE 'c420cf71'.
           05 TC-STACKED-c420cf71 PROCEDURE-POINTER.
      *To call program b4a83777
      *Which is generated code for STACKED.FooPgm2
      *Declared in source file Pgm2.tcbl
           05 TC-STACKED-b4a83777-Idt   PIC X(08) VALUE 'b4a83777'.
           05 TC-STACKED-b4a83777 PROCEDURE-POINTER.

       
       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.
       01 TC-A1 PIC X.

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
            IF NOT TC-STACKED-FctList-IsLoaded
              SET TC-STACKED-c420cf71   TO ENTRY 'c420cf71'
              SET TC-STACKED-b4a83777   TO ENTRY 'b4a83777'
              SET TC-STACKED-FctList-IsLoaded TO TRUE
            END-IF
               .

            set PntTab-Pnt TO ADDRESS OF TC-STACKED-PntTab

           .
                          
       PA-ALL-ENTRIES.
           ENTRY 'c420cf71' USING TC-A1
               CALL "c420cf71" USING TC-A1
               GOBACK.

           ENTRY 'b4a83777' USING TC-A1
               CALL "b4a83777" USING TC-A1
               GOBACK.


      *declare procedure Foo public
      *    input t pic x.
      *declare procedure FooPgm2 public
      *    input t pic x.
       END PROGRAM STACKED.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STACKED2.
       DATA DIVISION.
                                                             
       WORKING-STORAGE SECTION.
       01  TC-STACKED2-FctList-Loaded PIC X(02) VALUE space.
           88 TC-STACKED2-FctList-IsLoaded      VALUE 'OK'.

       01 TC-STACKED2-PntTab.
           05 TC-STACKED2-PntNbr         PIC S9(04) COMP VALUE 1.
      *To call program f22bfcb0
      *Which is generated code for STACKED2.Foo
      *Declared in source file Pgm2.tcbl
           05 TC-STACKED2-f22bfcb0-Idt   PIC X(08) VALUE 'f22bfcb0'.
           05 TC-STACKED2-f22bfcb0 PROCEDURE-POINTER.

       
       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.
       01 TC-A1 PIC X.

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
            IF NOT TC-STACKED2-FctList-IsLoaded
              SET TC-STACKED2-f22bfcb0   TO ENTRY 'f22bfcb0'
              SET TC-STACKED2-FctList-IsLoaded TO TRUE
            END-IF
               .

            set PntTab-Pnt TO ADDRESS OF TC-STACKED2-PntTab

           .
                          
       PA-ALL-ENTRIES.
           ENTRY 'f22bfcb0' USING TC-A1
               CALL "f22bfcb0" USING TC-A1
               GOBACK.


      *declare procedure Foo public
      *    input t pic x.
       END PROGRAM STACKED2.

