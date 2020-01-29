      *TypeCobol_Version:0.1(alpha)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2.
       data division.
       working-storage section.
       01  TC-PGM2-FctList-Loaded PIC X(02).
           88 TC-PGM2-FctList-IsLoaded      VALUE 'OK'.
       01 TC-PGM2-PntTab.
           05 TC-PGM2-PntNbr         PIC S9(04) COMP VALUE 1.
      *To call program b49bb8ce
      *Which is generated code for PGM2.GetTechnicalContext
      *Declared in source file Pgm2.tcbl
           05 TC-PGM2-b49bb8ce-Idt   PIC X(08) VALUE 'b49bb8ce'.
           05 TC-PGM2-b49bb8ce PROCEDURE-POINTER.

       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.

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

              SET TC-PGM2-FctList-IsLoaded TO TRUE
            END-IF
               .

            set PntTab-Pnt TO ADDRESS OF TC-PGM2-PntTab

           .
                          
       INIT-LIBRARY.
           continue.
           .
      *declare procedure GetTechnicalContext public
      *    input myname1 pic X.
       END PROGRAM PGM2.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STACKED.
       DATA DIVISION.
                                                            
       WORKING-STORAGE SECTION.
       01  TC-STACKED-FctList-Loaded PIC X(02).
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
                          
      *declare procedure Foo public
      *    input t pic x.
      *declare procedure FooPgm2 public
      *    input t pic x.
       END PROGRAM STACKED.
      *
      *declare procedure GetTechnicalContext public
      *    input myname1 pic X.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. b49bb8ce.
       END PROGRAM b49bb8ce.
      *
      *declare procedure Foo public
      *    input t pic x.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. c420cf71.
       DATA DIVISION.
       LINKAGE SECTION.
      *PGM2.Foo - Params :
      *     input(t: pic x)
       01 t pic x.
       PROCEDURE DIVISION
             USING BY REFERENCE t
           .
      *PGM2.Foo - Params :
      *     input(t: pic x)
           .
       END PROGRAM c420cf71.
      *
      *declare procedure FooPgm2 public
      *    input t pic x.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. b4a83777.
       DATA DIVISION.
       LINKAGE SECTION.
      *PGM2.FooPgm2 - Params :
      *     input(t: pic x)
       01 t pic x.
       PROCEDURE DIVISION
             USING BY REFERENCE t
           .
      *PGM2.FooPgm2 - Params :
      *     input(t: pic x)
           .
       END PROGRAM b4a83777.
