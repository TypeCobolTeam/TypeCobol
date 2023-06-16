      * 1 errors
      * Line 78[8,18] <37, Warning, General> - Warning: Paragraph 'TRAITEMENT' is empty
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM1.

       DATA DIVISION.
       Working-STORAGE SECTION.

      *01  mydate1     TYPE Date.
       01 mydate1.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                 
       01  myname1      PIC X(15).
       01  TC-PGM1-FctList-Loaded PIC X(02) VALUE space.
           88 TC-PGM1-FctList-IsLoaded      VALUE 'OK'.

       01 TC-PGM1-PntTab.
           05 TC-PGM1-PntNbr         PIC S9(04) COMP VALUE 4.
      *To call program a0508f35
      *Which is generated code for PGM1.check
      *Declared in source file FunDeclareWithExec.rdz.cbl
           05 TC-PGM1-a0508f35-Idt   PIC X(08) VALUE 'a0508f35'.
           05 TC-PGM1-a0508f35 PROCEDURE-POINTER.
      *To call program efd9419f
      *Which is generated code for PGM1.check
      *Declared in source file FunDeclareWithExec.rdz.cbl
           05 TC-PGM1-efd9419f-Idt   PIC X(08) VALUE 'efd9419f'.
           05 TC-PGM1-efd9419f PROCEDURE-POINTER.
      *To call program a02a7aa5
      *Which is generated code for PGM1.checkName
      *Declared in source file FunDeclareWithExec.rdz.cbl
           05 TC-PGM1-a02a7aa5-Idt   PIC X(08) VALUE 'a02a7aa5'.
           05 TC-PGM1-a02a7aa5 PROCEDURE-POINTER.
      *To call program f6b6da00
      *Which is generated code for PersonService.GetPersonByName
      *Declared in source file FunDeclareWithExec.rdz.cbl
           05 TC-PGM1-f6b6da00-Idt   PIC X(08) VALUE 'f6b6da00'.
           05 TC-PGM1-f6b6da00 PROCEDURE-POINTER.

       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.
       01 TC-A1 PIC X.
       01 TC-A2 PIC X.


       PROCEDURE DIVISiON USING PntTab-Pnt.
                          
      *
      *    IF CallIsCopy
      *      PERFORM Copy-Process-Mode
      *    ELSE
           PERFORM FctList-Process-Mode
           perform INIT-LIBRARY
      *    END-IF

           GOBACK.

        FctList-Process-Mode.
            IF NOT TC-PGM1-FctList-IsLoaded
              SET TC-PGM1-a0508f35   TO ENTRY 'a0508f35'
              SET TC-PGM1-efd9419f   TO ENTRY 'efd9419f'
              SET TC-PGM1-a02a7aa5   TO ENTRY 'a02a7aa5'
              SET TC-PGM1-f6b6da00   TO ENTRY 'f6b6da00'
              SET TC-PGM1-FctList-IsLoaded TO TRUE
            END-IF
               .

            set PntTab-Pnt TO ADDRESS OF TC-PGM1-PntTab

           .
                          

      *declare procedure check public
      *   input mydate        TYPE Date
      *  .

      *declare procedure check public
      *   input mydate        TYPE Date
      *   output r1           PIC X(5)
      *  .
       
      *declare procedure checkName public
      *   input myname        PIC X(15)
      *  .
           
       INIT-LIBRARY.
            EXIT.
       
       TRAITEMENT.
       PA-ALL-ENTRIES.
           ENTRY 'a0508f35' USING TC-A1
               CALL "a0508f35" USING TC-A1
               GOBACK.

           ENTRY 'efd9419f' USING TC-A1 TC-A2
               CALL "efd9419f" USING TC-A1 TC-A2
               GOBACK.

           ENTRY 'a02a7aa5' USING TC-A1
               CALL "a02a7aa5" USING TC-A1
               GOBACK.

           ENTRY 'f6b6da00' USING TC-A1
               CALL "f6b6da00" USING TC-A1
               GOBACK.


      *OK  call check of PGM1
      *   call check input mydate1
      *    .

      *PersonService contains public procedure
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PersonService.
       
       
       PROCEDURE DIVISION.       
      *declare procedure GetPersonById private
      *   input  personId  type date.
       
      *declare procedure GetPersonByName public
      *   input  name  pic x(15).
       
       INIT-LIBRARY.
            EXIT.
       
      *
      *declare procedure GetPersonById private
      *   input  personId  type date.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cd991005 IS COMMON.
          
       DATA DIVISION.
       LINKAGE SECTION.
      *PGM1.GetPersonById - Params :
      *     input(personId: DATE)
                       
       01 ERIM04-LOT-SOC                 PIC X(5).
       01 LOT-SOC                 PIC X(5).
       01 personId.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       PROCEDURE DIVISION
             USING BY REFERENCE personId
           .
      *PGM1.GetPersonById - Params :
      *     input(personId: DATE)
           EXEC SQL
             SELECT
               LOT_SOC              
       
       
             INTO
               :LOT-SOC             
       
             FROM TERILOT
       
             WHERE LOT_SOC             = :ERIM04-LOT-SOC
       
           END-EXEC  
           CONTINUE.
       END PROGRAM cd991005.
       END PROGRAM PersonService.


      *
      *declare procedure check public
      *   input mydate        TYPE Date
      *  .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. a0508f35 IS COMMON.
       data division.
       working-storage section.
      *PGM1.check - Params :
      *     input(mydate: DATE)
                               
       LINKAGE SECTION.
      *PGM1.check - Params :
      *     input(mydate: DATE)
                       
       01 ERIM04-LOT-SOC                 PIC X(5).
       01 LOT-SOC                 PIC X(5).
       01 mydate.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       PROCEDURE DIVISION
             USING BY REFERENCE mydate
           .
      *PGM1.check - Params :
      *     input(mydate: DATE)
           EXEC SQL
             SELECT
               LOT_SOC              
       
       
             INTO
               :LOT-SOC             
       
             FROM TERILOT
       
             WHERE LOT_SOC             = :ERIM04-LOT-SOC
       
           END-EXEC  
           CONTINUE.
       END PROGRAM a0508f35.
      *
      *declare procedure check public
      *   input mydate        TYPE Date
      *   output r1           PIC X(5)
      *  .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. efd9419f IS COMMON.
       data division.
       working-storage section.
      *PGM1.check - Params :
      *     input(mydate: DATE)
      *     output(r1: pic X(5))
                               
       LINKAGE SECTION.
      *PGM1.check - Params :
      *     input(mydate: DATE)
      *     output(r1: pic X(5))
                       
       01 ERIM04-LOT-SOC                 PIC X(5).
       01 LOT-SOC                 PIC X(5).
       01 mydate.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       01 r1 PIC X(5).
       PROCEDURE DIVISION
             USING BY REFERENCE mydate
                   BY REFERENCE r1
           .
      *PGM1.check - Params :
      *     input(mydate: DATE)
      *     output(r1: pic X(5))
           DISPLAY "P1"
           EXEC SQL
             SELECT
               LOT_SOC              
       
       
             INTO
               :LOT-SOC             
       
             FROM TERILOT
       
             WHERE LOT_SOC             = :ERIM04-LOT-SOC
       
           END-EXEC  
           CONTINUE.
       END PROGRAM efd9419f.
      *
      *declare procedure checkName public
      *   input myname        PIC X(15)
      *  .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. a02a7aa5 IS COMMON.
       data division.
       working-storage section.
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
                               
       01 TC-PersonSe pic X(08) value 'PERSONSE'.

       01 TC-Call          PIC X     VALUE 'T'.
           88 TC-FirstCall  VALUE 'T'.
           88 TC-NthCall    VALUE 'F'
                            X'00' thru 'S'
                            'U' thru X'FF'.
       linkage section.
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
                       
      *Common to all librairies used by the program.
       01 TC-Library-PntTab.
           05 TC-Library-PntNbr          PIC S9(04) COMP.
           05 TC-Library-Item OCCURS 1000
                               DEPENDING ON TC-Library-PntNbr
                               INDEXED   BY TC-Library-Idx.
              10 TC-Library-Item-Idt      PIC X(08).
              10 TC-Library-Item-Pnt      PROCEDURE-POINTER.

      *To call program f6b6da00 in module PersonService
      *Which is generated code for PersonService.GetPersonByName
      *Declared in source file FunDeclareWithExec.rdz.cbl
       01 TC-PersonSe-f6b6da00-Item.
          05 TC-PersonSe-f6b6da00-Idt PIC X(08).
          05 TC-PersonSe-f6b6da00 PROCEDURE-POINTER.
       01 myname PIC X(15).
       PROCEDURE DIVISION
             USING BY REFERENCE myname
           .
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
           PERFORM TC-INITIALIZATIONS
      *    Call PersonService::GetPersonByName input myname
           
           IF ADDRESS OF TC-PersonSe-f6b6da00-Item = NULL
             OR TC-PersonSe-f6b6da00-Idt not = 'f6b6da00'
               PERFORM TC-LOAD-POINTERS-PersonSe
           END-IF
      *    Equivalent to call f6b6da00 in module PersonService
           CALL TC-PersonSe-f6b6da00 USING
                                 myname
           end-call
                                                           
           .
      *=================================================================
       TC-INITIALIZATIONS.
      *=================================================================
            IF TC-FirstCall
                 SET TC-NthCall TO TRUE
                 SET ADDRESS OF TC-PersonSe-f6b6da00-Item  TO NULL
            END-IF
            .
      *=================================================================
       TC-LOAD-POINTERS-PersonSe.
      *=================================================================
            CALL 'ZCALLPGM' USING TC-PersonSe
            ADDRESS OF TC-Library-PntTab
            PERFORM VARYING TC-Library-Idx FROM 1 BY 1
            UNTIL TC-Library-Idx > TC-Library-PntNbr
                EVALUATE TC-Library-Item-Idt (TC-Library-Idx)
                WHEN 'f6b6da00'
                     SET ADDRESS OF
                     TC-PersonSe-f6b6da00-Item
                     TO ADDRESS OF
                     TC-Library-Item(TC-Library-Idx)
                WHEN OTHER
                     CONTINUE
                END-EVALUATE
            END-PERFORM
            .
       END PROGRAM a02a7aa5.
      *
      *declare procedure GetPersonByName public
      *   input  name  pic x(15).
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. f6b6da00 IS COMMON.
       DATA DIVISION.
       LINKAGE SECTION.
      *PGM1.GetPersonByName - Params :
      *     input(name: pic x(15))
       01 name pic x(15).
       PROCEDURE DIVISION
             USING BY REFERENCE name
           .
      *PGM1.GetPersonByName - Params :
      *     input(name: pic x(15))
           CONTINUE.
       END PROGRAM f6b6da00.
       END PROGRAM PGM1.


