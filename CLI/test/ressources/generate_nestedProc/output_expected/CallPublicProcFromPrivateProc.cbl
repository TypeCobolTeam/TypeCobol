      *TypeCobol_Version:v0.0.0-local
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
       01  TC-PGM1-FctList-Loaded PIC X(02).
           88 TC-PGM1-FctList-IsLoaded      VALUE 'OK'.

       01 TC-PGM1-PntTab.
           05 TC-PGM1-PntNbr         PIC S9(04) COMP VALUE 2.
      *To call program a0508f35
      *Which is generated code for PGM1.check
      *Declared in source file CallPublicProcFromPrivateProc.rdz.tcbl
           05 TC-PGM1-a0508f35-Idt   PIC X(08) VALUE 'a0508f35'.
           05 TC-PGM1-a0508f35 PROCEDURE-POINTER.
      *To call program cd991005
      *Which is generated code for PersonService.GetPersonById
      *Declared in source file CallPublicProcFromPrivateProc.rdz.tcbl
           05 TC-PGM1-cd991005-Idt   PIC X(08) VALUE 'cd991005'.
           05 TC-PGM1-cd991005 PROCEDURE-POINTER.

       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.
       01 TC-A1 PIC X.


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
              SET TC-PGM1-cd991005   TO ENTRY 'cd991005'
              SET TC-PGM1-FctList-IsLoaded TO TRUE
            END-IF
               .

            set PntTab-Pnt TO ADDRESS OF TC-PGM1-PntTab

           .
                          

      *declare procedure check public
      *   input mydate        TYPE Date
      *  .

      *declare procedure checkName private
      *   input myname        PIC X(15)
      *  .

       INIT-LIBRARY.
           EXIT.

       TRAITEMENT.
      *OK  call check of PGM1
      *    call check input mydate1
           CALL 'a0508f35' USING
                                 mydate1
           end-call
                                   
           .
       PA-ALL-ENTRIES.
           ENTRY 'a0508f35' USING TC-A1
               CALL "a0508f35" USING TC-A1
               GOBACK.

           ENTRY 'cd991005' USING TC-A1
               CALL "cd991005" USING TC-A1
               GOBACK.



      *PersonService contains public procedure
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PersonService.
       PROCEDURE DIVISION.       
      *declare procedure GetPersonById public
      *   input  personId  type date.
       
      *declare procedure GetPersonByName private
      *   input  name  pic x(15).
      *
      *declare procedure GetPersonByName private
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
                               
       01 TC-PersonSe pic X(08) value 'PERSONSE'.

       01 TC-Call          PIC X     VALUE 'T'.
           88 TC-FirstCall  VALUE 'T'.
           88 TC-NthCall    VALUE 'F'
                            X'00' thru 'S'
                            'U' thru X'FF'.
       linkage section.
      *PGM1.check - Params :
      *     input(mydate: DATE)
                       
      *Common to all librairies used by the program.
       01 TC-Library-PntTab.
           05 TC-Library-PntNbr          PIC S9(04) COMP.
           05 TC-Library-Item OCCURS 1000
                               DEPENDING ON TC-Library-PntNbr
                               INDEXED   BY TC-Library-Idx.
              10 TC-Library-Item-Idt      PIC X(08).
              10 TC-Library-Item-Pnt      PROCEDURE-POINTER.

      *To call program cd991005 in module PersonService
      *Which is generated code for PersonService.GetPersonById
      *Declared in source file CallPublicProcFromPrivateProc.rdz.tcbl
       01 TC-PersonSe-cd991005-Item.
          05 TC-PersonSe-cd991005-Idt PIC X(08).
          05 TC-PersonSe-cd991005 PROCEDURE-POINTER.
       01 mydate.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       PROCEDURE DIVISION
             USING BY REFERENCE mydate
           .
      *PGM1.check - Params :
      *     input(mydate: DATE)
           PERFORM TC-INITIALIZATIONS
      *    Call PersonService::GetPersonById input mydate
           
           IF ADDRESS OF TC-PersonSe-cd991005-Item = NULL
             OR TC-PersonSe-cd991005-Idt not = 'cd991005'
               PERFORM TC-LOAD-POINTERS-PersonSe
           END-IF
      *    Equivalent to call cd991005 in module PersonService
           CALL TC-PersonSe-cd991005 USING
                                 mydate
           end-call
                                                         
           .
      *=================================================================
       TC-INITIALIZATIONS.
      *=================================================================
            IF TC-FirstCall
                 SET TC-NthCall TO TRUE
                 SET ADDRESS OF TC-PersonSe-cd991005-Item  TO NULL
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
                WHEN 'cd991005'
                     SET ADDRESS OF
                     TC-PersonSe-cd991005-Item
                     TO ADDRESS OF
                     TC-Library-Item(TC-Library-Idx)
                WHEN OTHER
                     CONTINUE
                END-EVALUATE
            END-PERFORM
            .
       END PROGRAM a0508f35.
      *
      *declare procedure checkName private
      *   input myname        PIC X(15)
      *  .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. a02a7aa5 IS COMMON.
       data division.
       working-storage section.
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
                               
       linkage section.
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
                       
       01 myname PIC X(15).
       PROCEDURE DIVISION
             USING BY REFERENCE myname
           .
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
           CONTINUE.
       END PROGRAM a02a7aa5.
      *
      *declare procedure GetPersonById public
      *   input  personId  type date.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cd991005 IS COMMON.
       DATA DIVISION.
       LINKAGE SECTION.
      *PGM1.GetPersonById - Params :
      *     input(personId: DATE)
       01 personId.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       PROCEDURE DIVISION
             USING BY REFERENCE personId
           .
      *PGM1.GetPersonById - Params :
      *     input(personId: DATE)
           CONTINUE.
       END PROGRAM cd991005.
       END PROGRAM PGM1.

