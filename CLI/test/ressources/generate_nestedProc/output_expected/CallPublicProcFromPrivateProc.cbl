      *TypeCobol_Version:0.1(alpha)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM1.

       DATA DIVISION.
       Working-STORAGE SECTION.

      *01  mydate1     TYPE Date.
       01 mydate1.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                                 
       01  TC-PGM1-FctList-Loaded PIC X(02).
           88 TC-PGM1-FctList-IsLoaded      VALUE 'OK'.
       01 TC-PGM1-PntTab.
           05 TC-PGM1-PntNbr         PIC S9(04) COMP VALUE 2.
      *To call program cd991005GetPersonById
      *Which is generated code for PersonService.GetPersonById
      *Declared in source file CallPublicProcFromPrivateProc.rdz.tcbl
           05 TC-PGM1-cd991005-Idt   PIC X(08) VALUE 'cd991005'.
           05 TC-PGM1-cd991005 PROCEDURE-POINTER.
      *To call program f6b6da00GetPersonByName
      *Which is generated code for PersonService.GetPersonByName
      *Declared in source file CallPublicProcFromPrivateProc.rdz.tcbl
           05 TC-PGM1-f6b6da00-Idt   PIC X(08) VALUE 'f6b6da00'.
           05 TC-PGM1-f6b6da00 PROCEDURE-POINTER.

       LINKAGE SECTION.
       01 PntTab-Pnt POINTER.
       01 TC-A1 PIC X.


       PROCEDURE DIVISiON USING PntTab-Pnt.
                              .
                          

      *declare procedure check private
      *   input mydate        TYPE Date
      *  .

       INIT-LIBRARY.
       
       TRAITEMENT.
      *OK  call check of PGM1
      *    call check input mydate1
           CALL 'a0508f35check' USING
                                 mydate1
           end-call
                                   
           .
       PA-ALL-ENTRIES.
           ENTRY 'cd991005' USING TC-A1
               CALL "cd991005GetPersonById" USING TC-A1
               GOBACK.

           ENTRY 'f6b6da00' USING TC-A1
               CALL "f6b6da00GetPersonByName" USING TC-A1
               GOBACK.



      *PersonService contains public procedure
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PersonService.
       PROCEDURE DIVISION.       
      *declare procedure GetPersonById public
      *   input  personId  type date.
       
      *declare procedure GetPersonByName public
      *   input  name  pic x(15).
       END PROGRAM PersonService.


      *
      *declare procedure check private
      *   input mydate        TYPE Date
      *  .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. a0508f35check.
       data division.
       working-storage section.
      *PGM1.check - Params :
      *		input(mydate: DATE)
                               
       01 TC-PersonSe pic X(08) value 'PERSONSE'.

       01 TC-Call          PIC X     VALUE 'T'.
           88 TC-FirstCall  VALUE 'T'.
           88 TC-NthCall    VALUE 'F'
                            X'00' thru 'S'
                            'U' thru X'FF'.
       linkage section.
      *PGM1.check - Params :
      *		input(mydate: DATE)
                       
      *Common to all librairies used by the program.
       01 TC-Library-PntTab.
           05 TC-Library-PntNbr          PIC S9(04) COMP.
           05 TC-Library-Item OCCURS 1000
                               DEPENDING ON TC-Library-PntNbr
                               INDEXED   BY TC-Library-Idx.
              10 TC-Library-Item-Idt      PIC X(08).
              10 TC-Library-Item-Pnt      PROCEDURE-POINTER.

      *To call program cd991005GetPersonById in module PersonService
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
      *		input(mydate: DATE)
           PERFORM TC-INITIALIZATIONS
      *    Call PersonService::GetPersonById input mydate
           CALL 'cd991005GetPersonById' USING
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
       END PROGRAM a0508f35check.
      *
      *declare procedure GetPersonById public
      *   input  personId  type date.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cd991005GetPersonById IS COMMON.
       DATA DIVISION.
       LINKAGE SECTION.
      *PGM1.GetPersonById - Params :
      *		input(personId: DATE)
       01 personId.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       PROCEDURE DIVISION
             USING BY REFERENCE personId
           .
      *PGM1.GetPersonById - Params :
      *		input(personId: DATE)
           CONTINUE.
       END PROGRAM cd991005GetPersonById.
      *
      *declare procedure GetPersonByName public
      *   input  name  pic x(15).
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. f6b6da00GetPersonByName IS COMMON.
       DATA DIVISION.
       LINKAGE SECTION.
      *PGM1.GetPersonByName - Params :
      *		input(name: pic x(15))
       01 name pic x(15).
       PROCEDURE DIVISION
             USING BY REFERENCE name
           .
      *PGM1.GetPersonByName - Params :
      *		input(name: pic x(15))
           CONTINUE.
       END PROGRAM f6b6da00GetPersonByName.
       END PROGRAM PGM1.

