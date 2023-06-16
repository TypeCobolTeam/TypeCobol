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
           05 TC-PGM1-PntNbr         PIC S9(04) COMP VALUE 3.
      *To call program a0508f35
      *Which is generated code for PGM1.check
      *Declared in source file FunDeclareWithExec-PublicOnly.rdz.cbl
           05 TC-PGM1-a0508f35-Idt   PIC X(08) VALUE 'a0508f35'.
           05 TC-PGM1-a0508f35 PROCEDURE-POINTER.
      *To call program efd9419f
      *Which is generated code for PGM1.check
      *Declared in source file FunDeclareWithExec-PublicOnly.rdz.cbl
           05 TC-PGM1-efd9419f-Idt   PIC X(08) VALUE 'efd9419f'.
           05 TC-PGM1-efd9419f PROCEDURE-POINTER.
      *To call program a02a7aa5
      *Which is generated code for PGM1.checkName
      *Declared in source file FunDeclareWithExec-PublicOnly.rdz.cbl
           05 TC-PGM1-a02a7aa5-Idt   PIC X(08) VALUE 'a02a7aa5'.
           05 TC-PGM1-a02a7aa5 PROCEDURE-POINTER.

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
                       
       01 ERIM04-LOT-SOC          PIC X(5).
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
             SELECT LOT_SOC              
             INTO :LOT-SOC             
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
                       
       01 ERIM04-LOT-SOC          PIC X(5).
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
             SELECT LOT_SOC              
             INTO :LOT-SOC             
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
                               
      *01 var1 type Date.
       01 var1.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
                         
       linkage section.
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
                       
       01 myname PIC X(15).
       PROCEDURE DIVISION
             USING BY REFERENCE myname
           .
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
      *    Call check input var1
           CALL 'a0508f35' USING
                                 var1
           end-call
                                
           .
       END PROGRAM a02a7aa5.
       END PROGRAM PGM1.


