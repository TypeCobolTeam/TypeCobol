      * 1 errors
      * Line 78[8,18] <37, Warning, General> - Warning: Paragraph 'TRAITEMENT' is empty
Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
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

       01 TC-FunctionCode pic X(30).
      * Function which call program a0508f35
      * Which is generated code for PGM1.check
           88 Fct-a0508f35-check
              value 'Fct=a0508f35-check'.
      * Function which call program efd9419f
      * Which is generated code for PGM1.check
           88 Fct-efd9419f-check
              value 'Fct=efd9419f-check'.
      * Function which call program a02a7aa5
      * Which is generated code for PGM1.checkName
           88 Fct-a02a7aa5-checkName
              value 'Fct=a02a7aa5-checkName'.
      * Function which call program f6b6da00
      * Which is generated code for PersonService.GetPersonByName
           88 Fct-f6b6da00-GetPersonByName
              value 'Fct=f6b6da00-GetPersonByName'.

       LINKAGE SECTION.
       01 FunctionCode pic X(30).
       01 arg1 PIC X.
       01 arg2 PIC X.


       PROCEDURE DIVISiON USING TC-FunctionCode
                          arg1
                          arg2.




                   PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

        FctList-Process-Mode.
           evaluate true
               when Fct-a0508f35-check
                  call 'a0508f35' using arg1
               when Fct-efd9419f-check
                  call 'efd9419f' using arg1
                                        arg2
               when Fct-a02a7aa5-checkName
                  call 'a02a7aa5' using arg1
               when Fct-f6b6da00-GetPersonByName
                  call 'f6b6da00' using arg1
               when other
                 Perform Handle-Error
           end-evaluate

               .
       Handle-Error.
           continue
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
                               
       01 TypeCobol-Generated.
           05 TC-PersonSe pic X(08) value 'PERSONSE'.
           05 PersonSe-Fct-f6b6da00-GetPerso PIC X(30)
               value 'Fct=f6b6da00-GetPersonByName'.

       linkage section.
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
                       
       01 myname PIC X(15).
       PROCEDURE DIVISION
             USING BY REFERENCE myname
           .
      *PGM1.checkName - Params :
      *     input(myname: pic X(15))
      *    Call PersonService::GetPersonByName input myname
           CALL 'zcallpgm' using TC-PersonSe
                    PersonSe-Fct-f6b6da00-GetPerso
                                 myname
           end-call
                                                           
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

