Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
      *TypeCobol_Version:[[ParserVersion]]
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
       LINKAGE SECTION.
       01 TC-FunctionCode pic X(30).
      * Function which call program a0508f35
      * Which is generated code for PGM1.check
           88 Fct-a0508f35-check
              value 'Fct=a0508f35-check'.
      * Function which call program cd991005
      * Which is generated code for PersonService.GetPersonById
           88 Fct-cd991005-GetPersonById
              value 'Fct=cd991005-GetPersonById'.
       01 arg1 PIC X.


       PROCEDURE DIVISiON USING TC-FunctionCode
                          arg1.

           PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

       FctList-Process-Mode.
           evaluate true
              when Fct-a0508f35-check
                 call 'a0508f35' using arg1
              when Fct-cd991005-GetPersonById
                 call 'cd991005' using arg1
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
                               
       01 TypeCobol-Generated.
           05 TC-PersonSe pic X(08) value 'PERSONSE'.
           05 PersonSe-Fct-cd991005-GetPerso PIC X(30)
               value 'Fct=cd991005-GetPersonById'.

       linkage section.
      *PGM1.check - Params :
      *     input(mydate: DATE)
                       
       01 mydate.
           02 YYYY PIC 9(4).
           02 MM PIC 9(2).
           02 DD PIC 9(2).
       PROCEDURE DIVISION
             USING BY REFERENCE mydate
           .
      *PGM1.check - Params :
      *     input(mydate: DATE)
      *    Call PersonService::GetPersonById input mydate
           CALL TC-PersonSe USING
                    PersonSe-Fct-cd991005-GetPerso
                                 mydate
           end-call
                                                         
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

