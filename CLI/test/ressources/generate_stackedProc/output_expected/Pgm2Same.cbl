Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2.
       DATA DIVISION.
                                                         
       WORKING-STORAGE SECTION.
       01  TC-PGM2-FctList-Loaded PIC X(02).
           88 TC-PGM2-FctList-IsLoaded      VALUE 'OK'.

       01 TC-FunctionCode pic X(30).
      * Function which call program a4ee502d
      * Which is generated code for PGM2.Proc1
           88 Fct-a4ee502d-Proc1
              value 'Fct=a4ee502d-Proc1'.

       
       LINKAGE SECTION.
       01 FunctionCode pic X(30).
       01 arg1 PIC X.

      * same name PGM2
       PROCEDURE DIVISION USING TC-FunctionCode
                          arg1.




                   PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

        FctList-Process-Mode.
           evaluate true
               when Fct-a4ee502d-Proc1
                  call 'a4ee502d' using arg1
               when other
                 Perform Handle-Error
           end-evaluate

               .
       Handle-Error.
           continue
           .
                          
       INIT-LIBRARY.
           continue.
           .
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
