      *TypeCobol_Version:0.1(alpha)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2.
       DATA DIVISION.
                                                         
       WORKING-STORAGE SECTION.
       01  TC-PGM2-FctList-Loaded PIC X(02).
           88 TC-PGM2-FctList-IsLoaded      VALUE 'OK'.

       01 TC-FunctionCode pic X(30).
      * Function which call program a4ee502d
      * Which is generated code for PGM2.Proc1
           08 Fct-a4ee502d-Proc1
              value 'Fct=a4ee502d-Proc1'.

       
       LINKAGE SECTION.
       01 FunctionCode pic X(30).
       01 arg1 PIC X.

      * same name PGM2
       PROCEDURE DIVISION USING TC-FunctionCode
                          
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
