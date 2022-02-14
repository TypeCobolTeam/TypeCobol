      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2.
       data division.
       working-storage section.
       01  TC-PGM2-FctList-Loaded PIC X(02).
           88 TC-PGM2-FctList-IsLoaded      VALUE 'OK'.

       01 TC-FunctionCode pic X(30).
      * Function which call program b49bb8ce
      * Which is generated code for PGM2.GetTechnicalContext
           08 Fct-b49bb8ce-GetTechnicalContext
              value 'Fct=b49bb8ce-GetTechnicalContext'.
      * Function which call program a4ee502d
      * Which is generated code for PGM2.Proc1
           08 Fct-a4ee502d-Proc1
              value 'Fct=a4ee502d-Proc1'.

       LINKAGE SECTION.
       01 FunctionCode pic X(30).
       01 arg1 PIC X.

       PROCEDURE DIVISION USING TC-FunctionCode
                          
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
       01  TC-STACKED-FctList-Loaded PIC X(02).
           88 TC-STACKED-FctList-IsLoaded      VALUE 'OK'.

       01 TC-FunctionCode pic X(30).
      * Function which call program c420cf71
      * Which is generated code for STACKED.Foo
           08 Fct-c420cf71-Foo
              value 'Fct=c420cf71-Foo'.
      * Function which call program b4a83777
      * Which is generated code for STACKED.FooPgm2
           08 Fct-b4a83777-FooPgm2
              value 'Fct=b4a83777-FooPgm2'.

       
       LINKAGE SECTION.
       01 FunctionCode pic X(30).
       01 arg1 PIC X.

       PROCEDURE DIVISION USING TC-FunctionCode
                          
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
       01  TC-STACKED2-FctList-Loaded PIC X(02).
           88 TC-STACKED2-FctList-IsLoaded      VALUE 'OK'.

       01 TC-FunctionCode pic X(30).
      * Function which call program f22bfcb0
      * Which is generated code for STACKED2.Foo
           08 Fct-f22bfcb0-Foo
              value 'Fct=f22bfcb0-Foo'.

       
       LINKAGE SECTION.
       01 FunctionCode pic X(30).
       01 arg1 PIC X.

       PROCEDURE DIVISION USING TC-FunctionCode
                          
           .
                          
       PA-ALL-ENTRIES.
           ENTRY 'f22bfcb0' USING TC-A1
               CALL "f22bfcb0" USING TC-A1
               GOBACK.


      *declare procedure Foo public
      *    input t pic x.
       END PROGRAM STACKED2.
