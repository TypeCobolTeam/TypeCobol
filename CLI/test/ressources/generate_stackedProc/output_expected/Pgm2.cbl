Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2.
       data division.
       working-storage section.
       01 TC-FunctionCode pic X(30).
      * Function which call program b49bb8ce
      * Which is generated code for PGM2.GetTechnicalContext
           88 Fct-b49bb8ce-GetTechnicalContext
              value 'Fct=b49bb8ce-GetTechnicalContext'.
      * Function which call program a4ee502d
      * Which is generated code for PGM2.Proc1
           88 Fct-a4ee502d-Proc1
              value 'Fct=a4ee502d-Proc1'.

       LINKAGE SECTION.
       01 FunctionCode pic X(30).
       01 arg1 PIC X.

       PROCEDURE DIVISION USING TC-FunctionCode
                          arg1.

           PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

       FctList-Process-Mode.
           evaluate true
              when Fct-b49bb8ce-GetTechnicalContext
                 call 'b49bb8ce' using arg1
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
       01 TC-FunctionCode pic X(30).
      * Function which call program c420cf71
      * Which is generated code for STACKED.Foo
           88 Fct-c420cf71-Foo
              value 'Fct=c420cf71-Foo'.
      * Function which call program b4a83777
      * Which is generated code for STACKED.FooPgm2
           88 Fct-b4a83777-FooPgm2
              value 'Fct=b4a83777-FooPgm2'.

       
       LINKAGE SECTION.
       01 FunctionCode pic X(30).
       01 arg1 PIC X.

       PROCEDURE DIVISION USING TC-FunctionCode
                          arg1.

           PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

       FctList-Process-Mode.
           evaluate true
              when Fct-c420cf71-Foo
                 call 'c420cf71' using arg1
              when Fct-b4a83777-FooPgm2
                 call 'b4a83777' using arg1
              when other
                 Perform Handle-Error
           end-evaluate
               .
       Handle-Error.
           continue
           .
                          
      *declare procedure Foo public
      *    input t pic x.
      *declare procedure FooPgm2 public
      *    input t pic x.
       END PROGRAM STACKED.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STACKED2.
       DATA DIVISION.
                                                             
       WORKING-STORAGE SECTION.
       01 TC-FunctionCode pic X(30).
      * Function which call program f22bfcb0
      * Which is generated code for STACKED2.Foo
           88 Fct-f22bfcb0-Foo
              value 'Fct=f22bfcb0-Foo'.

       
       LINKAGE SECTION.
       01 FunctionCode pic X(30).
       01 arg1 PIC X.

       PROCEDURE DIVISION USING TC-FunctionCode
                          arg1.

           PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

       FctList-Process-Mode.
           evaluate true
              when Fct-f22bfcb0-Foo
                 call 'f22bfcb0' using arg1
              when other
                 Perform Handle-Error
           end-evaluate
               .
       Handle-Error.
           continue
           .
                          
      *declare procedure Foo public
      *    input t pic x.
       END PROGRAM STACKED2.
