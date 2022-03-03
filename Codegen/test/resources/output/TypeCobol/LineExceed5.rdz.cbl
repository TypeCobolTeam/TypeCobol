Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM1.
       DATA DIVISION.
                                                         
       WORKING-STORAGE SECTION.

       
       LINKAGE SECTION.
       01 TC-FunctionCode pic X(30).
      * Function which call program f1c0385c
      * Which is generated code for PGM1.StartCheckpoint
           88 Fct-f1c0385c-StartCheckpoint
              value 'Fct=f1c0385c-StartCheckpoint'.

       01 arg1 pic X.
       PROCEDURE DIVISION USING TC-FunctionCode
                           arg1.
           PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

       FctList-Process-Mode.
           evaluate true

              when Fct-f1c0385c-StartCheckpoint
                 call 'f1c0385c' using arg1
              when other
                 Perform Handle-Error
           end-evaluate
           .
       Handle-Error.
           continue
           .
                                
      *-----------------------------------------------------------------
      *declare procedure StartCheckpoint public
      *        input param1 pic X.
       END PROGRAM PGM1.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2.
       DATA DIVISION.
                                                         
       WORKING-STORAGE SECTION.

       
       LINKAGE SECTION.
       01 TC-FunctionCode pic X(30).
      * Function which call program f73481e6
      * Which is generated code for PGM2.CheckContract
           88 Fct-f73481e6-CheckContract
              value 'Fct=f73481e6-CheckContract'.

       01 arg1 pic X.
       PROCEDURE DIVISION USING TC-FunctionCode
                           arg1.
           PERFORM INIT-LIBRARY
           PERFORM FctList-Process-Mode
           GOBACK.

       FctList-Process-Mode.
           evaluate true

              when Fct-f73481e6-CheckContract
                 call 'f73481e6' using arg1
              when other
                 Perform Handle-Error
           end-evaluate
           .
       Handle-Error.
           continue
           .
                                
      *-----------------------------------------------------------------
      *declare procedure CheckContract public
      *        input param1 pic X.
       END PROGRAM PGM2.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       PROCEDURE DIVISION.      
      *declare procedure testos private.
       END PROGRAM MyPGM.
      *
      *declare procedure StartCheckpoint public
      *        input param1 pic X.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. f1c0385c.
       END PROGRAM f1c0385c.


      *
      *declare procedure CheckContract public
      *        input param1 pic X.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. f73481e6.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *PGM1.CheckContract - Params :
      *     input(param1: pic X)
       01 TypeCobol-Generated.
           05 TC-PGM1 pic X(08) value 'PGM1'.
           05 PGM1-Fct-f1c0385c-StartCheckpo PIC X(30)
               value 'Fct=f1c0385c-StartCheckpoint'.

       LINKAGE SECTION.
      *PGM1.CheckContract - Params :
      *     input(param1: pic X)
       01 param1 pic X.
       PROCEDURE DIVISION
             USING BY REFERENCE param1
           .
      *PGM1.CheckContract - Params :
      *     input(param1: pic X)
      *    call PGM1::StartCheckpoint input param1
           CALL 'zcallpgm' using TC-PGM1
                    PGM1-Fct-f1c0385c-StartCheckpo
                                 param1
           end-call
                                                  
           .
       END PROGRAM f73481e6.


      *
      *declare procedure testos private.
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bfc74757.
       data division.
       working-storage section.
      *PGM1.testos  - No Params
                               
       01 param1 pic X.
       01 TypeCobol-Generated.
           05 TC-PGM1 pic X(08) value 'PGM1'.
           05 PGM1-Fct-f1c0385c-StartCheckpo PIC X(30)
               value 'Fct=f1c0385c-StartCheckpoint'.

       LINKAGE SECTION.
      *PGM1.testos  - No Params
       PROCEDURE DIVISION
           .
      *PGM1.testos  - No Params
      *                        call PGM1::StartCheckpoint input param1
                               CALL 'zcallpgm' using TC-PGM1
                    PGM1-Fct-f1c0385c-StartCheckpo
                                 param1
           end-call
                                                                      
           .
       END PROGRAM bfc74757.


