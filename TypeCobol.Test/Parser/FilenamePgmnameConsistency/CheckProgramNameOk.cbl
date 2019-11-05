       IDENTIFICATION DIVISION.
       PROGRAM-ID. CheckProgramNameOk.
       PROCEDURE DIVISION.
           GOBACK.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NestedPgm IS COMMON.
       DATA DIVISION.
       LINKAGE SECTION.
       01 x PIC X.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM NestedPgm.
      
       END PROGRAM CheckProgramNameOk.
      