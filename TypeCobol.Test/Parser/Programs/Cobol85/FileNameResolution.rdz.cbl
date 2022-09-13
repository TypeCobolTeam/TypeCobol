       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MyPGM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-SOR003         ASSIGN TO   SOR003
      *OK can reuse same name as file for file status
                                   FILE STATUS F-SOR003.
       DATA DIVISION.
       FILE SECTION.

       FD  F-SOR003
           BLOCK CONTAINS 0
           RECORDING MODE F.
       01  Group1.
           05 Var1                   PIC X(06).

       PROCEDURE DIVISION.
      *OK qualify variable name with file name
           MOVE "A" TO Var1 IN F-SOR003
      *KO "F-SOR003"was defined as a type that was invalid in this
      *context.  The statement was discarded.
           move "A" to F-SOR003
      *Ok
           write Group1 in F-SOR003
      *Ok
           call "foo" using F-SOR003
           .
       END PROGRAM MyPGM.