       IDENTIFICATION DIVISION.
       PROGRAM-ID. tcMove.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC X.
       01 var2 PIC X.
       PROCEDURE DIVISION.
      *OK
           MOVE var1 TO var2.
      *KO unsupported MOVE on boolean values
           MOVE TRUE TO var2.
           MOVE FALSE TO var2.
      *KO invalid UNSAFE keyword
           MOVE UNSAFE var1 TO var2.
           MOVE UNSAFE TRUE TO var2.
           MOVE UNSAFE FALSE TO var2.
       END PROGRAM tcMove.