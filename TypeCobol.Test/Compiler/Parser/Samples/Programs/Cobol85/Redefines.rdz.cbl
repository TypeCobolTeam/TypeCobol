       IDENTIFICATION DIVISION.
       PROGRAM-ID. RedefinesTest.

       DATA DIVISION.
       WORKING-storage section.
       01 MyVar1.
           05 MyVar2 PIC X.

      * OK Redefines works - Cobol is case insensitive
       01 MyVar2 PIC X(05).
       01 MyVar2 PIC X(09).
       01 MyRedifines REDEFINES MyVar2.
       01 MyRedifines2 REDEFINES MyVar2.
       01 MyRedifines3 REDEFINES myvar2.
       01 MyRedifines4 REDEFINES MyVar2.
       01 REDEFINES MYVAR2.

      * KO Redefines could not find MyVar1
       01 MyRedifines3 REDEFINES MyVar1.

      * OK Redefines works
       01 .
            06 VarGroup PIC X.
            06 VarGroup-Bis REDEFINES VarGroup PIC X.
            06 FILTER REDEFINES VarGroup-Bis PIC X.


       PROCEDURE DIVISION.
       .

       END PROGRAM RedefinesTest.