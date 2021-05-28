       IDENTIFICATION DIVISION.
       PROGRAM-ID. tcSet.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC X.
       01 p-var1 POINTER.
       01 tab.
          05 item OCCURS 10 INDEXED BY idx.
             10 element PIC X.
       01 var2 PIC 9.
       01 varCondition PIC X.
          88 test1 VALUE 'A'.
          88 test2 VALUE 'B'.
       PROCEDURE DIVISION.
       
      *SET statement for assignment
      *OK
           SET p-var1 TO ADDRESS OF var1.
      *KO
           SET UNSAFE p-var1 TO ADDRESS OF var1.

      *SET statement for indexes
      *OK
           SET idx UP BY 9.
           SET idx DOWN BY 7.
      *KO
           SET idx UP BY var2 - 3.
           SET idx DOWN BY 8 * var2.

      *SET statement for conditions
      *OK
           SET test1 TO TRUE.
      *KO (Only to test the syntax, it actually applies to BOOL variables)
           SET test1 TO FALSE.
       END PROGRAM tcSet.