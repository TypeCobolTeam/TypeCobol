       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMREDFT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01 group0.
          05 group1.
             10 var1 PIC X(10).
          05 group2 REDEFINES group1.
             10 var2 PIC X(20).
          05 group3 REDEFINES group2.
             10 var3 PIC X(3).
          05 next-group.
             10 next-var PIC X.
      
        PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM PGMREDFT.