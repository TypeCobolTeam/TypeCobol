* issue #263
 IDENTIFICATION DIVISION.
 PROGRAM-ID. MYPROGA.
 PROCEDURE DIVISION.
     .
 IDENTIFICATION DIVISION.
 PROGRAM-ID. MYPROGB.
 PROCEDURE DIVISION.
     EVALUATE AA
       WHEN 'A'
        continue
       WHEN 'B'
        continue
     END-EVALUATE
     EVALUATE BB
        WHEN 'C'
         continue
     END-EVALUATE                                               
     .
 END PROGRAM MYPROGB.
 END PROGRAM MYPROGA.