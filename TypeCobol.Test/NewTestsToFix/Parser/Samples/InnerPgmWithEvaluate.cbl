 IDENTIFICATION DIVISION.
 PROGRAM-ID. MYPROGA.
 PROCEDURE DIVISION.
     .
 IDENTIFICATION DIVISION.
 PROGRAM-ID MYPROGB.
 PROCEDURE DIVISION.
     EVALUATE AA
       WHEN 'A'
        continue
       WHEN 'B'
        continue
     END-EVALUATE
     EVALUATE BB
        WHEN 'C'
     END-EVALUATE                                               
     .
 END PROGRAM.
 END PROGRAM.