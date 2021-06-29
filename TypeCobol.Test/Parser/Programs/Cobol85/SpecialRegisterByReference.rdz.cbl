       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm1.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 txt   pic X(100).
       01 RC    pic 99.
       PROCEDURE DIVISION.
      *KO cannot use LENGTH OF by reference
           Call "SOMEPGM" USING txt
                   by reference length of txt
                                RC.
      
      *KO cannot use LENGTH OF by reference
           Call "SOMEPGM" USING txt
                                length of txt
                                RC.
                                
      *KO cannot use LENGTH OF by reference
           Call "SOMEPGM" USING txt
                   by content   length of txt
                   by reference length of txt.
      
      *Ok
           Call "SOMEPGM" USING txt
                   by content   length of txt
                   by reference RC.
       END PROGRAM Pgm1.