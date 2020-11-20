       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC 9.
       PROCEDURE DIVISION.
       main.
      *OK
           ALTER p1 TO PROCEED TO other-finish
      *KO p2 is empty
           ALTER p2 TO PROCEED TO other-finish
      *KO p3 does not have any GO TO statement to alter
           ALTER p3 TO PROCEED TO other-finish
      *KO p4 has a GO TO but it's not the first statement
           ALTER p4 TO PROCEED TO other-finish
      *KO p5 has a unique GO TO but it uses a DEPENDING ON clause
           ALTER p5 TO PROCEED TO other-finish
           .
       p1.
           GO TO p2
           .
       p2.
       p3.
           DISPLAY 'p3'
           .
       p4.
           DISPLAY 'p4'
           GO TO finish
           .
       p5.
           GO TO finish DEPENDING ON var1
           .
       finish.
           DISPLAY 'finish'
           .
       other-finish.
           DISPLAY 'other'
           .
       END PROGRAM DVZZMFT1.