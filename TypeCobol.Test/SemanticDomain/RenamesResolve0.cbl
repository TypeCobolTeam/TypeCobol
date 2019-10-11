       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZXERRM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MPOINT GLOBAL.
           05 RX PIC 9(4).
           05 RY PIC 9(4).
      
       01 RPOINT.
           05 RX PIC 9(4).
           05 RY PIC 9(4).
           05 XY REDEFINES RY PIC 9V999.
      
       66 YY RENAMES RX OF RPOINT THRU RY OF RPOINT.
       01 COUNTER PIC 9999.
           88 check VALUE "444".
            
       PROCEDURE DIVISION.
         .
       END PROGRAM DVZXERRM.
      