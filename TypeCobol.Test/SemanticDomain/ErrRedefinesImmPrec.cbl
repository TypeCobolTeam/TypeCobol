       IDENTIFICATION DIVISION.
       PROGRAM-ID. RedefErr.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RPOINT.
          05 RX PIC 9(4).
          05 RY PIC 9(4).
          05 XY REDEFINES RX PIC 9V999.      
       END PROGRAM RedefErr.
