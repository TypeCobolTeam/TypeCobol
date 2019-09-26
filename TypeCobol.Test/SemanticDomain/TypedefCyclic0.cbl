       IDENTIFICATION DIVISION.
       PROGRAM-ID. TypedefCyclic0.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01 POINT TYPEDEF STRICT.
           05 X COMP-5.
           05 Y COMP-5.
      
       01 POINT-CYC0 TYPEDEF STRICT.
           05 X COMP-5.
           05 Y COMP-5.
           05 P1 TYPE POINT.
           05 P2 TYPE POINT-CYC0.
      
       01 POINT-CYC1 TYPEDEF STRICT.
           05 p3 TYPE POINT.
           05 P4 TYPE POINT-CYC0.
      
       01 POINT-CYC2 TYPEDEF STRICT.
           05 p3 TYPE POINT.
           05 P4 TYPE POINT-CYC3.
      
       01 POINT-CYC3 TYPEDEF STRICT.
           05 p3 TYPE POINT.
           05 P4 TYPE POINT-CYC2.
      
       01 P5 TYPE POINT-CYC0.
       01 P6 TYPE POINT-CYC1.
       01 P7 TYPE POINT-CYC0 OCCURS 5.
       01 P8 TYPE POINT-CYC1 OCCURS 10.
      
       01 P9.
           05 X1 PIC 9(4) COMP-5.
           05 Y1 PIC 9(4) COMP-5.
           05 CYC.
                10 XY TYPE POINT.
                10 XX TYPE POINT-CYC1.
      
       01 P10.
           05 X1 PIC 9(4) COMP-5.
           05 Y1 PIC 9(4) COMP-5.
           05 CYC.
                10 XY TYPE POINT.
                10 XX TYPE POINT.
      
       END PROGRAM TypedefCyclic0.
      