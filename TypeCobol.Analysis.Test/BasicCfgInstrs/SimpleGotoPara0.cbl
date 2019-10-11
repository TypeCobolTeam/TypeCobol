       IDENTIFICATION DIVISION.
       PROGRAM-ID. GOTO0.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(4) VALUE 4.
       01 B PIC 9(4) VALUE 4.
       01 C PIC 9(4) VALUE 5.
       PROCEDURE DIVISION.
         MAIN-PARA.
              DISPLAY 'BEGIN MAIN PARAGRAPH HERE'.
              GO TO PARA-1.
              DISPLAY 'A VALUE ', A.
              STOP RUN.
         PARA-1.
              COMPUTE A = B + C.
       END PROGRAM GOTO0.
      