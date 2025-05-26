       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOZERRCP.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 VAR-1 PIC X.
      * Error: VAR 2 instead of VAR-2
       01 VAR 2 PIC X.

       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOZERRCP.