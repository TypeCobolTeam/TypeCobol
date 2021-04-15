       IDENTIFICATION DIVISION.
       PROGRAM-ID. SelectFile.

       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT
           SELECT ENT00A       ASSIGN
           SELECT ENT00B       ASSIGN TO
           SELECT ENT010       ASSIGN TO ENT010
                  FILE STATUS WK-ST-ENT010.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-CTRL-FILE-STATUS.
           02  WK-ST-ENT010            PIC 9(2)       VALUE ZEROS.
                88  FIN-FICHIER-ENT010                 VALUE 10.
       
       PROCEDURE DIVISION.
       .

       END PROGRAM SelectFile.