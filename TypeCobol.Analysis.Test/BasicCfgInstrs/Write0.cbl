       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRITE01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE2 ASSIGN TO DISK2
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FS2.
       DATA DIVISION.
       FILE SECTION.
       FD  FILE2.
       01 STD-REC2.
          02 STD-NO2         PIC 9(03).
          02 STD-NAME2       PIC X(20).
          02 STD-GENDER2     PIC X(07).
          02 FILLER          PIC X(50).
       WORKING-STORAGE SECTION.
       77 WS-FS2              PIC 9(02).
       01 WS-FS-RCD           PIC X(80).
       PROCEDURE DIVISION.
           PERFORM SMPL-WRITE
           GOBACK
           .
       SMPL-WRITE.
           DISPLAY 'FILE WRITING....'
           OPEN OUTPUT FILE2
           WRITE STD-REC2 FROM WS-FS-RCD
           NOT INVALID KEY
             DISPLAY "Record is Ok"
           INVALID KEY
             PERFORM INVALIDKEY
           END-WRITE
           CLOSE FILE2
           .
       INVALIDKEY.
           DISPLAY "INVALID KEY"
           .
       END PROGRAM WRITE01.