       IDENTIFICATION DIVISION.
       PROGRAM-ID. START01.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE1 ASSIGN TO DISK1
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY  IS STD-NO
           FILE STATUS IS WS-FS.
       DATA DIVISION.
       FILE SECTION.
       FD  FILE1.
       01 STD-REC.
          02 STD-NO          PIC 9(03).
          02 STD-NAME        PIC X(20).
          02 STD-GENDER      PIC X(07).
          02 FILLER          PIC X(50).
       WORKING-STORAGE SECTION.
       77 WS-FS               PIC 9(02).
       PROCEDURE DIVISION.
           PERFORM SMPL-START
           GOBACK
           .
       SMPL-START.
           DISPLAY 'FILE BROWSING...'
           OPEN INPUT FILE1
           START FILE1 KEY EQUAL STD-NO
              INVALID KEY
                PERFORM NOT-FOUND
              NOT INVALID KEY
                PERFORM FOUND
           END-START.
           CLOSE FILE1
           DISPLAY STD-REC
           .
       NOT-FOUND.
           DISPLAY "STUDENT DETAILS NOT FOUND"
           .
       FOUND.
           DISPLAY "STUDENT DETAILS FOUND"
           .
       END PROGRAM START01.