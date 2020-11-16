       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ01.
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
       01 WS-EOF-SW           PIC X(01) VALUE 'N'.
          88 EOF-SW           VALUE 'Y'.
          88 NOT-EOF-SW       VALUE 'N'.
       01 WS-FS-RCD           PIC X(80).
       PROCEDURE DIVISION.
           PERFORM SMPL-READ
           GOBACK
           .
       SMPL-READ.
           DISPLAY 'FILE READING....'
           READ FILE1 NEXT RECORD
                      INTO WS-FS-RCD
           AT END
             SET EOF-SW         TO TRUE
           NOT AT END
             PERFORM DISPLAY-RECORD
           END-READ
           .
       DISPLAY-RECORD.
           DISPLAY WS-FS-RCD
           .
       END PROGRAM READ01.