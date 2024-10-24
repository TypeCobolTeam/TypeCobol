﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOFM117.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-DESC-1
               ASSIGN TO MY-FILE
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FILE-DESC-1
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  FILE-DESC-1-AREA PIC X(200).
       WORKING-STORAGE SECTION.
       01  VAR-FILE-BUFFER  PIC X(200).
       01  VAR-LINE-COUNT   PIC S9(4).
       PROCEDURE DIVISION.
           WRITE FILE-DESC-1-AREA
           FROM VAR-FILE-BUFFER
           AFTER ADVANCING VAR-LINE-COUNT LINES.
           GOBACK
           .
       END PROGRAM TCOFM117.