       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA        PIC X(20) VALUE 'HELLO'.
       01 WS-DATA1       PIC X(40) VALUE 'WHAT''S UP ?'.
       01 WS-OUTPUT-DATA PIC X(70).
       PROCEDURE DIVISION.
           PERFORM SMPL-STRING
           GOBACK
           .
       SMPL-STRING.
           DISPLAY 'STRING EXAMPLE....'
           STRING WS-DATA, WS-DATA1 DELIMITED BY SPACE
             INTO WS-OUTPUT-DATA
             ON OVERFLOW
                PERFORM ERROR-OCCURED
             NOT ON OVERFLOW
                PERFORM NO-ERROR
           END-STRING.
           DISPLAY 'DATA AFTER STRING FUNCTION : ' WS-OUTPUT-DATA
           .
       ERROR-OCCURED.
           DISPLAY "ERROR OCCURED"
           .
       NO-ERROR.
           DISPLAY "NO ERROR"
           .      
       END PROGRAM STRING01.