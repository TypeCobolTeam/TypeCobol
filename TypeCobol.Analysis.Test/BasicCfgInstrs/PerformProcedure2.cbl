﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORMPROC.
       AUTHOR. MAYANJE
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE
           DATA RECORD IS USER-REC.
       01  USER-REC.
           05  USER-ID               PIC 9999.
           05  USER-NAME             PIC X(20).
       WORKING-STORAGE SECTION.
       01  WORK-AREA.
           05  EOF-SEEN        PIC X          VALUE "N".
           05  ANSWER         PIC X          VALUE SPACES.
       PROCEDURE DIVISION.
       MAINLINE.
           PERFORM A-STARTUP
           PERFORM B-PROCESS
           PERFORM C-CLEANUP
           GOBACK.
       A-STARTUP.
           OPEN INPUT USER-FILE.
       B-PROCESS.
           READ USER-FILE
               AT END
                  MOVE "Y" TO EOF-SEEN.
           PERFORM B-LOOP
               WITH TEST AFTER UNTIL EOF-SEEN = "Y".
       B-LOOP.
           DISPLAY "THE ID IS " USER-ID.
           DISPLAY "THE NAME IS " USER-NAME.
           DISPLAY " ".
           DISPLAY "ENTER TO CONTINUE".
           ACCEPT ANSWER.
           READ USER-FILE
               AT END
                  MOVE "Y" TO EOF-SEEN.
       C-CLEANUP.
           CLOSE USER-FILE.
       END PROGRAM PERFORMPROC.
       