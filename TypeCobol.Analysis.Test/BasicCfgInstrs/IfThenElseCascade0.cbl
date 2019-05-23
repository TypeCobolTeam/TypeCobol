       IDENTIFICATION DIVISION.
       PROGRAM-ID. IFTHEN.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 menu-input PIC X(1) VALUE "R".
       PROCEDURE DIVISION.
           IF (menu-input = "0") THEN
             DISPLAY "OPTION 0"
           ELSE
            IF (menu-input >= "1") AND (menu-input <= "9") THEN
              DISPLAY "OPTION 1-9"
            ELSE
              IF (menu-input = "R") THEN
                DISPLAY "OPTION R"
              ELSE
                IF (menu-input = "X") THEN
                    DISPLAY "OPTION X"
                ELSE
                    DISPLAY "NO OPTION"
                END-IF
              END-IF
            END-IF
           END-IF.
       END PROGRAM IFTHEN.
      
      