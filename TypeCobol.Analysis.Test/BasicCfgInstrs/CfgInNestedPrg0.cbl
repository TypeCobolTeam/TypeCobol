       IDENTIFICATION DIVISION.
       PROGRAM-ID. StackedNestedPgms.
      
       PROCEDURE DIVISION.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested0.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM Nested0.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested1.
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
       END PROGRAM Nested1.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested2.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM Nested2.
      
       END PROGRAM StackedNestedPgms.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacke0.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM Stacke0.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacke1.
       PROCEDURE DIVISION.
           GOBACK.
       END PROGRAM Stacke1.
      