       IDENTIFICATION DIVISION.
       PROGRAM-ID. StackedNestedPgms.
      
       PROCEDURE DIVISION.
      
       DECLARE PROCEDURE Proc0.
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
       END-DECLARE.
      
       DECLARE PROCEDURE Proc1.
       END-DECLARE.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested0.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE NestedProc0.
       PROCEDURE DIVISION.
       END-DECLARE.
       END PROGRAM Nested0.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested1.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE NestedProc1.
       PROCEDURE DIVISION.
       END-DECLARE.
      
       END PROGRAM Nested1.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested2.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE NestedProc2.
       PROCEDURE DIVISION.
       END-DECLARE.
      
       END PROGRAM Nested2.
      
       END PROGRAM StackedNestedPgms.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacke0.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE StackedNestedProc0.
       PROCEDURE DIVISION.
       END-DECLARE.
      
       END PROGRAM Stacke0.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacke1.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE StackedNestedProc1.
       PROCEDURE DIVISION.
       END-DECLARE.
       END PROGRAM Stacke1.
      