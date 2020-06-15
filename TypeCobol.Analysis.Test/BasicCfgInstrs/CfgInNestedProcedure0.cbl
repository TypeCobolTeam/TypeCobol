       IDENTIFICATION DIVISION.
       PROGRAM-ID. StackedNestedPgms.
      
       PROCEDURE DIVISION.
      
       DECLARE PROCEDURE Proc0.
       END-DECLARE.
      
       DECLARE PROCEDURE Proc1.
       END-DECLARE.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested0.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE NestedProc0.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 m PIC 9 VALUE 2.
      
       PROCEDURE DIVISION.
       PARA-A.
       DISPLAY 'IN PARA-A'
       GO TO PARA-C.
      
       PARA-B.
       DISPLAY 'IN PARA-B '.
      
       PARA-C.
       DISPLAY 'IN PARA-C '.
       GO TO PARA-E PARA-F PARA-G DEPENDING ON m.
      
       PARA-D.
       DISPLAY 'IN PARA-D '.
      
       PARA-E.
       DISPLAY 'IN PARA-E '.
      
       PARA-F.
       DISPLAY 'IN PARA-F '.
      
       PARA-G.
       DISPLAY 'IN PARA-G '.
      
       GOBACK.
      
       END-DECLARE.
       END PROGRAM Nested0.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested1.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE NestedProc1.
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
      
       END PROGRAM Nested1.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested2.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE NestedProc2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       PROCEDURE DIVISION.
      
       MAIN-PARA.
      
             PERFORM FIRST-PARA THRU SECOND-PARA
      
             DISPLAY 'DOING PERFORM THRU'
      
             DISPLAY 'PARA NAME IS  FIRST-PARA'
      
             GOBACK.
      
       FIRST-PARA.
      
             DISPLAY 'PARA NAME IS  FIRST-PARA'.
      
       SECOND-PARA.
      
             DISPLAY 'PARA NAME IS  SECOND-PARA'.
      
       THIRD-PARA.
      
             DISPLAY 'PARA NAME IS  THIRD-PARA'.
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
      