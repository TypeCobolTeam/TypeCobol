       IDENTIFICATION DIVISION.
       PROGRAM-ID. StackedNestedPgms.
      
       PROCEDURE DIVISION.
      
       DECLARE PROCEDURE Proc0.
       END-DECLARE.
      
       DECLARE PROCEDURE Proc1.
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
       PROGRAM-ID. Stacked0.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE StackedNestedProc0.
       PROCEDURE DIVISION.
       END-DECLARE.
      
       END PROGRAM Stacked0.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacked1.
       PROCEDURE DIVISION.
           GOBACK.
       DECLARE PROCEDURE StackedNestedProc1.
       PROCEDURE DIVISION.
       END-DECLARE.
       END PROGRAM Stacked1.
      