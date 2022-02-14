       IDENTIFICATION DIVISION.
      *Ko COMMON on non-nested
       PROGRAM-ID. Main COMMON.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM Main.
      
       IDENTIFICATION DIVISION.
      *Ko COMMON on non-nested
       PROGRAM-ID. Stacked1 COMMON.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM Stacked1.
      
       IDENTIFICATION DIVISION.
      *Ok
       PROGRAM-ID. Stacked2.
       PROCEDURE DIVISION.
           GOBACK
           .
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested1.
       END PROGRAM Nested1.
       IDENTIFICATION DIVISION.
      *Ok
       PROGRAM-ID. Nested2 COMMON.
       END PROGRAM Nested2.
       END PROGRAM Stacked2.