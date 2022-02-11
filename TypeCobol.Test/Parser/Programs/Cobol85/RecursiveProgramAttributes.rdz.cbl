       IDENTIFICATION DIVISION.
      *Ok
       PROGRAM-ID. Main RECURSIVE.
       PROCEDURE DIVISION.
           GOBACK
           .
       IDENTIFICATION DIVISION.
      *Ko Nested program declared in recursive program
       PROGRAM-ID. Nested1.
       END PROGRAM Nested1.
       END PROGRAM Main.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacked1.
       PROCEDURE DIVISION.
           GOBACK
           .
       IDENTIFICATION DIVISION.
      *Ko Nested program can't be recursive
       PROGRAM-ID. Nested1 RECURSIVE.
       END PROGRAM Nested1.
       END PROGRAM Stacked1.
      
       IDENTIFICATION DIVISION.
      *Ok
       PROGRAM-ID. Stacked2 RECURSIVE.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM Stacked2.
