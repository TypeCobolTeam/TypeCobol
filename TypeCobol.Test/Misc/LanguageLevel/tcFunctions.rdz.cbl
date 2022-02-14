       IDENTIFICATION DIVISION.
       PROGRAM-ID. tcFunctions.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var-date PIC X(21).
       01 var-input PIC X(100).
       01 var-output PIC 9(2).
       PROCEDURE DIVISION.
       main.
           COMPUTE var-date = FUNCTION CURRENT-DATE()
           COMPUTE var-output = FUNCTION myFunction(var-input)
           GOBACK
           .
           
       DECLARE FUNCTION myFunction
               INPUT s_string PIC X(100)
               RETURNING s_length PIC 9(2).
       END-DECLARE.
       
       DECLARE PROCEDURE myProcedure.
       END-DECLARE.
           
       END PROGRAM tcFunctions.