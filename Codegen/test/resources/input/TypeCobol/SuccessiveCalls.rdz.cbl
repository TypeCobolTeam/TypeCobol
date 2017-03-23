       IDENTIFICATION DIVISION.
       PROGRAM-ID. Successive.
       SERVICE IS YSUXXESS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DATE-EU                   TYPEDEF strict.
           10 DD                     PIC 9(02).
           10 MM                     PIC 9(02).
           10 YYYY                   PIC 9(04).
       01  DATE-US                   TYPEDEF strict.
           10 YYYY                   PIC 9(04).
           10 DD                     PIC 9(02).
           10 MM                     PIC 9(02).

       PROCEDURE DIVISION.

       DECLARE FUNCTION ConvertUS2EU PRIVATE
                        INPUT     mydate TYPE DATE-US
                        RETURNING result TYPE DATE-EU.
       PROCEDURE DIVISION.
           MOVE mydate TO result.
       END-DECLARE.

       DECLARE PROCEDURE ConvertEU2US PRIVATE
                        INPUT     mydate TYPE DATE-EU
                        OUTPUT    result TYPE DATE-US.
       PROCEDURE DIVISION.
           MOVE mydate TO result.
       END-DECLARE.

       DECLARE FUNCTION GetCurrentDateUS PUBLIC
                        RETURNING result TYPE DATE-US.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 MyDateUS TYPE DATE-US.
       PROCEDURE DIVISION.
           CONTINUE.
       END-DECLARE.
       
       DECLARE FUNCTION GetCurrentDateEU PUBLIC
                        RETURNING result TYPE DATE-EU.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 mydateeu TYPE DATE-EU.
       01 mydateus TYPE DATE-US.
       01 tmp TYPE DATE-US.
       PROCEDURE DIVISION.
           MOVE FUNCTION GetCurrentDateUS() TO tmp.
           MOVE FUNCTION ConvertUS2EU(tmp) TO result.
           CALL  ConvertEU2US   INPUT mydateeu OUTPUT mydateus END-CALL.
       END-DECLARE.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. Nested.
       DATA DIVISION.
       PROCEDURE DIVISION.
       END PROGRAM Nested.


       END PROGRAM Successive.