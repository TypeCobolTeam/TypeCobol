       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOFM117.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 PIC U USAGE IS UTF-8.
       01 group1 GROUP-USAGE IS UTF-8.
          05 part1 PIC U BYTE-LENGTH 20.
          05 part2 PIC U BYTE-LENGTH 40.
          05 part3 PIC U BYTE-LENGTH 20.
       01 lit1 PIC U(80) VALUE U"This text has no escape char".
       01 lit2 PIC U(80) VALUE U'\u03BB is Lambda'.
       01 lit3 PIC U(80) VALUE u"\U0001F43F is a chipmunk emoji".
       PROCEDURE DIVISION.
           MOVE U"This text has no escape char" TO group1
           MOVE U'\u03BB is Lambda' TO group1
           MOVE u"\U0001F43F is a chipmunk emoji" TO group1
           GOBACK
           .
       END PROGRAM TCOFM117.