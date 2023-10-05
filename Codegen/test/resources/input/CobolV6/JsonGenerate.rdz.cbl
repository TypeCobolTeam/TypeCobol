       IDENTIFICATION DIVISION.
       PROGRAM-ID. JsonTest.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 elt.
          02 var1 PIC X(12).
          02 var2 PIC 9(2).
          02 var3 PIC X(20).
          02 var4 PIC X(20).
          02 var5 PIC X(7).
          02 var6 PIC 9(18).

       01 char-count PIC 9(3).
       01 result PIC X(256).
       01 ret-code PIC 9(1).

       PROCEDURE DIVISION.
           JSON GENERATE result FROM elt
           JSON GENERATE result FROM elt COUNT char-count
           JSON GENERATE result FROM elt COUNT IN char-count
           JSON GENERATE result FROM elt NAME var1 'Nom'
           JSON GENERATE result FROM elt NAME OF var1 'Nom'
           JSON GENERATE result FROM elt NAME OF var1 IS 'Nom'
           JSON GENERATE result FROM elt NAME OF elt IS OMITTED
           JSON GENERATE result FROM elt NAME OF elt OMITTED
           JSON GENERATE result FROM elt NAME elt IS OMITTED
           JSON GENERATE result FROM elt NAME elt OMITTED
           JSON GENERATE result FROM elt
             NAME OF var1 IS 'Nom'
             var2 'Age'
             var3 IS 'Adresse'
           JSON GENERATE result FROM elt SUPPRESS var4
           JSON GENERATE result FROM elt SUPPRESS var4 var5 var6
      
           JSON GENERATE result FROM elt END-JSON
           JSON GENERATE result FROM elt
             EXCEPTION MOVE 1 TO ret-code
           END-JSON
           JSON GENERATE result FROM elt
             ON EXCEPTION MOVE 1 TO ret-code
           END-JSON
           JSON GENERATE result FROM elt
             NOT EXCEPTION MOVE 0 TO ret-code
           END-JSON
           JSON GENERATE result FROM elt
             NOT ON EXCEPTION MOVE 0 TO ret-code
           END-JSON
           JSON GENERATE result FROM elt
             EXCEPTION MOVE 1 TO ret-code
             NOT EXCEPTION MOVE 0 TO ret-code
           END-JSON
           JSON GENERATE result FROM elt
             ON EXCEPTION MOVE 1 TO ret-code
             NOT EXCEPTION MOVE 0 TO ret-code
           END-JSON
           JSON GENERATE result FROM elt
             EXCEPTION MOVE 1 TO ret-code
             NOT ON EXCEPTION MOVE 0 TO ret-code
           END-JSON
           JSON GENERATE result FROM elt
             ON EXCEPTION MOVE 1 TO ret-code
             NOT ON EXCEPTION MOVE 0 TO ret-code
           END-JSON
           .
      
       END PROGRAM JsonTest.