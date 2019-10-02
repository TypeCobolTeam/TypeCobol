       IDENTIFICATION DIVISION.
       PROGRAM-ID. PgmTest.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MyCondition TYPE bool.
       77 chaine PIC X(20) VALUE "mammouth".
       77 chaine-c PIC X(20).
      
       PROCEDURE DIVISION.
           Set MyCondition to true
           IF MyCondition IF MyCondition THEN DISPLAY "OK" END-IF END-IF
           if MyCondition display "Ok1" end-if

           if MyCondition
               display "Ok1"
      *Ok - end-if aligned with if
           end-if
      
           if MyCondition
               display "Ok2"
      *Ko- end-if not aligned with if
               end-if
      
           if MyCondition
                display "Ok3"
                if MyCondition
                    display "Ok3-2"
                end-if
                end-if
      
           evaluate true
              when MyCondition
               display "OkE"
      *Ko- end-evaluate not aligned with evaluate
              when other
               display "other"
               end-evaluate
      
           PERFORM TEST AFTER UNTIL chaine = chaine-c
                DISPLAY chaine-c
                MOVE chaine TO chaine-c
             END-PERFORM.
      
           MOVE "cc" TO chaine
           STRING "ggg" chaine
               delimited by size
               INTO chaine-c
             END-STRING
      
           GOBACK.
       END PROGRAM PgmTest.
