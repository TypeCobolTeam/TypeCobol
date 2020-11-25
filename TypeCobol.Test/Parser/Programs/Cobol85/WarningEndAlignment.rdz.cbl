       IDENTIFICATION DIVISION.
       PROGRAM-ID. PgmTest.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MyCondition TYPE bool value true.
       77 chaine PIC X(20) VALUE "mammouth".
       77 chaine-c PIC X(20).
      
       PROCEDURE DIVISION.
           IF MyCondition IF MyCondition THEN DISPLAY "OK" END-IF END-IF
           if MyCondition display "Ok1" end-if
      
      *test alignment with copy that contains if end-if
           COPY CPYALIGNSTA.
      
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
      
      %<<<MyProc info
        @Deprecated : It is deprecated
        @Todo:
              - todo1
              - todo 2
        @Params:
          - myDate just a date
          - bla bla < 2
          - myBool xxxx
          - toto toto
          - bli xxxx
      %>>>
        DECLARE PROCEDURE MyProc
           INPUT    myDate      TYPE Date
                    bla         Pic S9(1)V9(12)
           IN-OUT   myBool      TYPE BOOL
           OUTPUT   toto        TYPE BOOL
                    bli         Pic PPP999PPP.
        PROCEDURE DIVISION.
           CONTINUE.
        END-DECLARE.
      
      %<<<MyProc2 info
        @Deprecated : It is deprecated
        @Todo:
              - todo1
              - todo 2
        @Params:
          - myDate just a date
          - bla bla < 2
          - myBool xxxx
          - toto toto
          - bli xxxx
      %>>>
        DECLARE PROCEDURE MyProc2
           INPUT    myDate      TYPE Date
                    bla         Pic S9(1)V9(12)
           IN-OUT   myBool      TYPE BOOL
           OUTPUT   toto        TYPE BOOL
                    bli         Pic PPP999PPP.
        PROCEDURE DIVISION.
           CONTINUE.
         END-DECLARE.

      *test alignment with copy that contains procedure
       COPY CPYALIGNPROC.

      *check if not exception with procedure with empty IF END-IF
       DECLARE PROCEDURE MyProc3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 testVar type BOOL.
       PROCEDURE DIVISION.
             IF testVar
      
             ELSE
      
             END-IF
             .
       END-DECLARE.

       END PROGRAM PgmTest.
      