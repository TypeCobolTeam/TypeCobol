       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 var1 pic x(30) value "hi".
       PROCEDURE DIVISION.
           display "max-ok_max-ok_max-ok_max-ok_max-ok_max-ok_max-ok_ma"
           display "3333333333333333333333333333222222222222222222222222
           move var1 to var1
           display "endxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx1234
           display "3333333333333333333333333333222222222222222222222222
           move "gggggggggggggggggggggggggggggggggggggggggggggssssssssss
                    to var1
      
      
      *    OK
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            " er zer "
           display 'endxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx1234xxxxxxx
      -            ' gggg '
           display 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            ' vvvv
      -            ' er zer '
           move "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -         " er zer " to var1
      
      
      *    KO
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            ' er zer '
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            " er zer '
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            ' er zer "
      
           display 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            " er zer "
           display 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            ' er zer "
           display 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            " er zer '
      
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            ' er zer
      -            " er zer "
           display 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            ' er zer
      -            " er zer '
      
      
      *    KO
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -              er zer "
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -             " er zer
      
           GOBACK.
       END PROGRAM Pgm.