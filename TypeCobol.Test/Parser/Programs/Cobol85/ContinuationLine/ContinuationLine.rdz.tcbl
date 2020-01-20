       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       data division.
       working-storage section.
       01 syntax-ok PIC X(200) VALUE "This text is way too long to fit o
      -                        "n a single line !".
       01 var1 pic x(100) value "hi".
       PROCEDURE DIVISION.
             move 'CHAMPS 082 A 085 INTERDITS EN OUVERTURE'
      -        to foo
      

      *    OK
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1234xxxxxxx
      -            " er zer "
           display 'endxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx1234xxxxxxx
      -            ' gggg '
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            " vvvv
      -            " er zer "
           display 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -            ' vvvv
      -            ' er zer '
           move "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -         " er zer " to var1
           move "Crazy NIST ""Syntax" to var1
           move "                                           Crazy NIST "
      -         ""Syntax" to var1      

      
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
      -            " er zer 
      -            ' er zer
      -            " er zer '
      
      
      *    KO
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -              er zer "
           display "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      -             " er zer

           goback
           .
       END PROGRAM DVZF0OSM.