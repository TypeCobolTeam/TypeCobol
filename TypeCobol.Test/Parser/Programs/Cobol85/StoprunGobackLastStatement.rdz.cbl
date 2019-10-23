       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       PROCEDURE DIVISION.
       INIT.
            goback
            .
       INIT2.
            goback
            display "Hello"
            .
       INIT3.
            stop run
            display "stopped"
            .
       sect SECTION.
            display "hi"
            .  
       sect1 SECTION.
            goback
            display "hi"
            .   
       sect2 SECTION.
       p1.
            goback
            display "hi"
            .   
       sect3 SECTION.
       END PROGRAM Pgm.
       