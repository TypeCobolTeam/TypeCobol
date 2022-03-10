      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZPGM1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01 str-message TYPE DVZZPGM2::Type1.
       01 str-message.
           02 var1 PIC X(20).
                                           
       procedure division.
      *    DISPLAY str-message::var1
           DISPLAY var1 OF str-message
           .
       END PROGRAM DVZZPGM1.
