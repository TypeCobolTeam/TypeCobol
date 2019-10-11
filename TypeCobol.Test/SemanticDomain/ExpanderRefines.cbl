       IDENTIFICATION DIVISION.
       PROGRAM-ID. VariableSeeking.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01 Group1.
           10 itemBuffer pic X(40).
           10 redefines itemBuffer.
              15 toto   type MyTypeUnderRedefines.
       01 MyTypeUnderRedefines typedef strict private.
                15 idt           pic 99.
      
       PROCEDURE DIVISION.
      
           move idt to idt.
      
       END PROGRAM VariableSeeking.
