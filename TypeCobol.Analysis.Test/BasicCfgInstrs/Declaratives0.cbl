       IDENTIFICATION DIVISION.
       PROGRAM-ID. DeclarativesTest.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      WITH DEBUGGING MODE
                      .
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       PROCEDURE DIVISION.
      
      DDECLARATIVES.
      DDECLARATION SECTION.
      D    USE FOR DEBUGGING ON ALL PROCEDURES.
      DAFFICHAGE-PARAGRAPHE.
      D       display DEBUG-NAME
      D    .
      DX.  EXIT.
      DEND DECLARATIVES.
      
             DISPLAY "TOTO".
      
       END PROGRAM DeclarativesTest.
      