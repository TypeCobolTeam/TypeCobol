       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEFORMTHRU.
       AUTHOR. MAYANJE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       PROCEDURE DIVISION.
      
       MAIN-PARA.
      
             PERFORM FIRST-PARA THRU SECOND-PARA
      
             DISPLAY 'DOING PERFORM THRU'
      
             DISPLAY 'PARA NAME IS  FIRST-PARA'
      
             STOP RUN.
      
       FIRST-PARA.
      
             DISPLAY 'PARA NAME IS  FIRST-PARA'.
      
       SECOND-PARA.
      
             DISPLAY 'PARA NAME IS  SECOND-PARA'.
      
       THIRD-PARA.
      
             DISPLAY 'PARA NAME IS  THIRD-PARA'.
       END PROGRAM PEFORMTHRU.
      