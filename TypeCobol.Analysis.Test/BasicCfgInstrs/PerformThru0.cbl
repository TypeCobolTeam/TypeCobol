       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORMTHRU.
       AUTHOR. MAYANJE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       PROCEDURE DIVISION.
      
       MAIN-PARA.
      
             PERFORM FIRST-PARA THRU THIRD-PARA
      
             DISPLAY 'DOING PERFORM THRU'
      
             DISPLAY 'PARA NAME IS  FIRST-PARA'
      
             GOBACK.
      
       FIRST-PARA.
      
             DISPLAY 'PARA NAME IS  FIRST-PARA'.
      
       SECOND-PARA.
      
             DISPLAY 'PARA NAME IS  SECOND-PARA'.
      
       THIRD-PARA.
      
             DISPLAY 'PARA NAME IS  THIRD-PARA'.
       END PROGRAM PERFORMTHRU.
      