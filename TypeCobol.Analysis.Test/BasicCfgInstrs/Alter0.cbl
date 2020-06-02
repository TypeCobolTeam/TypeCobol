       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
      
       PROCEDURE DIVISION.
      
       A-PARA.
           PERFORM  DISPLAY 'IN A-PARA'
           END-PERFORM.
           PERFORM C-PARA THRU E-PARA.
           ALTER B-PARA TO PROCEED TO E-PARA.
       B-PARA. GO TO B-PARA.
           DISPLAY 'IN B-PARA'.
           STOP RUN.
      
       C-PARA.
           DISPLAY 'IN C-PARA'.
      
       D-PARA.
           DISPLAY 'IN D-PARA'.
      
       E-PARA.
           DISPLAY 'IN E-PARA'.
       END PROGRAM HELLO.
      