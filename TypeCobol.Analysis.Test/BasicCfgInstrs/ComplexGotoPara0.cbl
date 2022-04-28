       IDENTIFICATION DIVISION.
       PROGRAM-ID. GOTOCplx.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 m PIC 9 VALUE 2.
      
       PROCEDURE DIVISION.
       PARA-A.
             DISPLAY 'IN PARA-A'
             GO TO PARA-C.
      
       PARA-B.
             DISPLAY 'IN PARA-B '.
      
       PARA-C.
             DISPLAY 'IN PARA-C '.
             GO TO PARA-E PARA-F PARA-G DEPENDING ON m.
      
       PARA-D.
             DISPLAY 'IN PARA-D '.
      
       PARA-E.
             DISPLAY 'IN PARA-E '.
      
       PARA-F.
             DISPLAY 'IN PARA-F '.
      
       PARA-G.
             DISPLAY 'IN PARA-G '.
      
             GOBACK.
       END PROGRAM GOTOCplx.
      