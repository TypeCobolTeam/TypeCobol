       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              SELECT hend.uma.ensi.* 
              from 
              iid.rr as new (h,k)
              , uma as ka(l,u)
              
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.