       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL 
              SELECT i.i.d.* 
              FROM 
              iid.rr as new (h,k)
              , uma as ka(l,u)
           END-EXEC.
           EXEC SQL
              SELECT * 
              FROM 
              t_table as new_name (c1,,)
           END-EXEC.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM DVZZMFT3.