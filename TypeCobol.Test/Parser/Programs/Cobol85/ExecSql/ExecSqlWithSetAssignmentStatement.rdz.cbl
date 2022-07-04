       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              SET :SALARY = 50000
              SET :SALARY = NULL
              SET :SALARY = DEFAULT, 
              (:SALARY, :COMMISSION) = (50000, 8000)
              SET (:SALARY, :COMMISSION) = (50000, 8000)
              SET (:SALARY, :COMMISSION) = (VALUES(NULL, 8000))
              SET (:SALARY_1, :COMMISSION_1) = (50000, 8000) ,
              :SALARY_2 = 50000        
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.