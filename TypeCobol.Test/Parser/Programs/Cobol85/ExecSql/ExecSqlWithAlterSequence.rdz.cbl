       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL               
              ALTER SEQUENCE org_seq2 MINVALUE 6.0
              MINVALUE 8.0    
              ALTER SEQUENCE org_seq RESTART
              ALTER SEQUENCE org_seq RESTART WITH 100.0
              ALTER SEQUENCE org_seq RESTART WITH 100.5
              ALTER SEQUENCE org_seq INCREMENT BY 5.0
              ALTER SEQUENCE org_seq2 MAXVALUE 25.0 NO MAXVALUE
              ALTER SEQUENCE org_seq2 MINVALUE 6.0
              ALTER SEQUENCE org_seq2 MINVALUE 6.1
              ALTER SEQUENCE org_seq2 MAXVALUE 7.0
              ALTER SEQUENCE org_seq2 MAXVALUE 7.2
              ALTER SEQUENCE org_seq2 NO CYCLE
              ALTER SEQUENCE org_seq2 CYCLE
              ALTER SEQUENCE org_seq2 CACHE 100
               ALTER SEQUENCE org_seq2 CACHE 100.55
              ALTER SEQUENCE org_seq2 NO CACHE
              ALTER SEQUENCE org_seq2 ORDER
              ALTER SEQUENCE org_seq2 NO ORDER
              ALTER SEQUENCE org_seq2 NO ORDER RESTART 
              INCREMENT BY 5.0           
              ALTER SEQUENCE org_seq INCREMENT BY 5.8
               ALTER SEQUENCE org_seq3
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.