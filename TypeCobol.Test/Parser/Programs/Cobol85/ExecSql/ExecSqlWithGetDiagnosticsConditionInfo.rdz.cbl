       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
             EXEC SQL 
              GET DIAGNOSTICS CONDITION 1             
               :dasqlcode   = DB2_RETURNED_SQLCODE,        
               :datokencnt  = DB2_TOKEN_COUNT,             
               :datoken1    = DB2_ORDINAL_TOKEN_1,         
               :datoken2    = DB2_ORDINAL_TOKEN_2,         
               :datoken3    = DB2_ORDINAL_TOKEN_3,         
               :datoken4    = DB2_ORDINAL_TOKEN_4,         
               :datoken5    = DB2_ORDINAL_TOKEN_5,         
               :dasqlerrd1b = DB2_MESSAGE_ID,              
               :damsgtext   = MESSAGE_TEXT,                
               :dasqlerrp   = DB2_MODULE_DETECTING_ERROR,  
               :dasqlstate  = RETURNED_SQLSTATE
              GET DIAGNOSTICS CONDITION :i :retsqlstate = RETURNED_SQLSTATE
              GET DIAGNOSTICS CONDITION :HV_PRODUCT_ID :i = DB2_PRODUCT_ID
              GET DIAGNOSTICS  :var4 = ALL STATEMENT,
              CONDITION 3,CONNECTION
              GET STACKED DIAGNOSTICS :var1 = DB2_LAST_ROW, MORE 
              GET STACKED DIAGNOSTICS :var1 = DB2_LAST_ROW, :var2 =MORE
              
             END-EXEC.
       PROCEDURE DIVISION.
         
           GOBACK
           .
       END PROGRAM DVZZMFT3.