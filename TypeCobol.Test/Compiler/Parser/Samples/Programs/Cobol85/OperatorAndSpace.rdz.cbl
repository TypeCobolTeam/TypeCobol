       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZXMNT0.
      *REMARKS. COPY=(
      *    YTTMCOI
      * ).
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       DATA DIVISION.
       working-storage section.
       01 WS-LINE-BREAK pic X(10).
       01 A.
           05 B pic X(50).
       01    pic X(03).
      
       01 buffer pic x(50).
       01 result pic  9.
       01 float pic  9(5)V9(4).
       01 .
               05  VarGroup             PIC 9.
               05  VarGroup-Bis REDEFINES VarGroup pic X.
               05  FILLER       REDEFINES VarGroup-Bis pic X.
      
      
      
       PROCEDURE DIVISION.
           Add 1 to VarGroup result
      
           compute result=1 - 1
      *    compute result=1- 1
      *    compute result=1 -1
           compute result=1+ 1
      *    compute result=1 +1
           compute result=1 + 1
           compute result=1/1
           compute result=1*1
           compute result=1**1
      
           compute result=result - result
      *    compute result=result- result
           compute result=result -result
           compute result=result+ result
           compute result=result +result
           compute result=result + result
           compute result=result/result
           compute result=result*result
           compute result=result**result
      
      
           if(result>1 or result<1 or result>=1 or
              result<=1 or result=1)
              continue
           end-if
      
           if(1>result or 1<result or 1>=result or
              1<=result or 1=result)
              continue
           end-if
           if(result>result or result<result or result>=result or
              result<=result or result=result)
              continue
           end-if
      
           move buffer(1:2) to buffer
      
           .
       END PROGRAM DVZXMNT0.