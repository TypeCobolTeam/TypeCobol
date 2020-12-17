       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPYRPL4C.
      
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 xxxENT. COPY YxxxENT REPLACING ==::== BY ====.
       01 xxxENT2. COPY YxxxENT REPLACING ==::== BY ====.
       01 xxxENT3. COPY YxxxENT REPLACING ==::== BY ==S==.
       PROCEDURE DIVISION.
           MOVE 'A' TO xxxENT-FCT01-Var1 OF xxxENT.
           MOVE 'A' TO xxxENT-FCT01-Var1 OF xxxENT2.
           MOVE 'A' TO xxxENTS-FCT01-Var1.
       END PROGRAM CPYRPL4C.
