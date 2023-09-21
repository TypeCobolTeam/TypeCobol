       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm1.
      *KO copy alters scan state by including DATA DIVISION
       COPY DataDivision.
       01 var2 PIC X.
       PROCEDURE DIVISION.
           DISPLAY var1
           DISPLAY var2
           GOBACK
           .
       END PROGRAM Pgm1.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm2.
      *OK See #2316, copys can now use PROCEDURE DIVISION code element
       COPY ProcedureDivision.
           DISPLAY 'This statement comes from main source file'
           GOBACK
           .
       END PROGRAM Pgm2.
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm3.
      *KO copy alters scan state by enabling DEBUGGING MODE
       COPY EnablingDebuggingMode.
       PROCEDURE DIVISION.
      D    DISPLAY 'This is a debug statement'
           GOBACK
           .
       END PROGRAM Pgm3.