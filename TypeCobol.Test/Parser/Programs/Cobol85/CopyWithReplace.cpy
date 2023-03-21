       REPLACE ==:TAG:== BY ==WORD==.
       01 :TAG: PIC X. *> WORD
       REPLACE ==WORD== BY ==var2==.
       01 var1 PIC X.
       01 WORD PIC 9. *> Var1
       REPLACE ==:TAG:== BY ==ENDWORD==.
       01 :TAG: PIC X.