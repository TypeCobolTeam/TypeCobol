       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOFM117.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-IdcStr PIC 9(02) value 1.
           REPLACE ==:BlocJL1 :== BY ==30==
           ==:BlocJL2                   :== BY ==30==
           ==:BlocJL3
                   :== BY ==30==.
       01 W-Tab.
         10 W-COD1   PIC X          OCCURS :BlocJL1:.
         10 W-COD2   PIC X          OCCURS :BlocJL2:.
         10 W-COD3   PIC X          OCCURS :BlocJL3:.
       01 Var1 pic X.
       END PROGRAM TCOFM117.
      