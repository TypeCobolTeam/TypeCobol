       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOZCHKP.
       data division.
       working-storage section.
       REPLACE
                ==C-NBRPCB==      BY ==6==.  
        01 W-LEVEL01.
          05  W-IDTPCB OCCURS C-NBRPCB    PIC X(8).      
      
       END PROGRAM TCOZCHKP.
