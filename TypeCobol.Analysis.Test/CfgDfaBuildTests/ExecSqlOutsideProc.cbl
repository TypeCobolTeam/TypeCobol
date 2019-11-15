       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXECOUTPROC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DCLTGMIPDS.
           10 NOM-DONSTAT   PIC X(10).
           10 TYPE-LIEU     PIC X(2).
           10 TYPE-MVM      PIC X(2).
      
           EXEC SQL
             DECLARE CUR-PRMPDS CURSOR WITH HOLD FOR
               SELECT TYPE_LIEU,
                      TYPE_MVM,
                 FROM TGMIPRM, TGMIPDS
                WHERE OBJET = 'DONSTAT'
                AND   NOM_DONSTAT = '????'
           END-EXEC.
      
       END PROGRAM EXECOUTPROC.
      