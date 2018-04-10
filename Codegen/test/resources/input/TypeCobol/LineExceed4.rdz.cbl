       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPgm.
       DATA DIVISION .
       working-storage section.
       01 ListeAppelTSUI TYPEDEF STRICT PUBLIC.
          05 NB-AppelTSUI               PIC 9(4).
          05 MAX-AppelTSUI              PIC 9(4) VALUE 20.
          05 ListeAppelTSUITab          OCCURS 20.
            10 AppelTSUI                TYPE AppelTSUI.

       01 AppelTSUI TYPEDEF STRICT PUBLIC.
          05 Nordre                     PIC X(2).
          05 Libcpt                     PIC X(50).
          05 Compteur                   PIC 9(15).
          05 NordreMetro                PIC X(2).

       01 W-ListeAppelTSUI              TYPE ListeAppelTSUI.
      
       procedure division.
           MOVE 0                          TO W-ListeAppelTSUI
                                              ::AppelTSUI::Nordre(1)
           .
       END PROGRAM MyPgm.
      