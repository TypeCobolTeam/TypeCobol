﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOZCHKP.
       data division.
       working-storage section.
       REPLACE
           ==:LONGUEUR-IDOFUS:==      BY ==0900==.
       01  var1         PIC X(:LONGUEUR-IDOFUS:).
      
       PROCEDURE DIVISION.
           move :LONGUEUR-IDOFUS: to var1
           .
      
       END PROGRAM TCOZCHKP.
