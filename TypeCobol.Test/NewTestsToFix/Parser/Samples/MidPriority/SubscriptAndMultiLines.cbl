﻿*Don't except any errors
 IDENTIFICATION DIVISION.
 PROGRAM-ID. C6LIST1B.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  W-CPT-LUS                   PIC 9(06)    VALUE ZEROES.
 01  ENTREE-ENR                    PIC X(80).
*---------------------*
 PROCEDURE DIVISION.
     DISPLAY '>>> DOUBLON BIN: ' ENTREE-ENR(3:19)
-            ' ENREG NUM: ' W-CPT-LUS
     EXIT.