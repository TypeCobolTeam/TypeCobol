IDENTIFICATION DIVISION.
PROGRAM-ID. EnclosingPgm.
DATA DIVISION.
LOCAL-STORAGE SECTION.

01 MyGlobalVar PIC X(5) GLOBAL.
01 MyNotGlobalVar PIC X(5).

01 MyGlobalGroup GLOBAL.
  05 MGG1.
    10 MGG2 PIC X(5).
  05 MGGDate PIC 9(8).

01 MyNotGlobalGroup.
  05 MNGG1.
    10 MNGG2 PIC X(5).
  05 MNGGDate PIC 9(8).

PROCEDURE DIVISION.

  IDENTIFICATION DIVISION.
  PROGRAM-ID. NestedPgm.

  DATA DIVISION.
    LOCAL-STORAGE SECTION.

      01 innerVar PIC X(8).

   PROCEDURE DIVISION.
   
     move MyGlobalVar               to innerVar.

     move MyNotGlobalVar            to innerVar.

     move MyGlobalGroup             to innerVar.
     move MGG1 OF MyGlobalGroup     to innerVar.
     move MGG2                      to innerVar.
     move MGGDate                   to innerVar.

     move MyNotGlobalGroup          to innerVar.
     move MNGG1 OF MyNotGlobalGroup to innerVar.
     move MNGG2                     to innerVar.
     move MNGGDate                  to innerVar.

  END PROGRAM NestedPgm.

END PROGRAM EnclosingPgm.
