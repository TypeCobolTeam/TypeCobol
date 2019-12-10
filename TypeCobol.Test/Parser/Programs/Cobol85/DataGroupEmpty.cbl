IDENTIFICATION DIVISION.
PROGRAM-ID. TypeCobol.
DATA DIVISION.
WORKING-storage section.
01 STR1.
*ko MyGroup doesn't have any child
    05 MyGroup.
        COPY CPYData2.

01 STR2.
*ko MyGroup doesn't have any child
    05 MyGroup.

01 STR3.
*ok
    05 MyGroup.
        10 DATA1 PIC X.

01 STR4.
*ok
    04 MyGroup.
        COPY CPYData2.

copy CPYData. 

PROCEDURE DIVISION.

END PROGRAM TypeCobol.