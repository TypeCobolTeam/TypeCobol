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

*ok
copy CPYData. 

*ko
01 STR5.   COPY CPYData3 REPLACING ==:MyCopy:== BY ==MyCopy==.

*ko
01 STR6.   
    05 MyGroup. 
        COPY CPYData4  REPLACING ==:MyCopy:== BY ==MyCopy==.

*ok
01 STR7.   COPY CPYData4 REPLACING ==:MyCopy:== BY ==MyCopy==.

PROCEDURE DIVISION.

END PROGRAM TypeCobol.