       IDENTIFICATION DIVISION.
       PROGRAM-ID. IndexTester.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 ZONE PIC X(5).
       01 TB-1.
           05 EL-1 PIC X(5) OCCURS 10 INDEXED BY IND-1.

       01 ZONEBIS PIC X(5).
       01 TB-2.
           05 EL-2 PIC X(5) OCCURS 10 INDEXED BY IND-2, IND-3.

       01 W-TabPCBNAME.
          05 W-PCBNAME OCCURS 3 INDEXED BY W-PCBNAME-Idc
                                PIC X(08).

       01 W-TabPCBAdr.
          05 W-PCBAdr  OCCURS 3 INDEXED BY W-PCBAdr-Idc
                                POINTER.

       PROCEDURE DIVISION.
      * OK : Test with 1 index
           MOVE EL-1(IND-1) TO ZONE  
           SET IND-1 TO 1

      * OK : Test with 2 Indexes    
           MOVE EL-2(IND-2) TO ZONEBIS  
           SET IND-2 TO 1
           SET IND-3 TO 1
           
      * All OK
           SET W-PCBNAME-Idc TO 1
           SET W-PCBNAME-Idc UP BY 1
           SET W-PCBAdr-Idc TO 1
           SET W-PCBAdr-Idc UP BY 1
           . 

       END PROGRAM IndexTester.