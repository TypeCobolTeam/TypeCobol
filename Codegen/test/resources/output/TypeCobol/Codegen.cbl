000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. Codegen.
000030* If you want to write
000040* a block-commented Haiku
      * you'll need three star signs
000060 DATA DIVISION.
000070 WORKING-STORAGE SECTION.
000080 COPY YFIRSTCOPY.
000090*01  MyKey    TYPEDEF PIC X(04).
000100*01  b TYPE Bool.                                                 ENDBOO
101010 01  b-value PIC X VALUE LOW-VALUE.
101010     88  b       VALUE 'T'.
101010     88  b-false VALUE 'F'.
000110 01  mykey  PIC X(04). COPY YSECONDCOPY.
000120 01  result PIC 9(32).                                            ENDDEF
101010 
000140 PROCEDURE DIVISION.
000150 
000160*DECLARE FUNCTION GetValue PRIVATE
      *                 INPUT     ikey   TYPE MyKey
000180*                 RETURNING result PIC 9(32).
       
000220*   SET b TO FALSE
101010    SET b-false TO TRUE
000230     MOVE 'TOTO' TO mykey
101010    MOVE GetValue(mykey) TO result.
000250    CONTINUE. 
000260 
000270 END PROGRAM Codegen.
101010*_________________________________________________________________
101010 IDENTIFICATION DIVISION.
101010 PROGRAM-ID. f79c1034.
101010 DATA DIVISION.
101010 LINKAGE SECTION.
101010 01 ikey.
101010 01 result PIC 9(32).
101010 PROCEDURE DIVISION
101010       USING BY REFERENCE ikey
101010             BY REFERENCE result
101010     .
101010     CONTINUE. 
101010 END PROGRAM f79c1034.
