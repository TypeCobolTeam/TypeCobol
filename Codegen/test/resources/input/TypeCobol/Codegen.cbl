000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. Codegen.
000030* If you want to write
000040* a block-commented Haiku
      * you'll need three star signs
000060 DATA DIVISION.
000070 WORKING-STORAGE SECTION.
000080 COPY YFIRSTCOPY.
000090 01  MyKey    TYPEDEF PIC X(04).
000100 01  b TYPE Bool.                                                 ENDBOO
000110 01  mykey  PIC X(04). COPY YSECONDCOPY.
000120 01  result PIC 9(32).                                            ENDDEF

000140 PROCEDURE DIVISION.
000150 
000160 DECLARE FUNCTION GetValue PRIVATE
                        INPUT     ikey   TYPE MyKey
000180                  RETURNING result PIC 9(32).
000190 PROCEDURE DIVISION.
000200     CONTINUE.                                                    ENDDEL
       END-DECLARE.
       
000220    SET b TO FALSE
000230     MOVE 'TOTO' TO mykey
000240    MOVE GetValue(mykey) TO result.
000250    CONTINUE.                                                     ENDCON
000260 
000270 END PROGRAM Codegen.