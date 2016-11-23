000010 IDENTIFICATION DIVISION.                                         000010
000020 PROGRAM-ID. Codegen.    
000031* If you want to write                                            000031
000032* a block-commented Haiku                            
      * you'll need three star signs                                    000033
000040 DATA DIVISION.                                                   000040
000050 WORKING-STORAGE SECTION.                                         000050
000060 COPY YFIRSTCOPY.                                                 000060
000070 01  MyKey    TYPEDEF PIC X(04).                                  000070
000080 01  b TYPE Bool.                                                 000080
000090 01  mykey  PIC X(04). COPY YSECONDCOPY.                          000090
000100 01  result PIC 9(32).                                            000100

000140 PROCEDURE DIVISION.                                              000140
000150                                                                  000150
000161 DECLARE FUNCTION GetValue PRIVATE                                000161
                        INPUT     ikey   TYPE MyKey
000163                  RETURNING result PIC 9(32).                     000163
000170 PROCEDURE DIVISION.                                              000170
000180     CONTINUE.                                                    000180
       END-DECLARE.                                                     000190
                                                                        000200
000210    SET b TO FALSE                                                000210
000221     MOVE 'TOTO'                                                  000221
000222             TO                                                   000222
000223               mykey                                              000223
000230    MOVE GetValue(mykey) TO result.                               000230
000240    CONTINUE.                                                     000240
000250 
000260 END PROGRAM Codegen.                                             000260