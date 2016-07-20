000000*Don't except any errors
000020 IDENTIFICATION DIVISION.                                         0000000
000030 PROGRAM-ID. EnclosingPgm.
002680 PROCEDURE DIVISION                                              
003480     GOBACK                                                       0000000
003490     .
023460 IDENTIFICATION DIVISION.                                         0000000
023470 PROGRAM-ID. NestedPgm    IS COMMON.          
023480 DATA DIVISION.                                                   0000000
023490 LINKAGE SECTION.                                                 0000000
023500 01  myData  PIC X.
023520                                                                  0000000
023530 PROCEDURE DIVISION.
023570     GOBACK                                                       0000000
023580     .                                                            0000000
023590                                                                  0000000
023600 END PROGRAM TRANS-ADR-CLC.                                       0000000
023620 END PROGRAM EnclosingPgm.
