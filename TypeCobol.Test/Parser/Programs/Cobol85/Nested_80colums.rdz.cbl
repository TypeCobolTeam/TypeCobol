000010 IDENTIFICATION DIVISION.                                         0000000
000020 PROGRAM-ID. EnclosingPgm.                                        0000000
000030 PROCEDURE DIVISION.                                              0000000
000040           GOBACK.
000050                                                                  0000000
000060   IDENTIFICATION DIVISION.                                       0000000
000070   PROGRAM-ID. NestedPgm IS COMMON.                               0000000
000080   DATA DIVISION.                                                 0000000
000090   LINKAGE SECTION.                                               0000000
000100   01 x PIC X.
000110   PROCEDURE DIVISION.                                            0000000
000120           GOBACK.
000130   END PROGRAM NestedPgm.                                         0000000
000140                                                                  0000000
000150 END PROGRAM EnclosingPgm.                                        0000000
