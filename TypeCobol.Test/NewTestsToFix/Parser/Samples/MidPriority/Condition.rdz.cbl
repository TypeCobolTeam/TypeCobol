000000*Don't except any errors 
001540 IDENTIFICATION DIVISION.                                         00154000
001570 PROGRAM-ID.     MyPGM.
002090 DATA DIVISION.                                                   00209000
002860 WORKING-STORAGE SECTION.                                         00286000
005510 01                PIC X.
005520        88 ASCII                 VALUE X'01'.                     00552000
005530        88 EBCDIC                VALUE X'02'.                     00553000
032810 PROCEDURE DIVISION.                                              03281000
      * OK
143400     IF NOT (ASCII OR EBCDIC)                                     51780000
143410        DISPLAY 'Hello'
143470     END-IF                                                       51850000
143480     .                                                            51860000