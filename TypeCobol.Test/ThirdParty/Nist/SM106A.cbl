000100 IDENTIFICATION DIVISION.                                         SM1064.2
000200 PROGRAM-ID.                                                      SM1064.2
000300     SM106A.                                                      SM1064.2
000400****************************************************************  SM1064.2
000500*                                                              *  SM1064.2
000600*    VALIDATION FOR:-                                          *  SM1064.2
000700*                                                              *  SM1064.2
000800*    "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".SM1064.2
000900*                                                              *  SM1064.2
001000*    "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".SM1064.2
001100*                                                              *  SM1064.2
001200****************************************************************  SM1064.2
001300*                                                              *  SM1064.2
001400*      X-CARDS USED BY THIS PROGRAM ARE :-                     *  SM1064.2
001500*                                                              *  SM1064.2
001600*        X-55  - SYSTEM PRINTER NAME.                          *  SM1064.2
001700*        X-82  - SOURCE COMPUTER NAME.                         *  SM1064.2
001800*        X-83  - OBJECT COMPUTER NAME.                         *  SM1064.2
001900*                                                              *  SM1064.2
002000****************************************************************  SM1064.2
002100 ENVIRONMENT DIVISION.                                            SM1064.2
002200*                                                                 SM1064.2
002300*********************** COPY STATEMENT USED **********************SM1064.2
002400*                                                                 SM1064.2
002500*COPY   K6SCA                                                     SM1064.2
002600*                                                                 SM1064.2
000000* Differs from original test because of #2318                     SM1064.2
000000* Current parser does not handle scan state change between files  SM1064.2
002700******************** COPIED TEXT BEGINS BELOW ********************SM1064.2
000100 CONFIGURATION SECTION.                                           K6SCA4.2
000200 SOURCE-COMPUTER.                                                 K6SCA4.2
000300     XXXXX082.                                                    K6SCA4.2
000400 OBJECT-COMPUTER.                                                 K6SCA4.2
000500     XXXXX083.                                                    K6SCA4.2
000600 INPUT-OUTPUT SECTION.                                            K6SCA4.2
000700 FILE-CONTROL.                                                    K6SCA4.2
000800     SELECT PRINT-FILE ASSIGN TO                                  K6SCA4.2
000900     XXXXX055.                                                    K6SCA4.2
000000 DATA DIVISION.                                                   SM1064.2
000000 COPY K6SCA_DATA.                                                 SM1064.2
000000 PROCEDURE DIVISION.                                              SM1064.2
000000 COPY K6SCA_PROC.                                                 SM1064.2
002900*********************** END OF COPIED TEXT ***********************SM1064.2
