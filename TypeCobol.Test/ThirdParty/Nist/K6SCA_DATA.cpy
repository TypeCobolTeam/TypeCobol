001100 FILE SECTION.                                                    K6SCA4.2
001200 FD  PRINT-FILE.                                                  K6SCA4.2
001300 01  PRINT-REC PICTURE X(120).                                    K6SCA4.2
001400 01  DUMMY-RECORD PICTURE X(120).                                 K6SCA4.2
001500 WORKING-STORAGE SECTION.                                         K6SCA4.2
001600 01  TEST-RESULTS.                                                K6SCA4.2
001700     02 FILLER                   PIC X      VALUE SPACE.          K6SCA4.2
001800     02 FEATURE                  PIC X(20)  VALUE SPACE.          K6SCA4.2
001900     02 FILLER                   PIC X      VALUE SPACE.          K6SCA4.2
002000     02 P-OR-F                   PIC X(5)   VALUE SPACE.          K6SCA4.2
002100     02 FILLER                   PIC X      VALUE SPACE.          K6SCA4.2
002200     02  PAR-NAME.                                                K6SCA4.2
002300       03 FILLER                 PIC X(19)  VALUE SPACE.          K6SCA4.2
002400       03  PARDOT-X              PIC X      VALUE SPACE.          K6SCA4.2
002500       03 DOTVALUE               PIC 99     VALUE ZERO.           K6SCA4.2
002600     02 FILLER                   PIC X(8)   VALUE SPACE.          K6SCA4.2
002700     02 RE-MARK                  PIC X(61).                       K6SCA4.2
002800 01  TEST-COMPUTED.                                               K6SCA4.2
002900     02 FILLER                   PIC X(30)  VALUE SPACE.          K6SCA4.2
003000     02 FILLER                   PIC X(17)  VALUE                 K6SCA4.2
003100            "       COMPUTED=".                                   K6SCA4.2
003200     02 COMPUTED-X.                                               K6SCA4.2
003300     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          K6SCA4.2
003400     03 COMPUTED-N               REDEFINES COMPUTED-A             K6SCA4.2
003500                                 PIC -9(9).9(9).                  K6SCA4.2
003600     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         K6SCA4.2
003700     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     K6SCA4.2
003800     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     K6SCA4.2
003900     03       CM-18V0 REDEFINES COMPUTED-A.                       K6SCA4.2
004000         04 COMPUTED-18V0                    PIC -9(18).          K6SCA4.2
004100         04 FILLER                           PIC X.               K6SCA4.2
004200     03 FILLER PIC X(50) VALUE SPACE.                             K6SCA4.2
004300 01  TEST-CORRECT.                                                K6SCA4.2
004400     02 FILLER PIC X(30) VALUE SPACE.                             K6SCA4.2
004500     02 FILLER PIC X(17) VALUE "       CORRECT =".                K6SCA4.2
004600     02 CORRECT-X.                                                K6SCA4.2
004700     03 CORRECT-A                  PIC X(20) VALUE SPACE.         K6SCA4.2
004800     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      K6SCA4.2
004900     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         K6SCA4.2
005000     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     K6SCA4.2
005100     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     K6SCA4.2
005200     03      CR-18V0 REDEFINES CORRECT-A.                         K6SCA4.2
005300         04 CORRECT-18V0                     PIC -9(18).          K6SCA4.2
005400         04 FILLER                           PIC X.               K6SCA4.2
005500     03 FILLER PIC X(2) VALUE SPACE.                              K6SCA4.2
005600     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     K6SCA4.2
005700 01  CCVS-C-1.                                                    K6SCA4.2
005800     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAK6SCA4.2
005900-    "SS  PARAGRAPH-NAME                                          K6SCA4.2
006000-    "       REMARKS".                                            K6SCA4.2
006100     02 FILLER                     PIC X(20)    VALUE SPACE.      K6SCA4.2
006200 01  CCVS-C-2.                                                    K6SCA4.2
006300     02 FILLER                     PIC X        VALUE SPACE.      K6SCA4.2
006400     02 FILLER                     PIC X(6)     VALUE "TESTED".   K6SCA4.2
006500     02 FILLER                     PIC X(15)    VALUE SPACE.      K6SCA4.2
006600     02 FILLER                     PIC X(4)     VALUE "FAIL".     K6SCA4.2
006700     02 FILLER                     PIC X(94)    VALUE SPACE.      K6SCA4.2
006800 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       K6SCA4.2
006900 01  REC-CT                        PIC 99       VALUE ZERO.       K6SCA4.2
007000 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       K6SCA4.2
007100 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       K6SCA4.2
007200 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       K6SCA4.2
007300 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       K6SCA4.2
007400 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       K6SCA4.2
007500 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       K6SCA4.2
007600 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      K6SCA4.2
007700 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       K6SCA4.2
007800 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     K6SCA4.2
007900 01  CCVS-H-1.                                                    K6SCA4.2
008000     02  FILLER                    PIC X(39)    VALUE SPACES.     K6SCA4.2
008100     02  FILLER                    PIC X(42)    VALUE             K6SCA4.2
008200     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 K6SCA4.2
008300     02  FILLER                    PIC X(39)    VALUE SPACES.     K6SCA4.2
008400 01  CCVS-H-2A.                                                   K6SCA4.2
008500   02  FILLER                        PIC X(40)  VALUE SPACE.      K6SCA4.2
008600   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  K6SCA4.2
008700   02  FILLER                        PIC XXXX   VALUE             K6SCA4.2
008800     "4.2 ".                                                      K6SCA4.2
008900   02  FILLER                        PIC X(28)  VALUE             K6SCA4.2
009000            " COPY - NOT FOR DISTRIBUTION".                       K6SCA4.2
009100   02  FILLER                        PIC X(41)  VALUE SPACE.      K6SCA4.2
009200                                                                  K6SCA4.2
009300 01  CCVS-H-2B.                                                   K6SCA4.2
009400   02  FILLER                        PIC X(15)  VALUE             K6SCA4.2
009500            "TEST RESULT OF ".                                    K6SCA4.2
009600   02  TEST-ID                       PIC X(9).                    K6SCA4.2
009700   02  FILLER                        PIC X(4)   VALUE             K6SCA4.2
009800            " IN ".                                               K6SCA4.2
009900   02  FILLER                        PIC X(12)  VALUE             K6SCA4.2
010000     " HIGH       ".                                              K6SCA4.2
010100   02  FILLER                        PIC X(22)  VALUE             K6SCA4.2
010200            " LEVEL VALIDATION FOR ".                             K6SCA4.2
010300   02  FILLER                        PIC X(58)  VALUE             K6SCA4.2
010400     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".K6SCA4.2
010500 01  CCVS-H-3.                                                    K6SCA4.2
010600     02  FILLER                      PIC X(34)  VALUE             K6SCA4.2
010700            " FOR OFFICIAL USE ONLY    ".                         K6SCA4.2
010800     02  FILLER                      PIC X(58)  VALUE             K6SCA4.2
010900     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".K6SCA4.2
011000     02  FILLER                      PIC X(28)  VALUE             K6SCA4.2
011100            "  COPYRIGHT   1985 ".                                K6SCA4.2
011200 01  CCVS-E-1.                                                    K6SCA4.2
011300     02 FILLER                       PIC X(52)  VALUE SPACE.      K6SCA4.2
011400     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              K6SCA4.2
011500     02 ID-AGAIN                     PIC X(9).                    K6SCA4.2
011600     02 FILLER                       PIC X(45)  VALUE SPACES.     K6SCA4.2
011700 01  CCVS-E-2.                                                    K6SCA4.2
011800     02  FILLER                      PIC X(31)  VALUE SPACE.      K6SCA4.2
011900     02  FILLER                      PIC X(21)  VALUE SPACE.      K6SCA4.2
012000     02 CCVS-E-2-2.                                               K6SCA4.2
012100         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      K6SCA4.2
012200         03 FILLER                   PIC X      VALUE SPACE.      K6SCA4.2
012300         03 ENDER-DESC               PIC X(44)  VALUE             K6SCA4.2
012400            "ERRORS ENCOUNTERED".                                 K6SCA4.2
012500 01  CCVS-E-3.                                                    K6SCA4.2
012600     02  FILLER                      PIC X(22)  VALUE             K6SCA4.2
012700            " FOR OFFICIAL USE ONLY".                             K6SCA4.2
012800     02  FILLER                      PIC X(12)  VALUE SPACE.      K6SCA4.2
012900     02  FILLER                      PIC X(58)  VALUE             K6SCA4.2
013000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".K6SCA4.2
013100     02  FILLER                      PIC X(13)  VALUE SPACE.      K6SCA4.2
013200     02 FILLER                       PIC X(15)  VALUE             K6SCA4.2
013300             " COPYRIGHT 1985".                                   K6SCA4.2
013400 01  CCVS-E-4.                                                    K6SCA4.2
013500     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      K6SCA4.2
013600     02 FILLER                       PIC X(4)   VALUE " OF ".     K6SCA4.2
013700     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      K6SCA4.2
013800     02 FILLER                       PIC X(40)  VALUE             K6SCA4.2
013900      "  TESTS WERE EXECUTED SUCCESSFULLY".                       K6SCA4.2
014000 01  XXINFO.                                                      K6SCA4.2
014100     02 FILLER                       PIC X(19)  VALUE             K6SCA4.2
014200            "*** INFORMATION ***".                                K6SCA4.2
014300     02 INFO-TEXT.                                                K6SCA4.2
014400       04 FILLER                     PIC X(8)   VALUE SPACE.      K6SCA4.2
014500       04 XXCOMPUTED                 PIC X(20).                   K6SCA4.2
014600       04 FILLER                     PIC X(5)   VALUE SPACE.      K6SCA4.2
014700       04 XXCORRECT                  PIC X(20).                   K6SCA4.2
014800     02 INF-ANSI-REFERENCE           PIC X(48).                   K6SCA4.2
014900 01  HYPHEN-LINE.                                                 K6SCA4.2
015000     02 FILLER  PIC IS X VALUE IS SPACE.                          K6SCA4.2
015100     02 FILLER  PIC IS X(65)    VALUE IS "************************K6SCA4.2
015200-    "*****************************************".                 K6SCA4.2
015300     02 FILLER  PIC IS X(54)    VALUE IS "************************K6SCA4.2
015400-    "******************************".                            K6SCA4.2
015500 01  CCVS-PGM-ID                     PIC X(9)   VALUE             K6SCA4.2
015600     "K6SCA".                                                     K6SCA4.2
