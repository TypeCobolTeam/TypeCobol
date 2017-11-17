000010 IDENTIFICATION DIVISION.                                         000010
000020 PROGRAM-ID. Codegen.    
000031* If you want to write                                            000031
000032* a block-commented Haiku                            
      * you'll need three star signs ( ͡° ͜ʖ ͡°)                           000033
000040 DATA DIVISION.                                                   000040
000050 WORKING-STORAGE SECTION.                                         000050
001500 replace ==:PFX:==  by ==CCTFAL-LOG-PFX==                         000000
001510         ==:FATAL:==  by ==CCTFAL-LOG-LVL-F or TRC-F==            000000
001520         ==:INFO:==  by ==CCTFAL-LOG-LVL-I or TRC-I==             000000
001530         ==:TRAC:==  by ==CCTFAL-LOG-LVL-T or TRC-T==.            000000
000060 COPY Codegen-FirstCopy.                                          000060
      *01  MyKey    TYPEDEF strict PIC X(04).
      *01  MyKey2    TYPEDEF strict.
      *    05 partA pic X(04).
      *    05 partB pic 9(04).
      *    05 partC pic X(05).
      *01  b TYPE Bool.
000080 01  b-value PIC X VALUE LOW-VALUE.
           88  b       VALUE 'T'.
           88  b-false VALUE 'F'.
                                                                        000080
000090 01  mykey  PIC X(04). COPY Codegen-SECONDCOPY SUPPRESS.          000090
002440 77 myconstant PIC X(10) value 'SHIBBOLEET'.                      002440
000100 01  result PIC 9(32).                                            000100

000140 PROCEDURE DIVISION.                                              000140
000150                                                                  000150
      *DECLARE FUNCTION GetValue PRIVATE
      *                 INPUT     ikey   TYPE MyKey
      *                           ikey2  TYPE MyKey2
      *                 RETURNING result PIC 9(32).
                                                                        000190
                                                                        000200
004010     if b                                                         000000
004020       CONTINUE                                                   000000
004090     end-if                                                       000000
      *   SET b TO FALSE
000210    SET b-false TO TRUE
                                                                        000210
000221     MOVE 'TOTO'                                                  000221
000222             TO                                                   000222
000223               mykey                                              000223
000230*   MOVE GetValue(mykey) TO result.                               000230
000240    GOBACK.                                                       000240
00024E    EJECT                                                         00024E
000250 
000260 END PROGRAM Codegen.                                             000260
      *
      *DECLARE FUNCTION GetValue PRIVATE
      *                 INPUT     ikey   TYPE MyKey
      *                           ikey2  TYPE MyKey2
      *                 RETURNING result PIC 9(32).
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. c0ca92c2GetValue.
       DATA DIVISION.
       LINKAGE SECTION.
       01 ikey PIC X(04).
       01 ikey2.
           02 partA pic X(04).
           02 partB pic 9(04).
           02 partC pic X(05).
       01 result PIC 9(32).
       PROCEDURE DIVISION
             USING BY REFERENCE ikey
                   BY REFERENCE ikey2
                   BY REFERENCE result
           .
000170                                                                  000170
000180     display "test codegen"
000180     CONTINUE.                                                    000180
       END PROGRAM c0ca92c2GetValue.
