       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0SOM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *Ok
           select F1-456789012345678901234567890  assign to UT-S-ENT001.
      *Ko
           select F3-4567890123456789012345678901 assign to UT-S-ENT002.

       data division.
       file section.
      *Ok
       FD  F1-456789012345678901234567890  block contains 0
                          recording mode F.
      *Ok
       01  F2-456789012345678901234567890    pic X(800).

      *Ko
       FD  F3-4567890123456789012345678901 block contains 0
                          recording mode F.
      *Ko
       01  F4-4567890123456789012345678901   pic X(1500).
       working-storage section.

      *Ok 30 chars
       01 V1-456789012345678901234567890 pic X.
      *Ok 30 chars
          88 V2-456789012345678901234567890 value 'X'.
      *KO 31 chars
       01 V3-4567890123456789012345678901 pic X.
      *KO 31 chars
          88 V4-4567890123456789012345678901 value 'X'.

         replace ==:toReplace:== by ==456789-123456789-1234567890==.
      *Ok
       01 V5-:toReplace: pic X.
      *Ko
       01 V5-:toReplace:1 pic X.

         replace off.

       procedure division.
      *Ok
       P1-456789012345678901234567890.
           exit.
      *Ko
       P2-4567890123456789012345678901.
           exit.

      *Ok
       S1-456789012345678901234567890 section.
           exit.
      *Ko
       S2-4567890123456789012345678901 section.
           exit. 

       IDENTIFICATION DIVISION.
      *Ok
       PROGRAM-ID. PGM1-6789012345678901234567890.
      *Ok
       END PROGRAM PGM1-6789012345678901234567890.

      *Ko
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM2-67890123456789012345678901.
      *Ko
       END PROGRAM PGM2-67890123456789012345678901.
       END PROGRAM DVZF0SOM.