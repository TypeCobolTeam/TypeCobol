       IDENTIFICATION DIVISION.
       PROGRAM-ID. TypeDefLevel88.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 ProcessingMode2 typedef strict pic X(02).
              88 RealTime value "TP".
              88 IMSBatch value "TI".
              88 DB2Batch value "TD".
              88 Batch    value "  ".
              88 Others123456   values "a1" thru "a9"
                                 "b1" thru "b9"
                                 "c1" thru "c9"
                                 "d1" thru "d9".

       01 ProcessingMode typedef strict.
           05 temp pic X(02).
              88 RealTime value "TP".
              88 IMSBatch value "TI".
              88 DB2Batch value "TD".
              88 Batch    value "  ".

       01  MyPM type ProcessingMode.
       01  MyPM2 type ProcessingMode2.

       PROCEDURE DIVISION.
           IF (MyPM::temp::IMSBatch = "TI" AND MyPM2::DB2Batch = "TD")
             DISPLAY "TI and TD"
          ELSE
             CONTINUE
          END-IF.
       END PROGRAM TypeDefLevel88.
