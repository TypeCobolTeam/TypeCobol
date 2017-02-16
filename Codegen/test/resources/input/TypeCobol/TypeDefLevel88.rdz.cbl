       IDENTIFICATION DIVISION.
       PROGRAM-ID. TypeDefLevel88.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 ProcessingMode2 typedef pic X(02).
              88 RealTime value "TP".
              88 IMSBatch value "TI".
              88 DB2Batch value "TD".
              88 Batch    value "  ".

       01 ProcessingMode typedef .
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
