       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           Select Out001        Assign To SOR001.
           Select AmbiguousName Assign To SOR001.
       DATA DIVISION.
       FILE SECTION.
       SD Out001.
       01 Out001Data PIC X(200).
       SD AmbiguousName.
       01 AmbiguousNameData PIC X(200).
       WORKING-STORAGE SECTION.
       01 tab1.
          05 a OCCURS 20.
             10 item-a PIC X.
       01 AmbiguousName PIC X.
       PROCEDURE DIVISION.
      *KO unable to resolve SORT target
           SORT something-not-defined
      *KO ambiguous reference
           SORT AmbiguousName
      *KO SORT file without KEY
           SORT Out001 INPUT PROCEDURE  inputProc
                       OUTPUT PROCEDURE outputProc
      *KO SORT file without input
           SORT Out001 ASCENDING Out001Data
                       OUTPUT PROCEDURE outputProc
      *KO SORT file without output
           SORT Out001 ASCENDING Out001Data
                       INPUT PROCEDURE  inputProc
      *KO SORT table with input
           SORT a ASCENDING item-a
                  INPUT PROCEDURE  inputProc
      *KO SORT table with output
           SORT a ASCENDING item-a
                  OUTPUT PROCEDURE outputProc
      *KO SORT table without KEY
           SORT a
      *Ok, this is a valid SORT file
           SORT Out001 ASCENDING Out001Data
                       INPUT PROCEDURE  inputProc
                       OUTPUT PROCEDURE outputProc
      *Ok, this is a valid SORT table
           SORT a ASCENDING item-a
           GOBACK
           .
       inputProc.
           DISPLAY "input proc"
           .
       outputProc.
           DISPLAY "output proc"
           .
       END PROGRAM TCOMFL06.