       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm2358.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *********************************************
      *A. Usage is declared on array
      *********************************************

       01 Group-A.
      *Ok
           05 Array-A01-OK OCCURS 50 pic X.

      *A"PICTURE"clause was not found for elementary item"Array-A02-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A02-KO OCCURS 50.
      *A"PICTURE"clause was not found for elementary item"Array-A03-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A03-KO OCCURS 50 usage is display.
      *The"PICTURE"clause for item"Array-A04-KO"was not compatible with the
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A04-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A04-KO OCCURS 50 usage is display-1.
           05 Array-A05-OK OCCURS 50 usage is index.

      *The"PICTURE"clause for item"Array-A06-KO"was not compatible with the
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A06-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A06-KO OCCURS 50 usage is national.

      *TODO Wait for support of UTF-8 (#2504)
      *The"PICTURE"clause for item"Array-A07-KO"was not compatible with the
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A07-KO".
      *   "PICTURE X(1)"was assumed.
      *   05 Array-A07-KO OCCURS 50 usage is utf-8.


      *The"PICTURE"clause for item"Array-A08-KO"was not compatible with the
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A08-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A08-KO OCCURS 50 binary.
      *The"PICTURE"clause for item"Array-A09-KO"was not compatible with the
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A09-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A09-KO OCCURS 50 packed-decimal.
      *The"PICTURE"clause for item"Array-A10-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A10-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A10-KO OCCURS 50 comp.
           05 Array-A11-OK OCCURS 50 comp-1.
           05 Array-A12-OK OCCURS 50 comp-2.

      *The"PICTURE"clause for item"Array-A13-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A13-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A13-KO OCCURS 50 comp-3.
      *The"PICTURE"clause for item"Array-A14-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A14-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A14-KO OCCURS 50 comp-4.
      *The"PICTURE"clause for item"Array-A15-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A15-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A15-KO OCCURS 50 comp-5.
           05 Array-A16-OK OCCURS 50 pointer.
           05 Array-A17-OK OCCURS 50 pointer-32.
           05 Array-A18-OK OCCURS 50 procedure-pointer.
           05 Array-A19-OK OCCURS 50 function-pointer.

      *A"PICTURE"clause was not found for elementary item"ARRAY-A20-KO".
      *   "PICTURE X(1)"was assumed.
           05 Counter pic 9.
           05 Array-A20-KO occurs 99 depending on Counter of Group-A
                             indexed by MyIdxCustom.

      *********************************************
      *B. Same thing but usage is declared on parent
      *********************************************

       01 Group-B01 usage is display.
      *A"PICTURE"clause was not found for elementary item"Array-B01-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B01-KO OCCURS 50.

       01 Group-B02 usage is display-1.
      *The"PICTURE"clause for item"Array-B02-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B02-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B02-KO OCCURS 50.
       01 Group-B03 usage is index.
            05 Array-B03-OK OCCURS 50.
       01 Group-B04 usage is national.
      *The"PICTURE"clause for item"Array-B04-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B04-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B04-KO OCCURS 50.

      *TODO Wait for support of UTF-8 (#2504)
      *01 Group-B05 usage is utf-8.
      *The"PICTURE"clause for item"Array-B05-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B05-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B05-KO OCCURS 50.
       01 Group-B06 binary.
      *The"PICTURE"clause for item"Array-B06-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B06-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B06-KO OCCURS 50.

       01 Group-B07 packed-decimal.
      *The"PICTURE"clause for item"Array-B07-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B07-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B07-KO OCCURS 50.

       01 Group-B08 comp.
      *The"PICTURE"clause for item"Array-B08-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B08-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B08-KO OCCURS 50.
       01 Group-B09 comp-1.
            05 Array-B09-OK OCCURS 50.
       01 Group-B10 comp-2.
            05 Array-B10-OK OCCURS 50.

       01 Group-B11 comp-3.
      *The"PICTURE"clause for item"Array-B11-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B11-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B11-KO OCCURS 50.

       01 Group-B12 comp-4.
      *The"PICTURE"clause for item"Array-B12-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B12-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B12-KO OCCURS 50.

       01 Group-B13 comp-5.
      *The"PICTURE"clause for item"Array-B13-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B13-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B13-KO OCCURS 50.

       01 Group-B14 pointer.
            05 Array-B14-OK OCCURS 50.
       01 Group-B15 pointer-32.
            05 Array-B15-OK OCCURS 50.
       01 Group-B16 procedure-pointer.
            05 Array-B16-OK OCCURS 50.
       01 Group-B17 function-pointer.
            05 Array-B17-OK OCCURS 50.


      *********************************************
      *C. group-usage is only national or UTF-8
      *********************************************

      *The"PICTURE"clause for item"Array-C01-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-C01-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group-C01 group-usage is national.
           05 Array-C01-KO OCCURS 50.

      *TODO Wait for support of UTF-8 (#2504)
      *01 Group-C02 group-usage is UTF-8.
      *    05 Array-C02-KO OCCURS 50.


      *********************************************
      *D. Usage is not declared directly on parent (test recursivity)
      *********************************************

       01 Group-D1 usage is pointer.
           05 Group-D11.
                10 Array-D1-OK OCCURS 50.

      *The"PICTURE"clause for item"ARRAY-D2-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"ARRAY-D2-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group-D2 comp-5.
           05 Group-D21.
                10 Array-D2-KO OCCURS 50.

       01 Group-D3 usage is procedure-pointer.
           05 Group-D31.
                10 Group-D311.
                    15 Array-D3-OK OCCURS 50.


      *The"PICTURE"clause for item"ARRAY-D4-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"ARRAY-D4-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group-D4 usage is display-1.
           05 Group-D41.
                10 Group-D411.
                    15 Array-D4-KO OCCURS 50.


      *The"PICTURE"clause for item"ARRAY-D5-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"ARRAY-D5-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group-D5 usage is comp-1.
           05 Array-D5-OK OCCURS 50.
           05 Group-D51 group-usage is national.
                10 Group-D511.
                    15 Array-D5-KO OCCURS 50.


      *********************************************
      *E. Usage is not required because of children
      *********************************************

       01 Group-E1.
           05 Array-E1-OK OCCURS 50.
                10 VarChild PIC X.

       END PROGRAM Pgm2358.