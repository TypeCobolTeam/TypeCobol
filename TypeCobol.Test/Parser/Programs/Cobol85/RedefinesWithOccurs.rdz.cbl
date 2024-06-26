       IDENTIFICATION DIVISION.
       PROGRAM-ID. RedefinesWithOccurs.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *********************************************
      *A. Single element, redefines with occurs 1
      *********************************************
      
       01 Group-A.
      *A "PICTURE" clause was not found for elementary item "SIMPLE-KO".
      *"PICTURE X(1)" was assumed.
           05 simple-KO.
      *A "PICTURE" clause was not found for elementary item
      *"SIMPLE-KO-R". "PICTURE X(1)" was assumed.
           05 simple-KO-R redefines simple-KO.      
      
      
      
      *Ok
           05 Array-A01-OK pic X.
           05 Array-A01-OK-R redefines Array-A01-OK OCCURS 1 pic X.
      
      
      
      *A"PICTURE"clause was not found for elementary item"Array-A02-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A02-KO.
      
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A02-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A02-KO-R redefines Array-A02-KO OCCURS 1.
      
      
      
      
      *A"PICTURE"clause was not found for elementary item"Array-A03-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A03-KO usage is display.
      
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A03-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A03-KO-R redefines Array-A03-KO OCCURS 1 usage is
           display.
      
      
      *The "PICTURE" clause for item "ARRAY-A04-KO" was not compatible
      *with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A04-KO". "PICTURE X(1)" was assumed.
           05 Array-A04-KO usage is display-1.
      *The "PICTURE" clause for item "ARRAY-A04-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A04-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A04-KO-R redefines Array-A04-KO OCCURS 1 usage is
           display-1.
      
      
           05 Array-A05-OK usage is index.
           05 Array-A05-OK-R redefines Array-A05-OK OCCURS 1 usage is
           index.
      
      *The"PICTURE"clause for item"Array-A06-KO"was not compatible with
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A06-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A06-KO usage is national.
      *The "PICTURE" clause for item "ARRAY-A06-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A06-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A06-KO-R redefines Array-A06-KO OCCURS 1 usage is
           national.
      
      
      
      *TODO Wait for support of UTF-8 (#2504)
      *The"PICTURE"clause for item"Array-A07-KO"was not compatible with
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A07-KO".
      *   "PICTURE X(1)"was assumed.
      *   05 Array-A07-KO usage is utf-8.
      *   05 Array-A07-KO-R redefines Array-A07-KO  OCCURS 1 usage is
      *   utf-8.
      
      
      *The"PICTURE"clause for item"Array-A08-KO"was not compatible with
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A08-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A08-KO binary.
      *The "PICTURE" clause for item "ARRAY-A08-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A08-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A08-KO-R redefines Array-A08-KO  OCCURS 1 binary.
      
      
      
      
      *The"PICTURE"clause for item"Array-A09-KO"was not compatible with
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A09-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A09-KO packed-decimal.
      *The "PICTURE" clause for item "ARRAY-A09-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A09-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A09-KO-R redefines Array-A09-KO OCCURS 1
           packed-decimal.
      
      
      *The"PICTURE"clause for item"Array-A10-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A10-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A10-KO comp.
      *The "PICTURE" clause for item "ARRAY-A10-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A10-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A10-KO-R redefines Array-A10-KO OCCURS 1 comp.
      
      
      
           05 Array-A11-OK comp-1.
           05 Array-A11-OK-R redefines Array-A11-OK OCCURS 1 comp-1.
      
      
           05 Array-A12-OK comp-2.
           05 Array-A12-OK-R redefines Array-A12-OK OCCURS 1 comp-2.
      
      
      *The"PICTURE"clause for item"Array-A13-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A13-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A13-KO comp-3.
      *The "PICTURE" clause for item "ARRAY-A13-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A13-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A13-KO-R redefines Array-A13-KO OCCURS 1 comp-3.
      
      
      
      
      *The"PICTURE"clause for item"Array-A14-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A14-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A14-KO comp-4.
      *The "PICTURE" clause for item "ARRAY-A14-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A14-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A14-KO-R redefines Array-A14-KO OCCURS 1 comp-4.
      
      
      
      
      *The"PICTURE"clause for item"Array-A15-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A15-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A15-KO comp-5.
      
      *The "PICTURE" clause for item "ARRAY-A15-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A15-KO-R". "PICTURE X(1)" was assumed.
           05 Array-A15-KO-R redefines Array-A15-KO OCCURS 1 comp-5.
      
      
           05 Array-A16-OK pointer.
           05 Array-A16-OK-R redefines Array-A16-OK OCCURS 1 pointer.
      
      
           05 Array-A17-OK pointer-32.
           05 Array-A17-OK-R redefines Array-A17-OK OCCURS 1 pointer-32.
      
           05 Array-A18-OK procedure-pointer.
           05 Array-A18-OK-R redefines Array-A18-OK OCCURS 1
           procedure-pointer.
      
           05 Array-A19-OK function-pointer.
           05 Array-A19-OK-R redefines Array-A19-OK OCCURS 1
           function-pointer.
      
      
      
      *********************************************
      *B. Same thing but usage is declared on parent
      *********************************************
      
       01 Group-B01 usage is display.
      *A"PICTURE"clause was not found for elementary item"Array-B01-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-B01-KO.
      
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-B01-KO-R". "PICTURE X(1)" was assumed.
           05 Array-B01-KO-R redefines Array-B01-KO OCCURS 1.
      
      
      
      
       01 Group-B02 usage is display-1.
      *The"PICTURE"clause for item"Array-B02-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B02-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-B02-KO.
      
      *The "PICTURE" clause for item "ARRAY-B02-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-B02-KO-R". "PICTURE X(1)" was assumed.
           05 Array-B02-KO-R redefines Array-B02-KO OCCURS 1.
      
      
      
      
       01 Group-B03 usage is index.
           05 Array-B03-OK.
           05 Array-B03-OK-R redefines Array-B03-OK OCCURS 1.
      
       01 Group-B04 usage is national.
      *The"PICTURE"clause for item"Array-B04-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B04-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-B04-KO.
      
      *The "PICTURE" clause for item "ARRAY-B04-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-B04-KO-R". "PICTURE X(1)" was assumed.
           05 Array-B04-KO-R redefines Array-B04-KO occurs 1.
      
      
      
      
      *TODO Wait for support of UTF-8 (#2504)
      *01 Group-B05 usage is utf-8.
      *The"PICTURE"clause for item"Array-B05-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B05-KO".
      *   "PICTURE X(1)"was assumed.
      *    05 Array-B05-KO.
      *    05 Array-B05-KO-R redefines Array-B05-KO OCCURS 1.
      
      
       01 Group-B06 binary.
      *The"PICTURE"clause for item"Array-B06-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B06-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B06-KO.
      
      *The "PICTURE" clause for item "ARRAY-B06-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-B06-KO-R". "PICTURE X(1)" was assumed.
            05 Array-B06-KO-R redefines Array-B06-KO OCCURS 1.
      
      
      
      
       01 Group-B07 packed-decimal.
      *The"PICTURE"clause for item"Array-B07-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B07-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B07-KO.
      
      *The "PICTURE" clause for item "ARRAY-B07-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-B07-KO-R". "PICTURE X(1)" was assumed.
            05 Array-B07-KO-R redefines Array-B07-KO OCCURS 1.
      
      
      
      
       01 Group-B08 comp.
      *The"PICTURE"clause for item"Array-B08-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B08-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B08-KO.
      
      *The "PICTURE" clause for item "ARRAY-B08-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-B08-KO-R". "PICTURE X(1)" was assumed.
            05 Array-B08-KO-R redefines Array-B08-KO OCCURS 1.
      
      
      
      
       01 Group-B09 comp-1.
            05 Array-B09-OK.
            05 Array-B09-OK-R redefines Array-B09-OK OCCURS 1.
      
       01 Group-B10 comp-2.
            05 Array-B10-OK.
            05 Array-B10-OK-R redefines Array-B10-OK OCCURS 1.
      
      
       01 Group-B11 comp-3.
      *The"PICTURE"clause for item"Array-B11-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B11-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B11-KO.
      *The "PICTURE" clause for item "ARRAY-B11-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-B11-KO-R". "PICTURE X(1)" was assumed.
            05 Array-B11-KO-R redefines Array-B11-KO OCCURS 1.
      
      
      
       01 Group-B12 comp-4.
      *The"PICTURE"clause for item"Array-B12-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B12-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B12-KO.
      *The "PICTURE" clause for item "ARRAY-B12-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-B12-KO-R". "PICTURE X(1)" was assumed.
            05 Array-B12-KO-R redefines Array-B12-KO OCCURS 1.
      
      
      
      
       01 Group-B13 comp-5.
      *The"PICTURE"clause for item"Array-B13-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B13-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B13-KO.
      *The "PICTURE" clause for item "ARRAY-B13-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-B13-KO-R". "PICTURE X(1)" was assumed.
            05 Array-B13-KO-R redefines Array-B13-KO OCCURS 1.
      
      
       01 Group-B14 pointer.
            05 Array-B14-OK.
            05 Array-B14-OK-R redefines Array-B14-OK OCCURS 1.
      
       01 Group-B15 pointer-32.
            05 Array-B15-OK.
            05 Array-B15-OK-R redefines Array-B15-OK OCCURS 1.
      
      
       01 Group-B16 procedure-pointer.
            05 Array-B16-OK.
            05 Array-B16-OK-R redefines Array-B16-OK OCCURS 1.
      
      
       01 Group-B17 function-pointer.
            05 Array-B17-OK.
            05 Array-B17-OK-R redefines Array-B17-OK OCCURS 1.
      
      
      
      *********************************************
      *C. group-usage is only national or UTF-8
      *********************************************
      
      *The"PICTURE"clause for item"Array-C01-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-C01-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group-C01 group-usage is national.
           05 Array-C01-KO.
      
      
      *The "PICTURE" clause for item "ARRAY-C01-KO-R" was not compatible
      *  with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-C01-KO-R". "PICTURE X(1)" was assumed.
           05 Array-C01-KO-R redefines Array-C01-KO OCCURS 1.
      
      
      *TODO Wait for support of UTF-8 (#2504)
      *01 Group-C02 group-usage is UTF-8.
      *    05 Array-C02-KO.
      *    05 Array-C02-KO-R redefines Array-C02-KO OCCURS 1.
      
      
      *********************************************
      *D. Usage is not declared directly on parent (test recursivity)
      *********************************************
      
       01 Group-D1 usage is pointer.
           05 Group-D11.
                10 Array-D1-OK.
                10 Array-D1-OK-R redefines Array-D1-OK OCCURS 1.
      
      
      *The"PICTURE"clause for item"ARRAY-D2-KO"was not compatible with
      *the specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"ARRAY-D2-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group-D2 comp-5.
           05 Group-D21.
                10 Array-D2-KO.
      *The "PICTURE" clause for item "ARRAY-D2-KO-R" was not compatible
      * with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-D2-KO-R". "PICTURE X(1)" was assumed.
                10 Array-D2-KO-R redefines Array-D2-KO OCCURS 1.
      
      
     
      
       01 Group-D3 usage is procedure-pointer.
           05 Group-D31.
                10 Group-D311.
                    15 Array-D3-OK.
                    15 Array-D3-OK-R redefines Array-D3-OK OCCURS 1.
      
      
       01 Group-D4 usage is display-1.
           05 Group-D41.
                10 Group-D411.
      
      *The"PICTURE"clause for item"ARRAY-D4-KO"was not compatible with
      *the specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"ARRAY-D4-KO".
      *   "PICTURE X(1)"was assumed.
                    15 Array-D4-KO.
      
      *The "PICTURE" clause for item "ARRAY-D4-KO-R" was not compatible
      * with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-D4-KO-R". "PICTURE X(1)" was assumed.
                    15 Array-D4-KO-R redefines Array-D4-KO OCCURS 1.
      
      
      
      
      
       01 Group-D5 usage is comp-1.
           05 Array-D5-OK.
      *Here 'group-usage is national' is not valid for IBM
      *because it is not compliant with 'usage comp-1' on parent.
      *As a result the children are not checked and only the 
      *following error is displayed:
      *The specified "USAGE" was different from the "USAGE" specified at
      *the group level.  The group "USAGE" was assumed for this item.
           05 Group-D51 group-usage is national.
      
                10 Group-D511.
      *Our parser does not check consistency between group-usage
      *definition and its parent.
      *As a result 'Array-D5-KO' inherits the usage from its parent
      *and 'national' is not considered as valid.
                    15 Array-D5-KO.
      *Idem
                    15 Array-D5-KO-R redefines Array-D5-KO OCCURS 1.
      
      
      
      *********************************************
      *E. Usage is not required because of children
      *********************************************
      
       01 Group-E1.
           05 Array-E1-OK.
      *A "PICTURE" clause was not found for elementary item "VARCHILD".
      *"PICTURE X(1)" was assumed.
                10 VarChild.
      
           05 Array-E1-OK-R redefines Array-E1-OK OCCURS 1.
      *A "PICTURE" clause was not found for elementary item "VARCHILD-R"
      *"PICTURE X(1)" was assumed.
                10 VarChild-R.
      
       END PROGRAM RedefinesWithOccurs.
      