       IDENTIFICATION DIVISION.
       PROGRAM-ID. RenamesAndUsage.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      *********************************************
      *A. Single element followed by renames
      *********************************************
      
       01 Group-A.
      *A "PICTURE" clause was not found for elementary item "SIMPLE-KO".
      *"PICTURE X(1)" was assumed.
           05 simple-KO.
           66 simple-KO-R renames simple-KO.
      
      
      
      
       01 Group-A1.
      *Ok
           05 Array-A01-OK pic X.
           66 Array-A01-OK-R renames Array-A01-OK.
      
       01 Group-A2.
      *A"PICTURE"clause was not found for elementary item"Array-A02-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A02-KO.
           66 Array-A02-OK-R renames Array-A02-KO .
      
      
       01 Group-A3.
      
      *A"PICTURE"clause was not found for elementary item"Array-A03-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A03-KO usage is display.
      
      *"usage" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "usage".
      *     66 Array-A03-KO-R renames Array-A03-KO  usage is
      *     display.
      
      
       01 Group-A4.
      *The "PICTURE" clause for item "ARRAY-A04-KO" was not compatible
      *with the specified "USAGE".  "USAGE DISPLAY" was assumed.
      *A "PICTURE" clause was not found for elementary item
      *"ARRAY-A04-KO". "PICTURE X(1)" was assumed.
           05 Array-A04-KO usage is display-1.
      *"usage" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "usage".
      *     66 Array-A04-KO-R renames Array-A04-KO  usage is
      *     display-1.
      
      
       01 Group-A5.
           05 Array-A05-OK usage is index.
      *"usage" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "usage".
      *     66 Array-A05-OK-R renames Array-A05-OK  usage is
      *     index.
      
      
       01 Group-A6.
      *The"PICTURE"clause for item"Array-A06-KO"was not compatible with
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A06-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A06-KO usage is national.
      *"usage" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "usage".
      *     66 Array-A06-KO-R renames Array-A06-KO  usage is
      *     national.
      
      
      *01 Group-A7.
      *TODO Wait for support of UTF-8 (#2504)
      *The"PICTURE"clause for item"Array-A07-KO"was not compatible with
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A07-KO".
      *   "PICTURE X(1)"was assumed.
      *   05 Array-A07-KO usage is utf-8.
      *   66 Array-A07-KO-R renames Array-A07-KO   usage is
      *   utf-8.
      
      
       01 Group-A8.
      *The"PICTURE"clause for item"Array-A08-KO"was not compatible with
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A08-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A08-KO binary.
      *"binary" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "binary".
           66 Array-A08-KO-R renames Array-A08-KO   binary.
      
      
      
       01 Group-A9.
      *The"PICTURE"clause for item"Array-A09-KO"was not compatible with
      *   specified"USAGE". "USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A09-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A09-KO packed-decimal.
      *"packed-decimal" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "packed-decimal".
           66 Array-A09-KO-R renames Array-A09-KO packed-decimal.
      
      
       01 Group-A10.
      *The"PICTURE"clause for item"Array-A10-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A10-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A10-KO comp.
      *"comp" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "comp".
           66 Array-A10-KO-R renames Array-A10-KO  comp.
      
      
       01 Group-A11.
           05 Array-A11-OK comp-1.
      *"comp-1" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "comp-1".
           66 Array-A11-OK-R renames Array-A11-OK  comp-1.
      
      
       01 Group-A12.
           05 Array-A12-OK comp-2.
      *"comp-2" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "comp-2".
           66 Array-A12-OK-R renames Array-A12-OK  comp-2.
      
      
       01 Group-A13.
      *The"PICTURE"clause for item"Array-A13-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A13-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A13-KO comp-3.
      *"comp-3" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "comp-3".
           66 Array-A13-KO-R renames Array-A13-KO  comp-3.
      
      
      
       01 Group-A14.
      *The"PICTURE"clause for item"Array-A14-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A14-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A14-KO comp-4.
      *"comp-4" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "comp-4".
           66 Array-A14-KO-R renames Array-A14-KO  comp-4.
      
      
      
       01 Group-A15.
      *The"PICTURE"clause for item"Array-A15-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-A15-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-A15-KO comp-5.
      
      *"comp-5" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "comp-5".
           66 Array-A15-KO-R renames Array-A15-KO  comp-5.
      
      
       01 Group-A16.
           05 Array-A16-OK pointer.
      *"pointer" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "pointer".
           66 Array-A16-OK-R renames Array-A16-OK  pointer.
      
       01 Group-A17.
           05 Array-A17-OK pointer-32.
      *"pointer-32" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "pointer-32".
           66 Array-A17-OK-R renames Array-A17-OK  pointer-32.
      
       01 Group-A18.
           05 Array-A18-OK procedure-pointer.
      *"procedure-pointer" was invalid.  Scanning was resumed at the
      * next area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "procedure-pointer".
           66 Array-A18-OK-R renames Array-A18-OK
           procedure-pointer.
      
       01 Group-A19.
           05 Array-A19-OK function-pointer.
      *"FUNCTION-POINTER" was invalid.  Scanning was resumed at the next
      * area"A"item, level-number, or the start of the next clause.
      *A period was required.  A period was assumed before
      * "FUNCTION-POINTER".
           66 Array-A19-OK-R renames Array-A19-OK function-pointer.
      
      
      
      *********************************************
      *B. Same thing but usage is declared on parent
      *********************************************
      
       01 Group-B01 usage is display.
      *A"PICTURE"clause was not found for elementary item"Array-B01-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-B01-KO.
           66 Array-B01-OK-R renames Array-B01-KO .
      
      
      
      
       01 Group-B02 usage is display-1.
      *The"PICTURE"clause for item"Array-B02-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B02-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-B02-KO.
           66 Array-B02-OK-R renames Array-B02-KO .
      
      
      
      
       01 Group-B03 usage is index.
           05 Array-B03-OK.
           66 Array-B03-OK-R renames Array-B03-OK .
      
       01 Group-B04 usage is national.
      *The"PICTURE"clause for item"Array-B04-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B04-KO".
      *   "PICTURE X(1)"was assumed.
           05 Array-B04-KO.
           66 Array-B04-OK-R renames Array-B04-KO .
      
      
      
      
      *TODO Wait for support of UTF-8 (#2504)
      *01 Group-B05 usage is utf-8.
      *The"PICTURE"clause for item"Array-B05-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B05-KO".
      *   "PICTURE X(1)"was assumed.
      *    05 Array-B05-KO.
      *    66 Array-B05-KO-R renames Array-B05-KO .
      
      
       01 Group-B06 binary.
      *The"PICTURE"clause for item"Array-B06-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B06-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B06-KO.
            66 Array-B06-OK-R renames Array-B06-KO .
      
      
      
      
       01 Group-B07 packed-decimal.
      *The"PICTURE"clause for item"Array-B07-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B07-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B07-KO.
      
            66 Array-B07-OK-R renames Array-B07-KO .
      
      
      
      
       01 Group-B08 comp.
      *The"PICTURE"clause for item"Array-B08-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B08-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B08-KO.
      
            66 Array-B08-OK-R renames Array-B08-KO .
      
      
      
      
       01 Group-B09 comp-1.
            05 Array-B09-OK.
            66 Array-B09-OK-R renames Array-B09-OK .
      
       01 Group-B10 comp-2.
            05 Array-B10-OK.
            66 Array-B10-OK-R renames Array-B10-OK .
      
      
       01 Group-B11 comp-3.
      *The"PICTURE"clause for item"Array-B11-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B11-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B11-KO.
            66 Array-B11-OK-R renames Array-B11-KO .
      
      
      
       01 Group-B12 comp-4.
      *The"PICTURE"clause for item"Array-B12-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B12-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B12-KO.
            66 Array-B12-OK-R renames Array-B12-KO .
      
      
      
      
       01 Group-B13 comp-5.
      *The"PICTURE"clause for item"Array-B13-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-B13-KO".
      *   "PICTURE X(1)"was assumed.
            05 Array-B13-KO.
            66 Array-B13-OK-R renames Array-B13-KO .
      
      
       01 Group-B14 pointer.
            05 Array-B14-OK.
            66 Array-B14-OK-R renames Array-B14-OK .
      
       01 Group-B15 pointer-32.
            05 Array-B15-OK.
            66 Array-B15-OK-R renames Array-B15-OK .
      
      
       01 Group-B16 procedure-pointer.
            05 Array-B16-OK.
            66 Array-B16-OK-R renames Array-B16-OK .
      
      
       01 Group-B17 function-pointer.
            05 Array-B17-OK.
            66 Array-B17-OK-R renames Array-B17-OK .
      
      
      
      *********************************************
      *C. group-usage is only national or UTF-8
      *********************************************
      
      *The"PICTURE"clause for item"Array-C01-KO"was not compatible with
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Array-C01-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group-C01 group-usage is national.
           05 Array-C01-KO.
           66 Array-C01-OK-R renames Array-C01-KO .
      
      
      *TODO Wait for support of UTF-8 (#2504)
      *01 Group-C02 group-usage is UTF-8.
      *    05 Array-C02-KO.
      *    66 Array-C02-KO-R renames Array-C02-KO .
      
      
      *********************************************
      *D. Usage is not declared directly on parent (test recursivity)
      *********************************************
      
       01 Group-D1 usage is pointer.
           05 Group-D11.
                10 Array-D1-OK.
                66 Array-D1-OK-R renames Array-D1-OK .
      
      
      
       01 Group-D2 comp-5.
           05 Group-D21.
      
      *The"PICTURE"clause for item"ARRAY-D2-KO"was not compatible with
      *the specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"ARRAY-D2-KO".
      *   "PICTURE X(1)"was assumed.
                10 Array-D2-KO.
                66 Array-D2-OK-R renames Array-D2-KO .
      
      
      
      
       01 Group-D3 usage is procedure-pointer.
           05 Group-D31.
                10 Group-D311.
                    15 Array-D3-OK.
                    66 Array-D3-OK-R renames Array-D3-OK .
      
      
       01 Group-D4 usage is display-1.
           05 Group-D41.
                10 Group-D411.
      
      *The"PICTURE"clause for item"ARRAY-D4-KO"was not compatible with
      *the specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"ARRAY-D4-KO".
      *   "PICTURE X(1)"was assumed.
                    15 Array-D4-KO.
                    66 Array-D4-OK-R renames Array-D4-KO .
      
      
      
      
      
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
                    66 Array-D5-OK-R renames Array-D5-KO .
      
      
      
      *********************************************
      *E. Usage is not required because of children
      *********************************************
      
       01 Group-E1.
           05 Array-E1-OK.
      *A "PICTURE" clause was not found for elementary item "VARCHILD".
      *"PICTURE X(1)" was assumed.
                10 VarChild.
      
           66 Array-E1-OK-R renames Array-E1-OK .
      *A "PICTURE" clause was not found for elementary item "VARCHILD-R"
      *"PICTURE X(1)" was assumed.
                10 VarChild-R.
      
       END PROGRAM RenamesAndUsage.