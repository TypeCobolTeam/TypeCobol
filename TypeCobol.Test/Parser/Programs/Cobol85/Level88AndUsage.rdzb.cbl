       IDENTIFICATION DIVISION.
       PROGRAM-ID. Level88AndUsage.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01 Group1-OK PIC 9.
           88 var88 VALUE 1.
      
      *A"PICTURE"clause was not found for elementary item"Group2-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group2-KO.
           88 var88 VALUE "1".
      
      *A"PICTURE"clause was not found for elementary item"Group3-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group3-KO USAGE IS display.
           88 var88 VALUE "1".
      
      *The"PICTURE"clause for item"Group4-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Group4-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group4-KO USAGE IS display-1.
           88 var88 VALUE "1".
      
      *Not supported
      *01 Group5-OK USAGE IS INDEX.
      *    88 var88 VALUE ?.
      
      *Not supported
      *01 Group6-KO USAGE IS national.
      *    88 var88 VALUE ?.
      
      *TODO Wait for support of UTF-8 (#2504)
      *The"PICTURE"clause for item"Group7-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Group7-KO".
      *   "PICTURE X(1)"was assumed.
      *01 Group7-KO USAGE IS utf-8.
      *     88 var88 VALUE 1.
      
      *The"PICTURE"clause for item"Group8-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Group8-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group8-KO USAGE IS binary.
           88 var88 VALUE "1".
      
      *The"PICTURE"clause for item"Group9-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Group9-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group9-KO USAGE IS packed-decimal.
           88 var88 VALUE "1".
      
      *The"PICTURE"clause for item"Group10-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Group10-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group10-KO USAGE IS comp.
           88 var88 VALUE "1".
      
       01 Group11-OK USAGE IS comp-1.
           88 var88 VALUE 0.
       01 Group12-OK USAGE IS comp-2.
           88 var88 VALUE 0.
      
      *The"PICTURE"clause for item"Group13-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Group13-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group13-KO USAGE IS comp-3.
           88 var88 VALUE "1".
      
      *The"PICTURE"clause for item"Group14-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Group14-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group14-KO USAGE IS comp-4.
           88 var88 VALUE "1".
      
      *The"PICTURE"clause for item"Group15-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Group15-KO".
      *   "PICTURE X(1)"was assumed.
       01 Group15-KO USAGE IS comp-5.
           88 var88 VALUE "1".
      
      *Not supported
      *01 Group16-OK POINTER.
      *     88 var88 VALUE ?.
      
      *Not supported
      *01 Group17-OK USAGE POINTER-32.
      *     88 var88 VALUE ?.
      
      *Not supported
      *01 Group18-OK USAGE IS procedure-pointer.
      *    88 var88 VALUE ?.
      
      *Not supported
      *01 Group19-OK USAGE IS function-pointer.
      *    88 var88 VALUE ?.
            
      
       END PROGRAM Level88AndUsage.