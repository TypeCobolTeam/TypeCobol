       IDENTIFICATION DIVISION.
       PROGRAM-ID. Level77AndUsage.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       77 Var1-OK PIC X.
      
      *A"PICTURE"clause was not found for elementary item"Var2-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var2-KO.
      
      *A"PICTURE"clause was not found for elementary item"Var3-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var3-KO USAGE IS display.
      
      *The"PICTURE"clause for item"Var4-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Var4-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var4-KO USAGE IS display-1.
      
       77 Var5-OK USAGE IS INDEX.
      
      *The"PICTURE"clause for item"Var6-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Var6-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var6-KO USAGE IS national.
      
      *TODO Wait for support of UTF-8 (#2504)
      *The"PICTURE"clause for item"Var7-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Var7-KO".
      *   "PICTURE X(1)"was assumed.
      *77 Var7-KO USAGE IS utf-8.
      
      *The"PICTURE"clause for item"Var8-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Var8-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var8-KO USAGE IS binary.
      
      *The"PICTURE"clause for item"Var9-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Var9-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var9-KO USAGE IS packed-decimal.
      
      *The"PICTURE"clause for item"Var10-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Var10-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var10-KO USAGE IS comp.
      
       77 Var11-OK USAGE IS comp-1.
       77 Var12-OK USAGE IS comp-2.
      
      *The"PICTURE"clause for item"Var13-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Var13-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var13-KO USAGE IS comp-3.
      
      *The"PICTURE"clause for item"Var14-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Var14-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var14-KO USAGE IS comp-4.
      
      *The"PICTURE"clause for item"Var15-KO"was not compatible with the
      *   specified"USAGE"."USAGE DISPLAY"was assumed.
      *A"PICTURE"clause was not found for elementary item"Var15-KO".
      *   "PICTURE X(1)"was assumed.
       77 Var15-KO USAGE IS comp-5.
      
       77 Var16-OK POINTER.
       77 Var17-OK USAGE POINTER-32.
       77 Var18-OK USAGE IS procedure-pointer.
       77 Var19-OK USAGE IS function-pointer.
      
       END PROGRAM Level77AndUsage.