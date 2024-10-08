       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPgm.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
      *             WITH DEBUGGING MODE
                .
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       REPOSITORY.
           FUNCTION ABS INTRINSIC
           FUNCTION MAX INTRINSIC
           FUNCTION PI INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TRIM pic X.
      *KO
      *Expected a data-name following a level-number, but found"PI".
      *   Scanning was resumed at the next area"A"item or the next
      *   level-number.
      *Name PI was specified as a"FUNCTION"name in the"REPOSITORY"
      *   paragraph and cannot be used as a data-name.
       01 PI pic X.
       01 VarX1 pic X(30).
       01 Var2 pic 999.
       01 Var3 pic 9.
       01 Var4 pic 99V999.
       01 VarX2 pic X(30).

       01 Var9 pic N(10) value "Hello".

       01 .
           05 Array1 occurs 9 pic 9.
       01 .
           05 LOG occurs 9 pic 9.
       01 .
           05 Array2 occurs 9.
                10 Array3 occurs 9 pic 9.


       procedure division.

           compute Var4 = MAX( Var2)
           compute Var4 = MAX( (Var2) )
           compute Var4 = MAX( ((Var2)) )


           compute Var4 = FUNCTION MAX( Var2)
           compute Var4 = FUNCTION MAX( (Var2) )
           compute Var4 = FUNCTION MAX( ((Var2)) )


           compute Var4 = MAX(Var2  Var3)
           compute Var4 = MAX(Var2, Var3)
           compute Var4 = MAX(Var2; Var3)
           compute Var4 = MAX((Var2)  Var3)
           compute Var4 = MAX(Var2, (Var3))
           compute Var4 = MAX((Var2); (Var3))



      *Recursive
           compute Var4 = ABS( ABS(Var2) )
           compute Var4 = ABS( ABS( ABS((Var2)) ))

      *Function And Subscripts
           compute Var4 = MAX(FUNCTION LENGTH(Var2(1:2)) 4 Var3 PI)
      *KO in Cobol but OK in TypeCobol so commented here
      *Expected a numeric data item or a numeric literal, but found")".
      *  The"COMPUTE"statement was discarded.
      *    compute Var4 = MAX(3 PI())


      *No Function, only Array
           compute Var4 = Array1( Var2 )
           compute Var4 = Array3(Var2 Var2)
           compute Var4 = Array3(Var2, Var2)
           compute Var4 = Array3(Var2; Var2)
      *Ok it's an array
           compute Var4 = LOG( Var2)
      *Combine function and Array
           compute Var4 = ABS (Array3(Var2, Var2))
      *KO Expected a numeric literal or an index-name in the"COMPUTE"
      *   statement, but found"FUNCTION".  The statement was discarded.
           compute Var4 = ABS (Array3(FUNCTION LENGTH(Var2(1:2)), Var2))
           move VarX1(2:2) to VarX2
      *KO on IBM but OK in our parser. Should we recreate this limitation ?
      *Numeric function"NUMERIC FUNCTION PI"was not allowed in this
      *  context.  The statement was  discarded.
           move PI to Var4
           compute Var4 = PI
           move function TRIM(VarX1 LEADING) to VarX2
           move function TRIM(VarX1, LEADING) to VarX2
           move function TRIM(VarX1; LEADING) to VarX2
           move space to TRIM

           .
       End Program MyPgm.