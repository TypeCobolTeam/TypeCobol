       IDENTIFICATION DIVISION.
      *Warning : A blank was missing before character "P" in column 19.
      *          A blank was assumed.
       PROGRAM-ID.Pgm.
      * OK: No warning
       AUTHOR.AUTHOR.
      * OK: No warning
       DATE-WRITTEN.NOVEMBRE 80.
      * OK: No warning
       DATE-COMPILED..
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *Warning: A blank was missing before character "I" in column 24.
      *         A blank was assumed.
       SOURCE-COMPUTER.IBM-370.
      *Warning: A blank was missing before character "I" in column 24.
      *         A blank was assumed.
       OBJECT-COMPUTER.IBMCOBOLII.
      *Error: "."was invalid. Scanning was resumed at the next area
      *       "A" item, level-number or the start of the next clause.
      *Warning: A blank was missing before character "." in column 22.
      *         A blank was assumed.
       data division..
      *Error: "."was invalid. Scanning was resumed at the next area
      *       "A" item, level-number or the start of the next clause.
      *Warning: A blank was missing before character "." in column 32.
      *         A blank was assumed.
       working-storage section..
       01 A pic X.
       01 B pic X.
      
      *Warning: A blank was missing before character "." in column 27.
      *         A blank was assumed.
       PROCEDURE DIVISION..
      
      *Warning: A blank was missing before character "." in column 24.
      *         A blank was assumed.
           move A to B..
      
      *Warning: A blank was missing before character "." in column 24.
      *         A blank was assumed.
      *Warning: A blank was missing before character "." in column 25.
      *         A blank was assumed.
      *Warning: A blank was missing before character "m" in column 26.
      *         A blank was assumed.
           move A to B...move A to B.
      
           .
      *Error: Expected end of source file or the beginning of a
      *       separately compiled program, but found".".  Skipped to end
      *       of source file or the beginning of the next program.
      *Warning: A blank was missing before character "." in column 24.
      *         A blank was assumed.
       END PROGRAM Pgm..