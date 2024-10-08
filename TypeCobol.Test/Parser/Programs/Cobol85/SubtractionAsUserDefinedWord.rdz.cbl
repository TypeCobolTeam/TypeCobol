﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       data division.
       working-storage section.
        01 a pic 9.
        01 300-200X  pic 9 value 1.
        01 200X-100X pic 9 value 1.
      *KO variable must contains at least one alphabetic char
        01 400-500 pic 9.
      *KO variable must contains at least one alphabetic char
        01 99-99.
      *KO variable must contains at least one alphabetic char
          05 88-88 pic 9.


       procedure division.
      *Should be KO. 400-500 is not a valid variable name because it
      *   contains no alphabetic char
      *   The parser will only report the error on the variable 
      *   declaration.
           compute a = 400-500

      *Should be KO. 99-99 and 88-88 are not valid variable name because it
      *   contains no alphabetic char
      *   The parser will only report the error on the variable 
      *   declaration.
           compute a = 88-88 of 99-99

      *Same tests with unknown variables 
      *Should be KO. 400-500 is not a valid variable name because it
      *   contains no alphabetic char
      *   The parser will only report the error on the variable 
      *   declaration.
           compute a = 66-66

      *Should be KO. 99-99 and 88-88 are not valid variable name because it
      *   contains no alphabetic char
      *   The parser will only report the error on the variable 
      *   declaration.
           compute a = 55-55 of 44-44



      *KO  "300-200X-100X" was not defined as a data-name.
      *    The statement was discarded.
           compute a = 300-200X-100X.
      *KO  "-200" was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
      *    A blank was missing before character"X"in column 32.
      *    A blank was assumed.
           compute a = 300 -200X-100X.
           compute a = 300-200X - 100.
           compute a = 300 - 200X-100X.
      *KO  Expected a numeric data item or a numeric literal, but
      *    found"300-100".  The"COMPUTE"statement was discarded.
           compute a = 300-100
      *KO  "100" was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
      *KO  "300" was not defined as a data-name.
      *    The statement was discarded.
      *KO  "300" was a name that ended with one or more hyphens.
      *    The hyphens were discarded.
           compute a = 300- 100
      *KO  "-100" was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
           compute a = 300 -100
           compute a = 300 - 100
           compute a = -300
           compute a = -300 - 100
      *Ok
           compute a = 30.0E-14
           compute a = 30.0E14
           compute a = +30.0E+14
           compute a = -30.0E-14
           compute a = -30.0E14
           compute a = -30.0E+14
      *KO "123E-4" was not defined as a data-name.
      *   The statement was discarded.
           compute a =  123E-4
      *KO "-1" was invalid.  Skipped to the next verb, period or
      *   procedure-name definition.
      *   A blank was missing before character"-"in column 32.
      *   A blank was assumed.
           compute a =  .25e-24-1
      *KO "-100" was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
      *    A blank was missing before character"-"in column 31.
      *    A blank was assumed.
           compute a = .25e-24-100
      *KO  "-100" was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
      *    A blank was missing before character"-"in column 31.
      *    A blank was assumed.
      *    A blank was missing before character"-"in column 35.
      *    A blank was assumed.
           compute a = .25e-24-100-123


      *Without floating point
      *
      *KO  "300E-14" was not defined as a data-name.
      *    The statement was discarded.
           compute a = 300E-14
      *KO  "300E14" was not defined as a data-name.
      *    The statement was discarded.
           compute a = 300E14
      *OK
           compute a = +300
      *KO  "E"was invalid.  Skipped to the next verb, period or
      *     procedure-name definition.
      *     A blank was missing before character"E"in column 28.
      *     A blank was assumed.
      *TODO #2251 our scanner must scan it as "+300" (integer) and "E"
      *Keep line commented, otherwise it produces an exception
      *    compute a = +300E
      *KO  "E" was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
      *    A blank was missing before character"E"in column 28.
      *    A blank was assumed.
      *    A blank was missing before character"+"in column 29.
      *    A blank was assumed.
      *TODO #2251 our scanner must scan it as "+300" (integer) and "E"
      *Keep line commented, otherwise it produces an exception
      *    compute a = +300E+14
      *KO "E-14" was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
      *    A blank was missing before character"E"in column 28.
      *    A blank was assumed.
      *TODO #2251 our scanner must scan it as "-300" (integer) 
      *     and "E-14"
      *Keep line commented, otherwise it produces an exception
      *    compute a = -300E-14
      *KO  "E14" was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
      *    A blank was missing before character"E"in column 28.
      *    A blank was assumed.
      *TODO #2251 our scanner must scan it as "-300" (integer) 
      *    and "E14"
      *Keep line commented, otherwise it produces an exception
      *    compute a = -300E14
      *KO  "E" was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
      *    A blank was missing before character"E"in column 28.
      *    A blank was assumed.
      *    A blank was missing before character"+"in column 29.
      *    A blank was assumed.
      *TODO #2251 our scanner must scan it as "-300" (integer), 
      *     "E" and "+14"
      *Keep line commented, otherwise it produces an exception
      *    compute a = -300E+14

      *KO  "E"was invalid.  Skipped to the next verb, period or
      *    procedure-name definition.
      *    A blank was missing before character"E"in column 28.
      *    A blank was assumed.
      *TODO #2251 our scanner must scan it as "-300" (integer) 
      *    and "E"
      *Keep line commented, otherwise it produces an exception
      *    compute a = -300E
           .

       END PROGRAM DVZF0OSM.