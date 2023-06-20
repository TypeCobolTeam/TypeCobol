       IDENTIFICATION DIVISION.
       PROGRAM-ID. SetToFalse.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
      *                WITH DEBUGGING MODE
                      .
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           .
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 Var1 pic X.
           88 ok1 value 'A' false 'B'.
           88 ok2 value 'A' false 'C'.
           88 ok3 value 'A1' 'A2' false IS 'D'.
           88 ok4 value 'A' when set to false is 'E'.
           88 ok5 value 'A' set false 'F'.
           88 ok6 value 'A' to false 'G'.
           88 ok7 value 'A' when false 'H'.
           88 ok8 value 'A' when to false 'I'.
      * Invalid clause WHEN SET TO FALSE
           88 ko1 value 'A' when to 'U'.
           88 ko2 value 'A' when set to is 'V'.
           88 ko3 value 'A' set 'W'.
           88 ko4 value 'A' to 'X'.
           88 ko5 value 'A' when 'Y'.
           88 ko6 value 'A' when is 'Z'.
           88 ko7 value 'A' when set to false is.
      * Missing clause WHEN SET TO FALSE
           88 ko8 value 'A'.
           
       procedure division.
           set ok1 to true
           set ok2 ok3 ok4 ok5 ok6 ok7 ok8 to true

      ***************************************************************
      * SET TO FALSE OK
      ***************************************************************
           set ok1 to false
           set ok2 ok3 ok4 ok5 ok6 ok7 ok8 to false

      ***************************************************************
      * Expected error: clause WHEN SET TO FALSE is invalid/missing *
      ***************************************************************

      * OK for the 5 following conditions:
           set ko3 to false
           set ko5 to false
           set ko6 to false
           set ko7 to false
           set ko8 to false

      * KO for the 3 following conditions:
           set ko1 to false
           set ko2 to false
           set ko4 to false
      * Clause WHEN SET TO FALSE is not checked correctly
      * (because of ANTLR strange behavior)

           goback.

       END PROGRAM SetToFalse.