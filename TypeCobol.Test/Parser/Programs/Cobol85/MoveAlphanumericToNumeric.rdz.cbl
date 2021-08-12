       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 alpha        pic X(03).
       01 alpha2       pic X(03).

       01 num1         pic 9(03).
       01 num2         comp-1.
       01 num3         comp-2.
       01 num4         pic 9(03) comp-3.
       01 num5         pic 9(03) comp-4.
       01 num6         pic 9(03) comp-5.
       01 num7         pic 9(03) packed-decimal.
       01 num8         pic 9(03) binary.
       01 num9         pic 9(03) comp.

       PROCEDURE DIVISION.

      *   Ok
           move alpha          to alpha2
      *   Ok because num1 is an extended numeric
           move alpha          to num1
      *   Ok as it is not a numeric
           move alpha          to num2
      *   Ok as it is not a numeric
           move alpha          to num3

      *   Ko because it is a comp variable
           move alpha          to num4
      *   Ko because it is a comp variable
           move alpha          to num5
      *   Ko because it is a comp variable
           move alpha          to num6
      *   Ko because it is a comp variable
           move alpha          to num7
      *   Ko because it is a comp variable
           move alpha          to num8
      *   Ko because it is a comp variable
           move alpha          to num9

      *   Ok because it is a function
           move FUNCTION WHEN-COMPILED          to num4
      *   Ok because it is a function
           move FUNCTION CURRENT-DATE           to num4
      *   Ok because it is a function
           move LENGTH                   alpha  to num4
      *   Ok because it is a function
           move LENGTH OF                alpha  to num4

           GOBACK
           .

       END PROGRAM Pgm.