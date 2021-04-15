       IDENTIFICATION DIVISION.
       PROGRAM-ID. TypoParenthesis.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       special-names. decimal-point is comma.
       DATA DIVISION.
       working-storage section.

      * OK
       01 pic X(5).
       01 pic X5).
       01 pic X)5).
       01 pic X(5.
      * OK
       01 pic X5.
       01 pic X)5.
       01 pic X(5(.
       01 pic X5(.
       01 pic X)5(.

      * OK
       01 pic S9(5)V9(5).
       01 pic S95)V9(5).
       01 pic S9)5)V9(5).
       01 pic S9(5V9(5).
      * OK
       01 pic S95V9(5).
       01 pic S9)5V9(5).
       01 pic S9(5(V9(5).
       01 pic S95(V9(5).
       01 pic S9)5(V9(5).
       01 pic S9(5)V95).
       01 pic S95)V95).
       01 pic S9)5)V95).
       01 pic S9(5V95).
       01 pic S95V95).
       01 pic S9)5V95).
       01 pic S9(5(V95).
       01 pic S95(V95).
       01 pic S9)5(V95).
       01 pic S9(5)V9)5).
       01 pic S95)V9)5).
       01 pic S9)5)V9)5).
       01 pic S9(5V9)5).
       01 pic S95V9)5).
       01 pic S9)5V9)5).
       01 pic S9(5(V9)5).
       01 pic S95(V9)5).
       01 pic S9)5(V9)5).
       01 pic S9(5)V9(5.
       01 pic S95)V9(5.
       01 pic S9)5)V9(5.
       01 pic S9(5V9(5.
       01 pic S95V9(5.
       01 pic S9)5V9(5.
       01 pic S9(5(V9(5.
       01 pic S95(V9(5.
       01 pic S9)5(V9(5.
      * OK
       01 pic S9(5)V95.
       01 pic S95)V95.
       01 pic S9)5)V95.
       01 pic S9(5V95.
      * OK
       01 pic S95V95.
       01 pic S9)5V95.
       01 pic S9(5(V95.
       01 pic S95(V95.
       01 pic S9)5(V95.
       01 pic S9(5)V9)5.
       01 pic S95)V9)5.
       01 pic S9)5)V9)5.
       01 pic S9(5V9)5.
       01 pic S95V9)5.
       01 pic S9)5V9)5.
       01 pic S9(5(V9)5.
       01 pic S95(V9)5.
       01 pic S9)5(V9)5.
       01 pic S9(5)V9(5(.
       01 pic S95)V9(5(.
       01 pic S9)5)V9(5(.
       01 pic S9(5V9(5(.
       01 pic S95V9(5(.
       01 pic S9)5V9(5(.
       01 pic S9(5(V9(5(.
       01 pic S95(V9(5(.
       01 pic S9)5(V9(5(.
       01 pic S9(5)V95(.
       01 pic S95)V95(.
       01 pic S9)5)V95(.
       01 pic S9(5V95(.
       01 pic S95V95(.
       01 pic S9)5V95(.
       01 pic S9(5(V95(.
       01 pic S95(V95(.
       01 pic S9)5(V95(.
       01 pic S9(5)V9)5(.
       01 pic S95)V9)5(.
       01 pic S9)5)V9)5(.
       01 pic S9(5V9)5(.
       01 pic S95V9)5(.
       01 pic S9)5V9)5(.
       01 pic S9(5(V9)5(.
       01 pic S95(V9)5(.
       01 pic S9)5(V9)5(.

       PROCEDURE DIVISION.
       END PROGRAM TypoParenthesis.