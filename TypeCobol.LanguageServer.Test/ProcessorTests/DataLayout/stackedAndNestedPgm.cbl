       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPGM.

       data division.
       working-storage section.
       01 main-group.
          05 main-var PIC X.

       PROCEDURE DIVISION.
           goback.

       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
      * Nested program in Main
      *-----------------------------------------------------------------
       PROGRAM-ID. Nested0.

       data division.
       working-storage section.
       01 Nested0-group.
          05 Nested0-var PIC X.

       END PROGRAM Nested0.
       END PROGRAM MAINPGM.

      *-----------------------------------------------------------------
      * Stacked program 1
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacked1.

       data division.
       working-storage section.
       01 Stacked1-group.
          05 Stacked1-var PIC X.

       PROCEDURE DIVISION.
           goback.
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
      * Nested program in Stacked 1
      *-----------------------------------------------------------------
       PROGRAM-ID. Nested1.

       END PROGRAM Nested1.
       END PROGRAM Stacked1.

      *-----------------------------------------------------------------
      * Stacked program 2
      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Stacked2.

       data division.
       working-storage section.
       01 Stacked2-group.
          05 Stacked2-var PIC X.

       END PROGRAM Stacked2.