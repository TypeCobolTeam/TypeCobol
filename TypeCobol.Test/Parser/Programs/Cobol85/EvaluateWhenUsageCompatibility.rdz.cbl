       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVALWHEN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GRP-BINARY                       USAGE BINARY.
          05 VAR-BINARY         PIC S9(2).
       77 VAR-COMP              PIC S9(2)  USAGE COMP.
       77 VAR-COMP1                        USAGE COMP-1.
       01 GRP-COMP2                        USAGE COMP-2.
          05 VAR-COMP2.
       77 VAR-COMP3             PIC S9(2)  USAGE COMP-3.
       01 GRP-COMP4                        USAGE COMP-4.
          05 VAR-COMP4          PIC S9(2).
       77 VAR-COMP5             PIC S9(2)  USAGE COMP-5.
       77 VAR-PACKED-DECIMAL    PIC S9(2)  USAGE PACKED-DECIMAL.
       77 VAR-DISPLAY1          PIC G      USAGE DISPLAY-1.
       01 GRP-DISPLAY                      USAGE DISPLAY.
          05 VAR-DISPLAY        PIC X.
       77 VAR-NATIONAL          PIC N      USAGE NATIONAL.
       01 GRP-INDEX                        USAGE INDEX.
          05 VAR-INDEX.
       77 VAR-OBJECT-REFERENCE             USAGE OBJECT REFERENCE.
       01 GRP-POINTER                      USAGE POINTER.
          05 VAR-POINTER.
       77 VAR-POINTER32                    USAGE POINTER-32.
       01 GRP-FUNCTION-POINTER             USAGE FUNCTION-POINTER.
          05 VAR-FUNCTION-POINTER.
       77 VAR-PROCEDURE-POINTER            USAGE PROCEDURE-POINTER.
       77 VAR-UTF8                         USAGE UTF-8.

       PROCEDURE DIVISION.

           EVALUATE VAR-BINARY
               WHEN VAR-BINARY
                    DISPLAY "OK"
               WHEN VAR-COMP
                    DISPLAY "OK"
               WHEN VAR-COMP1
                    DISPLAY "OK"
               WHEN VAR-COMP2
                    DISPLAY "OK"
               WHEN VAR-COMP3
                    DISPLAY "OK"
               WHEN VAR-COMP4
                    DISPLAY "OK"
               WHEN VAR-COMP5
                    DISPLAY "OK"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "OK"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-COMP
               WHEN VAR-BINARY
                    DISPLAY "OK"
               WHEN VAR-COMP
                    DISPLAY "OK"
               WHEN VAR-COMP1
                    DISPLAY "OK"
               WHEN VAR-COMP2
                    DISPLAY "OK"
               WHEN VAR-COMP3
                    DISPLAY "OK"
               WHEN VAR-COMP4
                    DISPLAY "OK"
               WHEN VAR-COMP5
                    DISPLAY "OK"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "OK"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-COMP1
               WHEN VAR-BINARY
                    DISPLAY "OK"
               WHEN VAR-COMP
                    DISPLAY "OK"
               WHEN VAR-COMP1
                    DISPLAY "OK"
               WHEN VAR-COMP2
                    DISPLAY "OK"
               WHEN VAR-COMP3
                    DISPLAY "OK"
               WHEN VAR-COMP4
                    DISPLAY "OK"
               WHEN VAR-COMP5
                    DISPLAY "OK"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "OK"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-COMP2
               WHEN VAR-BINARY
                    DISPLAY "OK"
               WHEN VAR-COMP
                    DISPLAY "OK"
               WHEN VAR-COMP1
                    DISPLAY "OK"
               WHEN VAR-COMP2
                    DISPLAY "OK"
               WHEN VAR-COMP3
                    DISPLAY "OK"
               WHEN VAR-COMP4
                    DISPLAY "OK"
               WHEN VAR-COMP5
                    DISPLAY "OK"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "OK"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-COMP3
               WHEN VAR-BINARY
                    DISPLAY "OK"
               WHEN VAR-COMP
                    DISPLAY "OK"
               WHEN VAR-COMP1
                    DISPLAY "OK"
               WHEN VAR-COMP2
                    DISPLAY "OK"
               WHEN VAR-COMP3
                    DISPLAY "OK"
               WHEN VAR-COMP4
                    DISPLAY "OK"
               WHEN VAR-COMP5
                    DISPLAY "OK"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "OK"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-COMP4
               WHEN VAR-BINARY
                    DISPLAY "OK"
               WHEN VAR-COMP
                    DISPLAY "OK"
               WHEN VAR-COMP1
                    DISPLAY "OK"
               WHEN VAR-COMP2
                    DISPLAY "OK"
               WHEN VAR-COMP3
                    DISPLAY "OK"
               WHEN VAR-COMP4
                    DISPLAY "OK"
               WHEN VAR-COMP5
                    DISPLAY "OK"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "OK"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-COMP5
               WHEN VAR-BINARY
                    DISPLAY "OK"
               WHEN VAR-COMP
                    DISPLAY "OK"
               WHEN VAR-COMP1
                    DISPLAY "OK"
               WHEN VAR-COMP2
                    DISPLAY "OK"
               WHEN VAR-COMP3
                    DISPLAY "OK"
               WHEN VAR-COMP4
                    DISPLAY "OK"
               WHEN VAR-COMP5
                    DISPLAY "OK"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "OK"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-PACKED-DECIMAL
               WHEN VAR-BINARY
                    DISPLAY "OK"
               WHEN VAR-COMP
                    DISPLAY "OK"
               WHEN VAR-COMP1
                    DISPLAY "OK"
               WHEN VAR-COMP2
                    DISPLAY "OK"
               WHEN VAR-COMP3
                    DISPLAY "OK"
               WHEN VAR-COMP4
                    DISPLAY "OK"
               WHEN VAR-COMP5
                    DISPLAY "OK"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "OK"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-DISPLAY
               WHEN VAR-DISPLAY
                    DISPLAY "OK"
               WHEN VAR-NATIONAL
                    DISPLAY "OK"
               WHEN VAR-BINARY
                    DISPLAY "KO"
               WHEN VAR-COMP
                    DISPLAY "KO"
               WHEN VAR-COMP1
                    DISPLAY "KO"
               WHEN VAR-COMP2
                    DISPLAY "KO"
               WHEN VAR-COMP3
                    DISPLAY "KO"
               WHEN VAR-COMP4
                    DISPLAY "KO"
               WHEN VAR-COMP5
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-DISPLAY1
               WHEN VAR-DISPLAY1
                    DISPLAY "OK"
               WHEN VAR-NATIONAL
                    DISPLAY "OK"
               WHEN VAR-BINARY
                    DISPLAY "KO"
               WHEN VAR-COMP
                    DISPLAY "KO"
               WHEN VAR-COMP1
                    DISPLAY "KO"
               WHEN VAR-COMP2
                    DISPLAY "KO"
               WHEN VAR-COMP3
                    DISPLAY "KO"
               WHEN VAR-COMP4
                    DISPLAY "KO"
               WHEN VAR-COMP5
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-NATIONAL
               WHEN VAR-NATIONAL
                    DISPLAY "OK"
               WHEN VAR-DISPLAY
                    DISPLAY "OK"
               WHEN VAR-DISPLAY1
                    DISPLAY "OK"
               WHEN VAR-BINARY
                    DISPLAY "KO"
               WHEN VAR-COMP
                    DISPLAY "KO"
               WHEN VAR-COMP1
                    DISPLAY "KO"
               WHEN VAR-COMP2
                    DISPLAY "KO"
               WHEN VAR-COMP3
                    DISPLAY "KO"
               WHEN VAR-COMP4
                    DISPLAY "KO"
               WHEN VAR-COMP5
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-INDEX
               WHEN VAR-INDEX
                    DISPLAY "OK"
               WHEN VAR-BINARY
                    DISPLAY "KO"
               WHEN VAR-COMP
                    DISPLAY "KO"
               WHEN VAR-COMP1
                    DISPLAY "KO"
               WHEN VAR-COMP2
                    DISPLAY "KO"
               WHEN VAR-COMP3
                    DISPLAY "KO"
               WHEN VAR-COMP4
                    DISPLAY "KO"
               WHEN VAR-COMP5
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-OBJECT-REFERENCE
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "OK"
               WHEN VAR-BINARY
                    DISPLAY "KO"
               WHEN VAR-COMP
                    DISPLAY "KO"
               WHEN VAR-COMP1
                    DISPLAY "KO"
               WHEN VAR-COMP2
                    DISPLAY "KO"
               WHEN VAR-COMP3
                    DISPLAY "KO"
               WHEN VAR-COMP4
                    DISPLAY "KO"
               WHEN VAR-COMP5
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-POINTER
               WHEN VAR-POINTER
                    DISPLAY "OK"
               WHEN VAR-POINTER32
                    DISPLAY "OK"
               WHEN VAR-BINARY
                    DISPLAY "KO"
               WHEN VAR-COMP
                    DISPLAY "KO"
               WHEN VAR-COMP1
                    DISPLAY "KO"
               WHEN VAR-COMP2
                    DISPLAY "KO"
               WHEN VAR-COMP3
                    DISPLAY "KO"
               WHEN VAR-COMP4
                    DISPLAY "KO"
               WHEN VAR-COMP5
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-POINTER32
               WHEN VAR-POINTER
                    DISPLAY "OK"
               WHEN VAR-POINTER32
                    DISPLAY "OK"
               WHEN VAR-BINARY
                    DISPLAY "KO"
               WHEN VAR-COMP
                    DISPLAY "KO"
               WHEN VAR-COMP1
                    DISPLAY "KO"
               WHEN VAR-COMP2
                    DISPLAY "KO"
               WHEN VAR-COMP3
                    DISPLAY "KO"
               WHEN VAR-COMP4
                    DISPLAY "KO"
               WHEN VAR-COMP5
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "KO"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-FUNCTION-POINTER
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "OK"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "OK"
               WHEN VAR-BINARY
                    DISPLAY "KO"
               WHEN VAR-COMP
                    DISPLAY "KO"
               WHEN VAR-COMP1
                    DISPLAY "KO"
               WHEN VAR-COMP2
                    DISPLAY "KO"
               WHEN VAR-COMP3
                    DISPLAY "KO"
               WHEN VAR-COMP4
                    DISPLAY "KO"
               WHEN VAR-COMP5
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-PROCEDURE-POINTER
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "OK"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "OK"
               WHEN VAR-BINARY
                    DISPLAY "KO"
               WHEN VAR-COMP
                    DISPLAY "KO"
               WHEN VAR-COMP1
                    DISPLAY "KO"
               WHEN VAR-COMP2
                    DISPLAY "KO"
               WHEN VAR-COMP3
                    DISPLAY "KO"
               WHEN VAR-COMP4
                    DISPLAY "KO"
               WHEN VAR-COMP5
                    DISPLAY "KO"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "KO"
               WHEN VAR-DISPLAY
                    DISPLAY "KO"
               WHEN VAR-DISPLAY1
                    DISPLAY "KO"
               WHEN VAR-INDEX
                    DISPLAY "KO"
               WHEN VAR-NATIONAL
                    DISPLAY "KO"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "KO"
               WHEN VAR-POINTER
                    DISPLAY "KO"
               WHEN VAR-POINTER32
                    DISPLAY "KO"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           EVALUATE VAR-UTF8
               WHEN VAR-BINARY
                    DISPLAY "NOT MANAGED"
               WHEN VAR-COMP
                    DISPLAY "NOT MANAGED"
               WHEN VAR-COMP1
                    DISPLAY "NOT MANAGED"
               WHEN VAR-COMP2
                    DISPLAY "NOT MANAGED"
               WHEN VAR-COMP3
                    DISPLAY "NOT MANAGED"
               WHEN VAR-COMP4
                    DISPLAY "NOT MANAGED"
               WHEN VAR-COMP5
                    DISPLAY "NOT MANAGED"
               WHEN VAR-PACKED-DECIMAL
                    DISPLAY "NOT MANAGED"
               WHEN VAR-DISPLAY
                    DISPLAY "NOT MANAGED"
               WHEN VAR-DISPLAY1
                    DISPLAY "NOT MANAGED"
               WHEN VAR-INDEX
                    DISPLAY "NOT MANAGED"
               WHEN VAR-NATIONAL
                    DISPLAY "NOT MANAGED"
               WHEN VAR-OBJECT-REFERENCE
                    DISPLAY "NOT MANAGED"
               WHEN VAR-POINTER
                    DISPLAY "NOT MANAGED"
               WHEN VAR-POINTER32
                    DISPLAY "NOT MANAGED"
               WHEN VAR-FUNCTION-POINTER
                    DISPLAY "NOT MANAGED"
               WHEN VAR-PROCEDURE-POINTER
                    DISPLAY "NOT MANAGED"
               WHEN VAR-UTF8
                    DISPLAY "NOT MANAGED"
               WHEN OTHER
                    DISPLAY "OTHER"
           END-EVALUATE

           GOBACK
           .
		   
       END PROGRAM EVALWHEN.