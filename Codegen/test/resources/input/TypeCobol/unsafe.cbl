       IDENTIFICATION DIVISION.
         PROGRAM-ID.   Test-UNSAFE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 SmallGroup TYPEDEF.
             05 x PIC 9(04).
             05 y PIC 9(04).
           01 ToughGroup TYPEDEF STRONG.
             05 x PIC 9(04).
             05 y PIC 9(04).
           01 Small TYPEDEF        PIC 9(04).
      * KO: Elementary TYPEDEF cannot be STRONG.
      *    01 Tough TYPEDEF STRONG PIC 9(04).

           01 identifier-1 TYPE ToughGroup.
           01 identifier-2 TYPE SmallGroup.
           01 identifier-3 TYPE Small.
           01 identifier-4 PIC 9(04).

       PROCEDURE DIVISION.
       
      * KO: receiver is strongly-typed, sender cannot be of a different TYPE
           MOVE        CORRESPONDING identifier-2 TO identifier-1
           MOVE        identifier-2 TO identifier-1
           MOVE        identifier-3 TO identifier-1
      * KO: receiver is strongly-typed, sender cannot have no TYPE
           MOVE        identifier-4 TO identifier-1
           MOVE        '1337'       TO identifier-1
      * OK: receiver is strongly-typed, with UNSAFE sender can be of a different TYPE
           MOVE UNSAFE CORRESPONDING identifier-2 TO identifier-1
           MOVE UNSAFE identifier-2 TO identifier-1
           MOVE UNSAFE identifier-3 TO identifier-1
      * OK: receiver is strongly-typed, with UNSAFE sender can have no TYPE
           MOVE UNSAFE identifier-4 TO identifier-1
           MOVE UNSAFE '1337'       TO identifier-1
      * WARN: receiver is weakly-typed, UNSAFE is useless
           MOVE UNSAFE CORRESPONDING identifier-1 TO identifier-2
           MOVE UNSAFE identifier-1 TO identifier-2
           MOVE UNSAFE identifier-4 TO identifier-3
           MOVE UNSAFE '1337'       TO identifier-3
           MOVE UNSAFE identifier-3 TO identifier-4
           MOVE UNSAFE '1337'       TO identifier-4
           .

       END PROGRAM Test-UNSAFE.