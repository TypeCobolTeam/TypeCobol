      * 11 CodeElements errors
      * "4"@(24:12>24:65): [30:1] Semantic error: Can't write non typed SmallGroup to strongly typed variable identifier-1:ToughGroup (use UNSAFE keyword for that)
      * "4"@(25:12>25:51): [30:1] Semantic error: Can't write non typed SmallGroup to strongly typed variable identifier-1:ToughGroup (use UNSAFE keyword for that)
      * "4"@(26:12>26:51): [30:1] Semantic error: Can't write non typed Small to strongly typed variable identifier-1:ToughGroup (use UNSAFE keyword for that)
      * "4"@(28:12>28:51): [30:1] Semantic error: Can't write non typed Numeric to strongly typed variable identifier-1:ToughGroup (use UNSAFE keyword for that)
      * "4"@(29:12>29:51): [30:1] Semantic error: Can't write non typed Alphanumeric to strongly typed variable identifier-1:ToughGroup (use UNSAFE keyword for that)
      * "4"@(38:12>38:65): [29:2] Warning: Useless UNSAFE with non strongly typed receiver.
      * "4"@(39:12>39:51): [29:2] Warning: Useless UNSAFE with non strongly typed receiver.
      * "4"@(40:12>40:51): [29:2] Warning: Useless UNSAFE with non strongly typed receiver.
      * "4"@(41:12>41:51): [29:2] Warning: Useless UNSAFE with non strongly typed receiver.
      * "4"@(42:12>42:51): [29:2] Warning: Useless UNSAFE with non strongly typed receiver.
      * "4"@(43:12>43:51): [29:2] Warning: Useless UNSAFE with non strongly typed receiver.
       IDENTIFICATION DIVISION.
         PROGRAM-ID.   Test-UNSAFE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *    01 SmallGroup TYPEDEF.                                             
      *      05 x PIC 9(04).                                                  
      *      05 y PIC 9(04).                                                  
      *    01 ToughGroup TYPEDEF STRONG.                                      
      *      05 x PIC 9(04).                                                  
      *      05 y PIC 9(04).                                                  
      *    01 Small TYPEDEF        PIC 9(04).                                 
      * KO: Elementary TYPEDEF cannot be STRONG.
      *    01 Tough TYPEDEF STRONG PIC 9(04).

      *    01 identifier-1 TYPE ToughGroup.                                   
       01 identifier-1.                                                       
         02 x PIC 9(04).                                                      
         02 y PIC 9(04).                                                      
      *    01 identifier-2 TYPE SmallGroup.                                   
       01 identifier-2.                                                       
         02 x PIC 9(04).                                                      
         02 y PIC 9(04).                                                      
      *    01 identifier-3 TYPE Small.                                        
       01 identifier-3 PIC 9(04).                                                       
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
           MOVE CORRESPONDING identifier-2 TO identifier-1
           MOVE identifier-2 TO identifier-1
           MOVE identifier-3 TO identifier-1
      * OK: receiver is strongly-typed, with UNSAFE sender can have no TYPE
           MOVE identifier-4 TO identifier-1
           MOVE '1337'       TO identifier-1
      * WARN: receiver is weakly-typed, UNSAFE is useless
           MOVE CORRESPONDING identifier-1 TO identifier-2
           MOVE identifier-1 TO identifier-2
           MOVE identifier-4 TO identifier-3
           MOVE '1337'       TO identifier-3
           MOVE identifier-3 TO identifier-4
           MOVE '1337'       TO identifier-4
           .

       END PROGRAM Test-UNSAFE.
