      *<< 
          Multiline Comment here
             *>>
       IDENTIFICATION DIVISION.
      *<< 
          Multiline Comment here
             *>>
       PROGRAM-ID. Functions.
*<< 
          Multiline Comment here
             *>>
       DATA DIVISION.
      *<< 
          Multiline Comment here
      *>>
       WORKING-STORAGE SECTION.
      *<< 
          Multiline Comment here
      *>>
       01 myType TYPEDEF STRICT.
      *<< 
          Multiline Comment here
      *>>
        02  var0 PIC 9.
      *<< 
          Multiline Comment here
      *>>
        02  var1 PIC 9(3).
      *<< 
          Multiline Comment here
      *>>
        02  myBool Type BOOL.
      *<< 
          Multiline Comment here
      *>>
        02 subGroup.
      *<< 
          Multiline Comment here
      *>>
         05 var2 Pic XXX.
      *<< 
          Multiline Comment here
      *>>

       PROCEDURE DIVISION.
      *<< 
          Multiline Comment here
      *>>
       
         DECLARE function FUN
      *<< 
          Multiline Comment here
      *>>
               INPUT x PIC 9(05)
      *<< 
          Multiline Comment here
      *>>
                     y PIC 9(03).
      *<< 
          Multiline Comment here
      *>>

           PROCEDURE DIVISION.
      *<< 
          Multiline Comment here
      *>>
             CONTINUE.
      *<< 
          Multiline Comment here
      *>>
         END-DECLARE.
      *<< 
          Multiline Comment here
      *>>

      *<< 
          Multiline Comment here
      *>>
       END PROGRAM Functions.
      *<< 
          Multiline Comment here
      *>>
