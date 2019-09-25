       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestDefBool.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
      
       01 ErrorCode typedef strict private.
       05 V PIC 9(4).
      
       PROCEDURE DIVISION.
       declare procedure LoopTraceback PRIVATE
           input  startingDsa   pointer
           output errCode  type ErrorCode
           .
       data division.
       local-storage section.
       01 LoopControl typedef strict.
         05 loop          pic S9(5) COMP-5.
         05 sta           type bool.
         05 t             pic S9(5) COMP-5.
         05 savedTrace    pic S9(5) COMP-5.
       01 controler  type LoopControl.
       linkage section.
      
      
       procedure division.
           goback
           .
       end-declare.
      
       END PROGRAM TestDefBool.
      