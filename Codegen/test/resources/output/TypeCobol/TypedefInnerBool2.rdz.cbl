       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestDefBool.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
      
      *01 ErrorCode typedef strict private.
      *05 V PIC 9(4).
      
       PROCEDURE DIVISION.
      *declare procedure LoopTraceback PRIVATE
      *    input  startingDsa   pointer
      *    output errCode  type ErrorCode
      *    .
      
       END PROGRAM TestDefBool.
      
      *
      *declare procedure LoopTraceback PRIVATE
      *    input  startingDsa   pointer
      *    output errCode  type ErrorCode
      *    .
      *_________________________________________________________________
       IDENTIFICATION DIVISION.
       PROGRAM-ID. df291d1cLoopTraceback.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       data division.
       local-storage section.
      *01 LoopControl typedef strict private.
      *  05 loop          pic S9(5) COMP-5.
      *  05 sta           type bool.
      *  05 t             pic S9(5) COMP-5.
      *  05 savedTrace    pic S9(5) COMP-5.
      *01 controler  type LoopControl.
       01 controler.
           02 loop pic S9(5) COMP-5.
          02  sta-value PIC X VALUE LOW-VALUE.
              88  sta       VALUE 'T'.
              88  sta-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
           02 t pic S9(5) COMP-5.
           02 savedTrace pic S9(5) COMP-5.
                                      
       linkage section.
      
      
       01 startingDsa pointer.
       01 errCode.
           02 V PIC 9(4).
       PROCEDURE DIVISION
             USING BY REFERENCE startingDsa
                   BY REFERENCE errCode
           .
           goback
           .
       END PROGRAM df291d1cLoopTraceback.
