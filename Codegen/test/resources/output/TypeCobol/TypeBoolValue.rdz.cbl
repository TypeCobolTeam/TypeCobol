       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestDefBool.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
      *01 A type bool value true.
       01  A-value PIC X VALUE 'T'.
           88  A       VALUE 'T'.
           88  A-false VALUE 'F'
                           X'00' thru 'S'
                           'U' thru X'FF'.
                                 
      *01 B type bool value false.
       01  B-value PIC X VALUE 'F'.
           88  B       VALUE 'T'.
           88  B-false VALUE 'F'
                           X'00' thru 'S'
                           'U' thru X'FF'.
                                        

      *01 Person TYPEDEF STRICT.
      *    05 Registered type Bool value false.
      *01 W-Person1 Type Person.
       01 W-Person1.
          02  Registered-value PIC X VALUE 'F'.
              88  Registered       VALUE 'T'.
              88  Registered-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
                                

      *01 Person2 TYPEDEF STRICT.
      *    05 Registered2 type Bool value true.
      *    05 W-Person2 Type Person.
      *    05 Registered3 type Bool.
      *01 W-Person3 Type Person2.
       01 W-Person3.
          02  Registered2-value PIC X VALUE 'T'.
              88  Registered2       VALUE 'T'.
              88  Registered2-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
           02 W-Person2.
            03  Registered-value PIC X VALUE 'F'.
                88  Registered       VALUE 'T'.
                88  Registered-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
          02  Registered3-value PIC X VALUE LOW-VALUE.
              88  Registered3       VALUE 'T'.
              88  Registered3-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
                                 

      *01 myType TYPEDEF STRICT.
      *     05 myVar PIC 9(10).
      *     05 secondGroup.
      *         10 bval type bool value false.
      *         10 varGroup pic 9(10).
      *         10 typedGroupvar TYPE secondType.
      *         10 ThirdGroup.
      *            15 varGroup pic 9(10).
      *            15 bval2 type bool value true.
      *01 secondType TYPEDEF STRICT.
      *     05 Grp.
      *         10 vargroup PIC 9(10).
      
       01 Vars.
           02 Vars3.
      *    05 bval TYPE bool value true.
           05  bval-value PIC X VALUE 'T'.
           88  bval       VALUE 'T'.
           88  bval-false VALUE 'F'
                           X'00' thru 'S'
                           'U' thru X'FF'.
                                        
      *    05 MyVar1 TYPE myType.
           05 MyVar1.
           06 myVar PIC 9(10).
           06 secondGroup.
            07  bval-value PIC X VALUE 'F'.
                88  bval       VALUE 'T'.
                88  bval-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
             07 varGroup pic 9(10).
             07 typedGroupvar.
               08 Grp.
                 09 vargroup PIC 9(10).
             07 ThirdGroup.
               08 varGroup pic 9(10).
              08  bval2-value PIC X VALUE 'T'.
                  88  bval2       VALUE 'T'.
                  88  bval2-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
                                 
       01 Vars2.
      *    05 MyVar1 TYPE myType.
           05 MyVar1.
           06 myVar PIC 9(10).
           06 secondGroup.
            07  bval-value PIC X VALUE 'F'.
                88  bval       VALUE 'T'.
                88  bval-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
             07 varGroup pic 9(10).
             07 typedGroupvar.
               08 Grp.
                 09 vargroup PIC 9(10).
             07 ThirdGroup.
               08 varGroup pic 9(10).
              08  bval2-value PIC X VALUE 'T'.
                  88  bval2       VALUE 'T'.
                  88  bval2-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
                                 
      *01 MyVar2 TYPE myType.
       01 MyVar2.
           02 myVar PIC 9(10).
           02 secondGroup.
            03  bval-value PIC X VALUE 'F'.
                88  bval       VALUE 'T'.
                88  bval-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
             03 varGroup pic 9(10).
             03 typedGroupvar.
               04 Grp.
                 05 vargroup PIC 9(10).
             03 ThirdGroup.
               04 varGroup pic 9(10).
              04  bval2-value PIC X VALUE 'T'.
                  88  bval2       VALUE 'T'.
                  88  bval2-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
                             

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
       PROGRAM-ID. ae4b6d10.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       data division.
       local-storage section.
      *TestDefBool.LoopTraceback - Params :
      *     input(startingDsa: pointer)
      *     output(errCode: ErrorCode)
                             
      *01 LoopControl typedef strict.
      *  05 loop          pic S9(5) COMP-5.
      *  05 sta           type bool value true.
      *  05 t             pic S9(5) COMP-5.
      *  05 savedTrace    pic S9(5) COMP-5.
      *  05 sta2           type bool value false.
      *  05 sta3           type bool.
      *01 controler  type LoopControl.
       01 controler.
           02 loop pic S9(5) COMP-5.
          02  sta-value PIC X VALUE 'T'.
              88  sta       VALUE 'T'.
              88  sta-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
           02 t pic S9(5) COMP-5.
           02 savedTrace pic S9(5) COMP-5.
          02  sta2-value PIC X VALUE 'F'.
              88  sta2       VALUE 'T'.
              88  sta2-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
          02  sta3-value PIC X VALUE LOW-VALUE.
              88  sta3       VALUE 'T'.
              88  sta3-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
                                      
       linkage section.
      *TestDefBool.LoopTraceback - Params :
      *     input(startingDsa: pointer)
      *     output(errCode: ErrorCode)
                       
            
       01 startingDsa pointer.
       01 errCode.
           02 V PIC 9(4).
       PROCEDURE DIVISION
             USING BY REFERENCE startingDsa
                   BY REFERENCE errCode
           .
      *TestDefBool.LoopTraceback - Params :
      *     input(startingDsa: pointer)
      *     output(errCode: ErrorCode)
           goback
           .
       END PROGRAM ae4b6d10.


