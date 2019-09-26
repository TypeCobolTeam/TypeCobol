       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestDefBool.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
       01 A type bool value true.
       01 B type bool value false.      

       01 Person TYPEDEF STRICT.
           05 Registered type Bool value false.
       01 W-Person1 Type Person.

       01 Person2 TYPEDEF STRICT.
           05 Registered2 type Bool value true.       
           05 W-Person2 Type Person.
           05 Registered3 type Bool.       
       01 W-Person3 Type Person2.

       01 myType TYPEDEF STRICT.
            05 myVar PIC 9(10).
            05 secondGroup.
                10 bval type bool value false.
                10 varGroup pic 9(10).
                10 typedGroupvar TYPE secondType.
                10 ThirdGroup.
                   15 varGroup pic 9(10).
                   15 bval2 type bool value true.      
       01 secondType TYPEDEF STRICT.
            05 Grp.
                10 vargroup PIC 9(10).
      
       01 Vars.
           02 Vars3.
           05 bval TYPE bool value true.
           05 MyVar1 TYPE myType.
       01 Vars2.
           05 MyVar1 TYPE myType.
       01 MyVar2 TYPE myType.

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
         05 sta           type bool value true.
         05 t             pic S9(5) COMP-5.
         05 savedTrace    pic S9(5) COMP-5.
         05 sta2           type bool value false.
         05 sta3           type bool.
       01 controler  type LoopControl.
       linkage section.
            
       procedure division.
           goback
           .
       end-declare.

       END PROGRAM TestDefBool.
      