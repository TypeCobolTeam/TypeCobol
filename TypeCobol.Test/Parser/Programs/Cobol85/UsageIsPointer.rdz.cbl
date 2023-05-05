       IDENTIFICATION DIVISION.
       PROGRAM-ID. Pgm2503.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Var1 pointer.
       01 Var2 usage is pointer.

       01 Var3 pointer-32.
       01 Var4 usage is pointer-32.

      *Ok on a group
       01 Group1 pointer-32.
      *    Ok because group has a valid usage that apply to its children
           05 Var5.
      *    Ok same usage as parent
           05 Var6 pointer-32.

      *    Ko usage different than parent
      *    TODO currently not check by our parser
      *The specified"USAGE"was different from the "USAGE"specified at
      *the group level.  The group "USAGE" was assumed for this item.
           05 Var7 pointer.

       01 Var10 pic X.

       LINKAGE SECTION.
       01 Link1 PIC 9(8).

       procedure division.
           set Var1 to address of Var10
           set Var2 to address of Var10
           set Var3 to address of Var10
           set Var4 to address of Var10

           set address of Link1 to Var1
           set address of Link1 to Var2
           set address of Link1 to Var3
           set address of Link1 to Var4

           goback
           .
       END PROGRAM Pgm2503.