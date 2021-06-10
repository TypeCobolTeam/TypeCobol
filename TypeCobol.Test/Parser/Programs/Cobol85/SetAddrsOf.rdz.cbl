       IDENTIFICATION DIVISION.
       PROGRAM-ID. SetAdrrOf.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 W-myDate TYPE DATE.
       01 W-myDate2 TYPE DATE.
       01 W-PicVar PIC X(10).
       01 W-PointerVar POINTER.

       linkage SECTION.
       01 myDate TYPE DATE.
       01 myDate2 TYPE DATE.
       01 PicVar PIC X(10).
       01 MyBool TYPE BOOL.


       PROCEDURE DIVISION.


      * OF for now but should be KO
       SET LENGTH OF W-mydate2 to ADDRESS OF W-mydate.

      * Create error because it's in working-storage
       SET ADDRESS OF W-mydate2 to ADDRESS OF W-mydate.
      * Create error because it's in working-storage
       SET ADDRESS OF W-myDate TO ADDRESS OF W-PicVar
      * Create error because it's in working-storage
       SET ADDRESS OF W-PicVar TO ADDRESS OF W-mydate.


      * Ok, because the left part of SET use a linkage variable
       SET ADDRESS OF mydate2 to ADDRESS OF W-mydate.
      * Ok, because the left part of SET use a linkage variable
       SET ADDRESS OF myDate TO ADDRESS OF W-PicVar
      * Ok, because the left part of SET use a linkage variable
       SET ADDRESS OF PicVar TO ADDRESS OF W-mydate.


      * Should not create error
       SET ADDRESS OF mydate2 to ADDRESS OF mydate.
      * Should not create error
       SET ADDRESS OF myDate TO ADDRESS OF PicVar
      * No error
       SET ADDRESS OF PicVar TO ADDRESS OF mydate.
      *KO
       SET ADDRESS OF myDate TO W-PointerVar.
      *OK
       SET W-PointerVar TO ADDRESS OF myDate.

           .

       END PROGRAM SetAdrrOf.