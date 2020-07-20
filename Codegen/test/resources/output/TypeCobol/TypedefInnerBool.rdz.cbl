       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestDefBool.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
      *01  MyBool TYPE BOOL.
       01  MyBool-value PIC X VALUE LOW-VALUE.
           88  MyBool       VALUE 'T'.
           88  MyBool-false VALUE 'F'
                           X'00' thru 'S'
                           'U' thru X'FF'.
                            
      *01  MyType TYPEDEF STRICT.
      *    05  var1 type Bool.
      
      *01 MyVar Type MyType.
       01 MyVar.
          02  var1-value PIC X VALUE LOW-VALUE.
              88  var1       VALUE 'T'.
              88  var1-false VALUE 'F'
                             X'00' thru 'S'
                             'U' thru X'FF'.
                            
      
       PROCEDURE DIVISION.
      
       END PROGRAM TestDefBool.
      
