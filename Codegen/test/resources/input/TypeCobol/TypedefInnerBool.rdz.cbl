       IDENTIFICATION DIVISION.
       PROGRAM-ID. TestDefBool.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370
                      .
       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
       01  MyBool TYPE BOOL.
       01  MyType TYPEDEF STRICT.
           05  var1 type Bool.
      
       01 MyVar Type MyType.
      
       PROCEDURE DIVISION.
      
       END PROGRAM TestDefBool.
      