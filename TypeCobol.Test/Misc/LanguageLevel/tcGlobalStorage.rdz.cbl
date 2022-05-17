       IDENTIFICATION DIVISION.
       PROGRAM-ID. tcGlobalStorage.
            SERVICE ID IS someCopy IN someLib.
       DATA DIVISION.
       GLOBAL-STORAGE SECTION.
       01 myVar PIC X.       
       PROCEDURE DIVISION.
           GOBACK
            .
       END PROGRAM tcGlobalStorage.