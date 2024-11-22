       IDENTIFICATION DIVISION.
       PROGRAM-ID. TokenProperties.
      * Use this sample to test various tokens and their properties
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * For imported tokens
       COPY CopyTokenProperties.
       01 var1 PIC X.
      * For replaced tokens
       REPLACE ==:SUFFIX:== BY ==replaced==.
       01 var1-:SUFFIX: PIC X.
       REPLACE ==TabSize== BY ==80==.
       01 exempleTab.
          05 item PIC X OCCURS TabSize.
       REPLACE ==to create a ReplacedTokenGroup==
            BY ==var1-group-replaced==.
       01 to create a ReplacedTokenGroup PIC X.
      * For continuation token
       01 var1-continued PIC X(1000) VALUE 'This value is continued on t
      -    'he next line'.
       PROCEDURE DIVISION.
           GOBACK
           .
       missingToken.
      * ANTLR will create a missing token for this statement
           INITIALIZE var1 WITH DEFAULT
           .
       END PROGRAM TokenProperties.