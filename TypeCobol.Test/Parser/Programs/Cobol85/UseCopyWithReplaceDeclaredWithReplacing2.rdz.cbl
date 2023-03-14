       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CopyWithReplace2 REPLACING ==WORD1== BY ==WORD2==.

      *Expect following variables after "copy" is imported and 
      *"replacing/replace" applied
      *Current Limitation of the parser : Replacing is not applied on "Replace"
      *  WORD2

      *  WORD2
      *  WORD2

      *  WORD3
      *  WORD3
       END PROGRAM MyPGM.