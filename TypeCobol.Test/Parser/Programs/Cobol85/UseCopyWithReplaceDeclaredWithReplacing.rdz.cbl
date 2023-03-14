       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CopyWithReplace REPLACING ==WORD== BY ==var1==.
      *Expect following variables after "copy" is imported and 
      *"replacing/replace" applied
      *Current Limitation of the parser : Replacing is not applied on "Replace"
      *  WORD
      *  Var1
      *  Var1
      *  ENDWORD
       END PROGRAM MyPGM.