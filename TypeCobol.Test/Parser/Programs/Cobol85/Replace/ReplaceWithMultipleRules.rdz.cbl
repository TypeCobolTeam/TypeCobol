       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZS0OSM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
       data division.
       working-storage section.
       REPLACE
          ==C-Nb==                 BY ==1==
          ==C-NbX==                BY ==3==
          ==3== by ==4==
      
          ==:TOTO:==              BY ==3==
          .
      
      *Parenthesis separator  -  Replace APPLY
       01 Var         pic X(C-Nb).
       01 Var         pic X(C-NbX).
       
      *Semi colon separator   -  Replace APPLY
       01 Var:C-Nb    pic X.
       01 Var:C-NbX   pic X.
      *Parenthesis separator  -  Replace APPLY
       01 Var(C-Nb    pic X.
       01 Var(C-NbX   pic X.
      *Semi colon separator  -  Replace APPLY
      *Bug: replace mechanism will replace the whole token
      *since the replace operation is considered a single replacement token
       01 Var:C-Nb:    pic X.
       01 Var:C-NbX:   pic X.
      

      *No separator     -  Replace don't apply
       01 VarC-Nb     PIC X.
       01 VarC-NbX    PIC X.
      
      *Comma separator  -  Replace don't apply
      *Bug: our parser will produce 3 tokens here and the replace
      *mechanism will match the replace operation on 1 token at a time
       01 Var,C-Nb    pic X.
       01 Var,C-NbX   pic X.
      
      
      
      
       procedure division.
      
           goback
           .
       end program DVZS0OSM.