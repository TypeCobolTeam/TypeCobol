       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZS0OSM.
       data division.
       working-storage section.
       REPLACE LEADING ==C-Nb==                 BY ==1==
               LEADING ==C-NbX==                BY ==3==
          .
      
      *Parenthesis separator  -  Replace APPLY
       01 Var1     PIC X(C-Nb).  *> Result = X(1)
       01 Var1     PIC X(C-NbX). *> Result = X(1X)
      
      *Semi colon BEFORE - Word to replace at end -  Replace APPLY
       01 Var:C-Nb    pic X.  *> Result = Var:1
       01 Var:C-NbX   pic X.  *> Result = Var:1X
      
      *Semi colon AFTER - Word to replace before -  Replace APPLY
       01 C-Nb:Var    pic X.   *> Result = 1:Var
       01 C-NbX:Var   pic X.   *> Result = 1X:Var
      
      *Semi colon AFTER -  Word to replace at end -  Replace DONT apply
       01 VarC-Nb:    pic X.
       01 VarC-NbX:   pic X.
      *Parenthesis BEFORE  -  Replace APPLY
       01 Var(C-Nb    pic X. *> Result = Var(1
       01 Var(C-NbX   pic X. *> Result = Var(1X
      
      *Parenthesis AFTER  -  Replace DONT apply
       01 VarC-Nb)    pic X.
       01 VarC-NbX)   pic X.
      
      *Semi colons AROUND -  Replace APPLY
       01 Var:C-Nb:    pic X. *> Result = Var:1:
       01 Var:C-NbX:   pic X. *> Result = Var:1X:
      
      
      *Literals  -  Replace don't apply
       01 Var0    pic X(10) value ' C-Nb' .
       01 Var0    pic X(10) value 'C-Nb'.
      
       01 Var0    pic X(10) value " C-Nb" .
       01 Var0    pic X(10) value " C-Nb".
      
      *No separator     -  Replace don't apply
       01 VarC-Nb     PIC X.
       01 VarC-NbX    PIC X.
      
      *Comma separator  -  Replace don't apply
       01 Var,C-Nb    pic X.
       01 Var,C-NbX   pic X.
      
      *Comma separator  -  Replace APPLY
       01 C-Nb,Var    pic X. *> Result = 1,Var
       01 C-NbX,Var   pic X. *> Result = 1X,Var
      
       end program DVZS0OSM.