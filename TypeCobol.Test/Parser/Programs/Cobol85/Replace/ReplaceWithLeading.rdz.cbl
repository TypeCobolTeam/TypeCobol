       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZS0OSM.
       data division.
       working-storage section.
       REPLACE LEADING ==C-Nb==                 BY ==1==
               LEADING ==C-NbX==                BY ==3==
               LEADING ==(C-Nb)==               BY ==5==
               LEADING ==Nb==                   BY ==7 9==
          .
      
      *Parenthesis separator  -  Replace OK
       01 Var1     PIC X(C-Nb).  *> Result = X(1)
       01 Var1     PIC X(C-NbX). *> Result = X(1X)
      
      *Semi colon BEFORE - Word to replace at end -  Replace OK
       01 Var:C-Nb    pic X.  *> Result = Var:1
       01 Var:C-NbX   pic X.  *> Result = Var:1X
      
      *Semi colon AFTER - Word to replace before -  Replace OK
       01 C-Nb:Var    pic X.   *> Result = 1:Var
       01 C-NbX:Var   pic X.   *> Result = 1X:Var
      
      *Semi colon AFTER -  Word to replace at end -  Replace KO
       01 VarC-Nb:    pic X.
       01 VarC-NbX:   pic X.

      *Semi colon AFTER -  Word to replace before -  Replace OK
       01 C-NbVar:    pic X.   *> Result = 1Var:
       01 C-NbXVar:   pic X.   *> Result = 1XVar:
      
      *Parenthesis BEFORE  -  Replace OK
       01 Var(C-Nb    pic X. *> Result = Var(1
       01 Var(C-NbX   pic X. *> Result = Var(1X
      
      *Parenthesis AFTER  -  Replace KO
       01 VarC-Nb)    pic X.
       01 VarC-NbX)   pic X.
      
      *Semi colons AROUND -  Replace OK
       01 Var:C-Nb:    pic X. *> Result = Var:1:
       01 Var:C-NbX:   pic X. *> Result = Var:1X:
      
      *Literals  -  Replace KO
       01 Var0    pic X(10) value ' C-Nb' .
       01 Var0    pic X(10) value 'C-Nb'.
      
       01 Var0    pic X(10) value " C-Nb" .
       01 Var0    pic X(10) value " C-Nb".
      
      *No separator     -  Replace OK
       01 C-NbVar     PIC X. *> Result = 1Var
       01 C-NbXVar    PIC X. *> Result = 1XVar
      
      *No separator     -  Replace KO
       01 VarC-Nb     PIC X.
       01 VarC-NbX    PIC X.
      
      *Comma separator  -  Replace KO
      *It's a known bug in our parser
       01 Var,C-Nb    pic X.
       01 Var,C-NbX   pic X.
      
      *Comma separator  -  Replace OK
      *It's a known bug in our parser
       01 C-Nb,Var    pic X. *> Result = 1,Var
       01 C-NbX,Var   pic X. *> Result = 1X,Var
      
       end program DVZS0OSM.