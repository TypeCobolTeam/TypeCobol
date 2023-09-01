       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZS0OSM.
       data division.
       working-storage section.
       REPLACE TRAILING ==C-Nb==                 BY ==1==
               TRAILING ==C-NbX==                BY ==3==
               TRAILING ==(C-Nb)==               BY ==5==
               TRAILING ==Nb==                   BY ==7 9==
          .
      
      *Parenthesis separator  -  Replace OK
       01 Var1     PIC X(C-Nb).  *> Result = X(1)
       01 Var1     PIC X(XC-Nb). *> Result = X(X1)
      
      *Semi colon BEFORE - Word to replace at end -  Replace OK
       01 Var:C-Nb    pic X.  *> Result = Var:1
       01 Var:XC-Nb   pic X.  *> Result = Var:X1
      
      *Semi colon AFTER - Word to replace before -  Replace OK
       01 C-Nb:Var    pic X.   *> Result = 1:Var
       01 XC-Nb:Var   pic X.   *> Result = X1:Var
      
      *Semi colon AFTER -  Word to replace at end -  Replace OK
       01  VarC-Nb:    pic X.   *> Result = Var1:
       01  VarXC-Nb:   pic X.   *> Result = VarX1:
      
      *Semi colon AFTER -  Word to replace before -  Replace KO
       01 C-NbVar:    pic X.
       01 XC-NbVar:   pic X.
      
      *Parenthesis BEFORE  -  Replace OK
       01 Var(C-Nb    pic X. *> Result = Var(1
       01 Var(XC-Nb   pic X. *> Result = Var(X1
      
      *Parenthesis AFTER  -  Replace OK
       01 VarC-Nb)    pic X. *> Result = Var1)
       01 VarXC-Nb)   pic X. *> Result = VarX1)
      
      *Semi colons AROUND -  Replace OK
       01 Var:C-Nb:    pic X. *> Result = Var:1:
       01 Var:XC-Nb:   pic X. *> Result = Var:X1:
      
      *Literals  -  Replace KO
       01 Var0    pic X(10) value ' C-Nb' .
       01 Var0    pic X(10) value 'C-Nb'.
      
       01 Var0    pic X(10) value " C-Nb" .
       01 Var0    pic X(10) value " C-Nb".
      
      *No separator     -  Replace KO
       01 C-NbVar     PIC X.
       01 XC-NbVar    PIC X.
      
      *No separator     -  Replace OK
       01 VarC-Nb     PIC X. *> Result = Var1
       01 VarXC-Nb    PIC X. *> Result = VarX1
      
      *Comma separator  -  Replace OK
      *It's a known bug in our parser
       01 Var,C-Nb    pic X. *> Result = Var,1
       01 Var,XC-Nb   pic X. *> Result = Var,X1
      
      *Comma separator  -  Replace KO
      *It's a known bug in our parser
       01 C-Nb,Var    pic X.
       01 XC-Nb,Var   pic X.
      
       end program DVZS0OSM.