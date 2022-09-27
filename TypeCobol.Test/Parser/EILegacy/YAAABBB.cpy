          05  Group2.

      *            Suffixed
                   20  AAABBB-Var0                          PIC X(008). 
      *            Not suffixed
                   20  AAABBB-Var1                           PIC X(008). 
      *            Not suffixed as there are 2 suffix to add                   
                   20  AAABBB-Var2-AAABBB-Var2              PIC X(008).
      *            Not suffixed    
                   20                            AAABBB-Var3 PIC X(008). 
      *            OK suffixed    
                   20                                       AAABBB-Var5
                     PIC X(004).
      *            Not suffixed    
                   20                                        AAABBB-Var6
                     PIC X(004).

      *            Not suffixed
                   20  AAABBB-Var20 PIC X(99) value 'aaaaaaaaaaaaaaaaaaa
      -    'bbbbb'.

      *            Not suffixed + error
                   20  AAABBB-Var21 PIC X(99) value 'aaaaaaaaaaaaaaaaaa
      -    'bbbbb'.

      *            Ok suffixed
                   20  AAABBB-Var22 PIC X(99)
                                               value 'aaaaaaaaaaaaaaaaaa
      -    'bbbbb'.