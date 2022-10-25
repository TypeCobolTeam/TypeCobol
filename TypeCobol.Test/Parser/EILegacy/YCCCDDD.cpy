      *Removed
       01 CCCDDD.
      *Suffixed twice
          05  CCCDDD-RET-CCCDDD-var1.
      
      /
      *            suffixed
             10  CCCDDD-RootLv.
      
      *            suffixed
                15  CCCDDD-ErrorCode.
      
      *            Not suffixed
                   20  CCCDDD-ErrorCode-Var                  PIC X(008).
      *            Not suffixed
                   20  CCCDDD-ErrorCode-CCCDDD-Var2         PIC X(008).
      
      *            Not suffixed
                   20             CCCDDD-ErrorCode-ErrorMsg1 PIC X(055).
      *            Not suffixed
                   20                         CCCDDD-ErrorCode-ErrorMsg2
                     PIC X(030).
      *            29 chars: suffixing ok
                   20  CCCDDD-a901234567890123456789  PIC X(001).
      *            30 chars:
      *                suffixing is ok but resulting name is too long
                   20  CCCDDD-a9012345678901234567890 PIC X(001).
      
      *            Not suffixed
                   20  CCCDDD-Var20 PIC X(99) value 'ccccccccccccccccccc
      -    'ddddd'.
      *            Ko value is altered
                   20  CCCDDD-Var21 PIC X(99) value 'cccccccccccccccccc
      -    'ddddd'.
      *            Ok
                   20  CCCDDD-Var22 PIC X(99)
                                               value 'cccccccccccccccccc
      -    'ddddd'.
      
      *            suffixed
                15  FILLER REDEFINES CCCDDD-ErrorCode.
                    20 FILLER                          PIC X(008).
      *Not Removed
      D 01 CCCDDD.
      *Suffixed twice
      D   05  CCCDDD-RET-CCCDDD-var1.
      
      /
      *            suffixed
      D      10  CCCDDD-RootLv.
      
      *            suffixed
      D         15  CCCDDD-ErrorCode.
      
      *            Not suffixed
      D            20  CCCDDD-ErrorCode-Var                  PIC X(008).
      *            Not suffixed
      D            20  CCCDDD-ErrorCode-CCCDDD-Var2         PIC X(008).
      
      *            Not suffixed
      D            20             CCCDDD-ErrorCode-ErrorMsg1 PIC X(055).
      *            Not suffixed
      D            20                         CCCDDD-ErrorCode-ErrorMsg2
      D              PIC X(030).
      *            29 chars: suffixing ok
      D            20  CCCDDD-a901234567890123456789  PIC X(001).
      *            30 chars:
      *                suffixing is ok but resulting name is too long
      D            20  CCCDDD-a9012345678901234567890 PIC X(001).
      
      *            Not suffixed
      D            20  CCCDDD-Var20 PIC X(99) value 'ccccccccccccccccc'.
      
      *            suffixed
      D         15  FILLER REDEFINES CCCDDD-ErrorCode.
      D             20 FILLER                          PIC X(008).






















