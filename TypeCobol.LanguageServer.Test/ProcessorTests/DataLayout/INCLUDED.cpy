       02 root-included.
          05 grp-included.
             10 var-included-1 PIC X.
             10 FILLER PIC X.
             10 var-included-occ PIC X OCCURS 3.
          05 grp-included-redef REDEFINES grp-included.
             10 var-included-2 PIC X(5).