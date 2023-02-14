       IDENTIFICATION division.
       PROGRAM-ID. MyPGM.
       data division.
       working-storage section.
       01  MyArray.
          05 TAB-EXC-ELE occurs 1 to 50
                          indexed by TAB-EXC-IDX.
             10 TAB-EXC-CLE   pic X(08).

       procedure division.
           perform varying TAB-EXC-IDX from 1 by 1
                   until   TAB-EXC-IDX > 50

                    TODO : case1.
                    TODO : case2.
           end-perform
           .
       end program MyPGM.