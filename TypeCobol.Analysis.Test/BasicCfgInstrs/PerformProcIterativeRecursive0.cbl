       identification division.
       program-id.  perfrecur.
       data division.
       working-storage section.
       77  n pic 9(4) comp value zero.
      
       procedure division.
           perform pararec varying n from 1 by 1 until n > 3
           goback.
      
       pararec.
           add 1 to n
           display n
           if n < 3
              display 'before perform'
              perform pararec  varying n from 1 by 1 until n > 5
              display 'after perform'
           else
              display 'not done'
           end-if
           display 'terminate'.
       END PROGRAM perfrecur.
       