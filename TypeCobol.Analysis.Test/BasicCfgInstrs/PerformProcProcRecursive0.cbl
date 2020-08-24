       identification division.
       program-id.  perfrecur.
       data division.
       working-storage section.
       77  n pic 9(4) comp value zero.
      
       procedure division.
           perform pararec
           goback.
      
       pararec.
           add 1 to n
           display n
           if n < 3
              display 'before perform'
              perform pararec2
              display 'after peform'
           else
              display 'not done'
           end-if
           display 'terminate'.
       pararec2.
           add 1 to n
           display n
           if n < 3
              display 'before perform'
              perform pararec
              display 'after peform'
           else
              display 'not done'
           end-if
           display 'terminate'.           
       END PROGRAM perfrecur.
