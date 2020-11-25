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
              display 'before perform pararec2 in pararec'
              perform pararec2
              display 'after perform pararec2 in pararec'
           else
              display 'not done in pararec'
              perform pararec3
           end-if
           display 'terminate in pararec'.
       pararec2.
           add 1 to n
           display n
           if n < 3
              display 'before perform pararec in pararec2'
              perform pararec
              display 'after perform pararec in pararec2'
           else
              display 'not done in pararec2'
           end-if
           display 'terminate in pararec2'.     
       pararec3.
           add 1 to n
           display n
           if n < 3
              display 'before perform pararec2 in pararec3'
              perform pararec2
              display 'after perform pararec2 in pararec3'
           else
              display 'not done in pararec3'
           end-if
           display 'terminate in pararec3'.
       END PROGRAM perfrecur.
       