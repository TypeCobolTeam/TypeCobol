       identification division.
       program-id.  perfrecur.
       data division.
       working-storage section.
       77  n pic 9(4) comp value zero.
      
       procedure division.
           perform pararec
           perform pararec4
           goback.
      
       pararec.
           add 1 to n
           display n
           if n < 3
              display 'before perform pararec2 in pararec'
              perform pararec2
              display 'after perform pararec2 in pararec'
           else
              display 'not done in pararec perform pararec3'
              perform pararec3
              display 'not done in pararec perform pararec4'
              perform pararec4
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
       pararec4.
           add 1 to n
           display n
           if n < 3
              display 'before perform pararec3 in pararec4'
              perform pararec3 test after varying n from 1 by 1 until n > 5
              display 'after perform pararec3 in pararec4'
           else
              display 'Try recurse in pararec4'
              perform pararec4 varying n from 1 by 1 until n > 5
           end-if
           display 'terminate in pararec4'.
       END PROGRAM perfrecur.
       