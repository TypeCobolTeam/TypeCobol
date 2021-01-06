       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZZMFT3.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        
       01 GROUP1.
      *   05 GROUP1-DATA1 PIC X.
       exec sql include YxxxDAT replacing ==:Prefix:== by ==GROUP1==
       end-exec.
       
       01 GROUP2.
      *   05 GROUP2-DATA1 PIC X.
       exec sql include
                        YxxxDAT replacing
                        ==:Prefix:== by ==GROUP2==
       end-exec.
        
       01 GROUP3.
      *   05 GROUP3-DATA1 PIC X.
       exec sql
                include YxxxDAT
                replacing ==:Prefix:== by ==GROUP3==
       end-exec.
        
       01 GROUP4.
      *   05 GROUP4-DATA1 PIC X.
       exec
            sql include YxxxDAT
            replacing
            ==:Prefix:== by ==GROUP4==
       end-exec.
        
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM DVZZMFT3.