       identification division.
       program-id. TCOMFL02.
       data division.
       working-storage section.
      
       REPLACE ==:tag:== BY ==name1==.
       01 var-:tag: PIC X.
       REPLACE ==:tag:== BY ==name2==.
       01 : tag:-var PIC X.
       REPLACE ==:tag:== BY ==name3==.
       01 var-:tag : PIC X.
       REPLACE ==:tag:== BY ==name4==.
       01 : tag :var: tag : PIC X.
      
       REPLACE ==: tag:== BY ==name5==.
       01 var-:tag: PIC X.
       REPLACE ==: tag:== BY ==name6==.
       01 : tag:-var PIC X.
       REPLACE ==: tag:== BY ==name7==.
       01 var-:tag : PIC X.
       REPLACE ==: tag:== BY ==name8==.
       01 : tag :var: tag : PIC X.
      
       REPLACE ==:tag :== BY ==name9==.
       01 var-:tag: PIC X.
       REPLACE ==:tag :== BY ==name10==.
       01 : tag:-var PIC X.
       REPLACE ==:tag :== BY ==name11==.
       01 var-:tag : PIC X.
       REPLACE ==:tag :== BY ==name12==.
       01 : tag :var: tag : PIC X.
      
       REPLACE ==: tag :== BY ==name13==.
       01 var-:tag: PIC X.
       REPLACE ==: tag :== BY ==name14==.
       01 : tag:-var PIC X.
       REPLACE ==: tag :== BY ==name15==.
       01 var-:tag : PIC X.
       REPLACE ==: tag :== BY ==name16==.
       01 : tag :var: tag : PIC X.
      
       procedure division.
           display var-name1
           display name2-var
           display var-name3
           display name4varname4
           display var-name5
           display name6-var
           display var-name7
           display name8varname8
           display var-name9
           display name10-var
           display var-name11
           display name12varname12
           display var-name13
           display name14-var
           display var-name15
           display name16varname16
           goback
           .
       end program TCOMFL02.