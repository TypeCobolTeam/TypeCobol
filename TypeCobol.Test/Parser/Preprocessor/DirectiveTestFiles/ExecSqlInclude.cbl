000001 EXEC SQL INCLUDE TEXTNAME1 SUPPRESS END-EXEC.
000002 execute 
000002   sql 
000003    include textname2 of library2
000004    replacing
000005       == toto == by == titi ==
000006       == exec == by == execute ==
000007 end-exec.

000001 exec sql Truncate table toto end-exec.
000002 exec include textname3 end-exec.
000003 exec sql include end-exec.
000004 exec sql include textname4 end-exec
000005 exec sql include textname5.
000006 exec 
000007 sql select
000008      * from any
000009 end-exec.
000007 exec sql include textname6 endexec.
000008 display "ok ?".
000009 exec sql delete * from table end-exec. 

000001 exec sql include textname7
000002 end-exec.
000003 
000004 exec sql include
000005                  textname8
000006 end-exec.
000007 
000008 exec sql
000009          include textname9
000010 end-exec.
000000 
000008 exec
000009      sql include textnameA
000010 end-exec.