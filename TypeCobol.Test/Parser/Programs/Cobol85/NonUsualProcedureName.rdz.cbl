       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPgm.
       data division.
       working-storage section.
      
       procedure division.
      
       REPLACE ==:-MAJ:== BY ====.
      *Ok
           PERFORM EACGAPEL-C0001:-MAJ:.

      *Ok
           perform
           300-000-PAR2
      *Ok
           perform 300-000-SEC1
           perform 300 TIMES
           perform 300-000-PAR2 THRU 300-000-SEC1
           perform 300-000-PAR2 THRU 300-000-SEC1 300 TIMES
           perform 300-000-PAR3 OF 300-000-SEC1 300 TIMES
           perform 300-000-PAR3 OF 300-000-SEC1 THRU 
                   300-000-PAR3 OF 300-000-SEC1 300 TIMES
           goback.
           REPLACE ==:PAR1:== BY ==PAR2==.
      *Ok
      
       300-000-:PAR1:.
           exit.
      *---------------------------------------------------------------*
       EACGAPEL-C0001-TRACE.
           continue
           .
      *---------------------------------------------------------------*
       EACGAPEL-C0001.
           continue
           .
       EACGAPEL-C0002.
           continue
           .
      *Ok
       300-000-SEC1 section.
           DISPLAY "IN SECTION".
       300-000-PAR3.
           exit.
       END PROGRAM MyPgm.
