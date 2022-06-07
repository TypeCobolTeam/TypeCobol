       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPgm.
       data division.
       working-storage section.
       01 TOTO pic 9.
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
      *Ok Number too big
           perform 300303030303030303030303030303030303030 TIMES

      *TODO Numeric paragraph name are not supported yet
           perform 300

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

       300.
           exit.
       END PROGRAM MyPgm.