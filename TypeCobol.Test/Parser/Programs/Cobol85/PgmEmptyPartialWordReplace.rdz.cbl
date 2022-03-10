       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOJCM01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
      
       PROCEDURE DIVISION.
      
       REPLACE ==:-MAJ:== BY ====.
           PERFORM EACGAPEL-C0001:-MAJ:.
       REPLACE ==:-MAJ0:== BY ==   ==.
           PERFORM EACGAPEL-C0002:-MAJ0:.
       REPLACE ==:EMPTY:== BY ====.
      *---------------------------------------------------------------*
       EACGAPEL-C0001-TRACE.
           continue
           :EMPTY:.
      *---------------------------------------------------------------*
       EACGAPEL-C0001.
           continue
           .
       EACGAPEL-C0002.
           continue
           .
      
       END PROGRAM TCOJCM01.
      