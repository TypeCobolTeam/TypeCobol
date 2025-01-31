       IDENTIFICATION DIVISION.
       PROGRAM-ID. DVZF0OSM.
       DATA DIVISION.
       Working-storage section.
       REPLACE ==/PAIBRGA-NB-MAX/== BY ==300==
               ==_PAIBRGA-NB-MAX_== BY ==300==.
      
       01  W-TAB.
      *No error expected
           05 W-TAB1 OCCURS /PAIBRGA-NB-MAX/.
             10 W-COD      PIC X(002).
      
      *No error expected
           05 W-TAB1 OCCURS _PAIBRGA-NB-MAX_.
             10 W-COD      PIC X(002).
      
      *Replace will not match because / is not a separator for partial
      *cobol word 
       01 W-Var/PAIBRGA-NB-MAX/ pic X.
      *It workds because character _ is allowed in variable name but
      *the replace will not takes place here
       01 W-Var_PAIBRGA-NB-MAX_ pic X.
       
       END PROGRAM DVZF0OSM.