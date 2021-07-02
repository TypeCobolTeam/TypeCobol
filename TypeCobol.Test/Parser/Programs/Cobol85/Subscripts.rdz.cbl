       IDENTIFICATION DIVISION.
       PROGRAM-ID. Subscripts.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 tab1.
          05 oc1 occurs 10.
             10 item PIC X.
       01 tab2.
          05 oc1 occurs 10.
             10 oc2 occurs 20.
                15 oc3 occurs 30.
                   20 item PIC X.
       01 tab3.
          05 oc1 occurs unbounded.
             10 item PIC X.
       01 notATable.
          05 group1.
             10 item PIC X.
       01 elt PIC X.
       PROCEDURE DIVISION.
      *OK
           MOVE item OF tab1 (1)          TO elt
           MOVE item OF tab2 (1, 2, 3)    TO elt
           MOVE item OF tab1 (10)         TO elt
           MOVE item OF tab2 (10, 20, 30) TO elt
           MOVE item OF tab3 (9999999999) TO elt
      *KO
           MOVE item OF tab1 (0)          TO elt
           MOVE item OF tab1 (-20)        TO elt
           MOVE item OF tab1 (20)         TO elt
           MOVE item OF tab2 (20, 30, 40) TO elt
           MOVE item OF notATable (9)     TO elt
           MOVE item of tab1              TO elt
           MOVE item of tab2 (1)          TO elt
           MOVE item of tab1 (1 2)        TO elt
           MOVE item of tab2 (1 2 3 4)    TO elt
           GOBACK
           .
       END PROGRAM Subscripts.