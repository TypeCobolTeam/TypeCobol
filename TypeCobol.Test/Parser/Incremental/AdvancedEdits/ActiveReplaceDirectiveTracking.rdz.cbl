       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyPGM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       replace ==:TAG:== by ==Var==.

       01 :TAG:1 pic X.
       01 :TAG:2 pic X.





































       01 :TAG:3 pic X.
       01 :TAG:4 pic X.
       01 :TAG:5 pic X.
       procedure division.
           move :TAG:1 to :TAG:5
           goback
           .
       END PROGRAM MyPGM.