000001 COPY CPY5 REPLACING A BY PAYROLL 
000002                     B BY PAY-CODE 
000003                     C BY GROSS-PAY 
000004                     D BY HOURS.
000005 
000006 COPY CPY6 REPLACING ==:TAG:== BY ==Payroll==.
000007 
      *Currently unsupported, see issues #2077 and #2078
000008 COPY CPY7 REPLACING ==(01)== BY ==(01)==
000009                     == 01 == BY == 05 ==.
