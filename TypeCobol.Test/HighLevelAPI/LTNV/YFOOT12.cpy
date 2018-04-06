        01 FOOT12.
         02 FOOT12-COB.
           05 FOOT12-LIS.
            07 FOOT12-LIS-HIG-NBR                  PIC 9(4).
            07 filler                              PIC X(50).
            07 FOOT12-LIS-HIG.
      
             10 FOOT12-LIS-HIG-TAB.
              12 FOOT12-LHIG                         OCCURS 15.
               15 FOOT12-LHIG-TAB-LIB              PIC X(50).
               15 FOOT12-LHIG-TAB-MTF              PIC X(50).
               15 FOOT12-LHIG-TAB-DAT-HEU.
                   20 FOOT12-LHIG-TAB-DAT          PIC X(08).
                   20 FOOT12-LHIG-TAB-HEU.
                       25 FOOT12-LHIG-TAB-HH       PIC X(02).
                       25 FOOT12-LHIG-TAB-MM       PIC X(02).
                       25 FOOT12-LHIG-TAB-SS       PIC X(02).
      