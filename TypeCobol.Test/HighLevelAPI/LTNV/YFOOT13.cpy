        01 FOOT13.
         02 FOOT13-COB.
           05 FOOT13-LIS.
            07 FOOT13-LIS-HIS-NBR                  PIC 9(4).
            07 filler                              PIC X(50).
            07 FOOT13-LIS-HIG.
      
             10 FOOT13-LIS-HIG-TAB.
              12 FOOT13-LHIG                         OCCURS 15.
               15 FOOT13-LHIG-TAB-LIB              PIC X(50).
               15 FOOT13-LHIG-TAB-MTF              PIC X(50).
               15 FOOT13-LHIG-TAB-DAT-HEU.
                   20 FOOT13-LHIG-TAB-DAT          PIC X(08).
                   20 FOOT13-LHIG-TAB-HEU.
                       25 FOOT13-LHIG-TAB-HH       PIC X(02).
                       25 FOOT13-LHIG-TAB-MM       PIC X(02).
                       25 FOOT13-LHIG-TAB-SS       PIC X(02).
      
           07 filler                               PIC X(500).