       01  FOOFRW.
         02 FOOFRW-COB.
     
           05 FOOFRW-EME.
             10 FOOFRW-EME-PFO                   pic X(050).
           05 FOOFRW-DAT                         pic X(008).
           05 FOOFRW-HEU                         pic X(008).
      
           05 FOOFRW-CPY.
              10 FOOFRW-CPY-ZON                     pic X.
              10 FOOFRW-CPY-HER                     pic X.
      
              10 FOOFRW-PAY                     pic X(02).
           05 FOOFRW-LOG.
             10 FOOFRW-LOG-LVL                   pic X.
             10 FOOFRW-LOG-APPEND                pic X.
             10 FOOFRW-LOG-MOD                   pic X(01).
             10 FOOFRW-LOG-PFX                   pic X(05).
      
      
      
          02 FOOFRW-COB-LGR PIC S9(9) BINARY.
          02 FOOFRW-DD.
           05 FILX0000 PIC X(96).
           05 FILX0001 PIC X(96).
           05 FILX0002 PIC X(96).
           05 FILX0003 PIC X(96).
           05 FILX0004 PIC X(96).
           05 FILX0005 PIC X(96).
           05 FILX0006 PIC X(96).
           05 FILX0007 PIC X(96).
           05 FILX0008 PIC X(96).
           05 FILX0009 PIC X(96).
           05 FILX0011 PIC X(96).
           05 FILX0012 PIC X(96).
           05 FILX0013 PIC X(96).
           05 FILX0014 PIC X(96).
           05 FILX0015 PIC X(96).
           05 FILX0016 PIC X(96).
           05 FILX0017 PIC X(96).
           05 FILX0018 PIC X(96).
           05 FILX0019 PIC X(96).
           05 FILX0020 PIC X(96).
           05 FILX0021 PIC X(96).
           05 FILX0022 PIC X(96).
           05 FILX0023 PIC X(96).
           05 FILX0024 PIC X(96).
           05 FILX0025 PIC X(96).
           05 FILX0026 PIC X(96).
           05 FILX0027 PIC X(96).
           05 FILX0028 PIC X(96).
           05 FILX0029 PIC X(96).
           05 FILX0030 PIC X(96).
           05 FILX0031 PIC X(96).
          02 FOOFRW-LGMAX-LTNV PIC S9(9) BINARY VALUE 1029.
