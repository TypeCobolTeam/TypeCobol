       IDENTIFICATION DIVISION.                                         
      *$$IDENTIFICATION                                                 
       PROGRAM-ID.    CGS100.                                           
       AUTHOR.        ANALYST:                                          
                      PROGRAMMER:                                       
       DATE-WRITTEN.  02/26/15.                                         
       DATE-COMPILED.                                                   
                                                                        
      ***************************************************************** 
      * SOURCE GENERE PAR INSTALL/1                                   * 
      * MODEL UTILISE = CTS011                                        * 
      * VERSION = LINUX                                               * 
      *                                                               * 
      * SERVICE                                                       * 
      * IDEM CTS010 AVEC ZONE SUPPLEMENTAIRE PASSEE (WAZIS)           * 
      *                                                               * 
      ***************************************************************** 
      *                                                               * 
      *  NAME       : &PRGRMID.                                       * 
      *  NAME       : CGS100                                          * 
      *                                                               * 
      ***************************************************************** 
                                                                        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
         SPECIAL-NAMES.                                                 
           SYMBOLIC CHARACTERS APOSTR IS 126.                           
                                                                        
       DATA DIVISION.                                                   
      ***************************************************************** 
      *                                                               * 
      *              W O R K I N G    S T O R A G E                   * 
      *                                                               * 
      ***************************************************************** 
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 LT-BEGIN-OF-WS.                                               
           03 LT-START-OF-WS                       PIC X(42)            
      *        VALUE "***** BEGIN WORKING STORAGE &PRGRMID. *****".     
               VALUE "***** BEGIN WORKING STORAGE CGS100 *****".        
                                                                        
      *COPY CQOMC0WK.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copyb/CQOMC0WK.CPY"  CQOMC0WK
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CQOMC0WK
      *LibFac  liberFactory GCOS to Linux Translator  *                 CQOMC0WK
      *LibFac  From input file: CQOMC0WK              *                 CQOMC0WK
      *LibFac  On   the   date: 2015/03/23            *                 CQOMC0WK
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           CQOMC0WK
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CQOMC0WK
      ***************************************************************** CQOMC0WK
      *                                                               * CQOMC0WK
      * >>>>>>>>>> > >  NE  PAS REGENERER  < < <<<<<<<<<<<<<<<<<<<<<< * CQOMC0WK
      *                 +++++++++++++++++                             * CQOMC0WK
      *   COPYBOOK NAME  : CQOMC0WK                           BATCH   * CQOMC0WK
      *                                                               * CQOMC0WK
      *   COPYBOOK DESC  : INTERFACE ACCES MOD MESU                   * CQOMC0WK
      *                                                               * CQOMC0WK
      * 12/98: MODIF ZONES SPECIFIQUES A CQSRVB                       * CQOMC0WK
      *                                                               * CQOMC0WK
      ***************************************************************** CQOMC0WK
       01  PERF-CQOMC0WR.                                               CQOMC0WK
           11 PERF-CQOMC00.                                             CQOMC0WK
             16 PERF-NDTHEU                      PIC S9(15) COMP-3.     CQOMC0WK
             16 PERF-ZAPPLT                      PIC X(8).              CQOMC0WK
             16 PERF-D4A2MJ                      PIC S9(9) COMP-3.      CQOMC0WK
             16 PERF-NHR8PO                      PIC S9(9) COMP-3.      CQOMC0WK
             16 PERF-ZTYPAP                      PIC X(1).              CQOMC0WK
             16 PERF-CCOD1C                      PIC X(1).              CQOMC0WK
             16 PERF-CCOD1C0                     PIC X(1).              CQOMC0WK
             16 PERF-PARA01                      PIC X(1).              CQOMC0WK
             16 PERF-PIOCDE                      PIC X(28).             CQOMC0WK
             16 PERF-D1-USER-ID                  PIC X(8).              CQOMC0WK
             16 PERF-CWCURCNV                    PIC X(4).              CQOMC0WK
             16 PERF-PARA04                      PIC X(4).              CQOMC0WK
             16 PERF-PISRVC                      PIC X(12).             CQOMC0WK
             16 PERF-PIDMOD                      PIC X(8).              CQOMC0WK
             16 PERF-PTYENT                      PIC X(8).              CQOMC0WK
             16 PERF-PIDENT                      PIC X(8).              CQOMC0WK
             16 PERF-PARA05                      PIC X(5).              CQOMC0WK
             16 PERF-NNBR5C                      PIC S9(5) COMP-3.      CQOMC0WK
             16 PERF-DISPLAY                     PIC X.                 CQOMC0WK
             16 PERF-TRT-RETOUR                  PIC X.                 CQOMC0WK
             16 PERF-NBA9PO0                     PIC S9(15) COMP-3.     CQOMC0WK
             16 PERF-APPLICATIF.                                        CQOMC0WK
                 18 PERF-APPL-ZNOCCD             PIC S99.               CQOMC0WK
                 18 PERF-APPL-ZL0100             PIC X(70).             CQOMC0WK
      *01  PERF-TRAITEMENT                       COMP-6 VALUE ZERO.     CQOMC0WK
       01  PERF-TRAITEMENT                                              CQOMC0WK
       PIC S9(00009) COMP-5 SYNC                        VALUE ZERO.     CQOMC0WK
                 88 PERF-TRT-OUI                 VALUE ZERO.            CQOMC0WK
                 88 PERF-TRT-NON                 VALUE 1.               CQOMC0WK
       01 SV-NAME-OF-THIS-PROGRAM                PIC X(8) VALUE SPACE.  CQOMC0WK
      ******************************************************************CQOMC0WK
      * FCN  STR1403    LE  06/02/97                                   *CQOMC0WK
      *                                                                *CQOMC0WK
      *  AJOUT DES ZONES DE GESTION DES CODES RETOUR BATCH             *CQOMC0WK
      *                                                                *CQOMC0WK
      ******************************************************************CQOMC0WK
       01  ANO-MSG-TOPS.                                                CQOMC0WK
           05  WS-ANO-04                 PIC  X(1).                     CQOMC0WK
               88  WS-ANO-04-NON-TROUVE                  VALUE '0'.     CQOMC0WK
               88  WS-ANO-04-TROUVE                      VALUE '1'.     CQOMC0WK
           05  WS-ANO-AUTRE              PIC  X(2)       VALUE '  '.    CQOMC0WK
       01  ANO-MSG-AREA.                                                CQOMC0WK
           05  ANO-MSG-ANOCRIST                 PIC X(8)                CQOMC0WK
                                                VALUE 'ANOCRIST'.       CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-CDORIG.                                          CQOMC0WK
                10  ANO-MSG-CDDEP               PIC X(2).               CQOMC0WK
                10  ANO-MSG-CDTRAIT             PIC X(8).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-TYTRT                    PIC X(1).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-NOMCH                    PIC X(6).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-NOMMD                    PIC X(8).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-DOSSX.                                           CQOMC0WK
               10  ANO-MSG-DOSS                 PIC 9(7).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-SUPPT                    PIC X(1).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-CDRESP                   PIC X(1).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-NOMENT                   PIC X(8).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-IDENT                    PIC X(6).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-ZFONCT                   PIC X(3).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-TABEND                   PIC X(1).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-CABEND                   PIC X(4).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-ZRTGENX.                                         CQOMC0WK
               10  ANO-MSG-ZRTGEN               PIC 9(3).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-ZRTDETX.                                         CQOMC0WK
               10  ANO-MSG-ZRTDET               PIC 9(5).               CQOMC0WK
           05  FILLER                           PIC X(1) VALUE '/'.     CQOMC0WK
           05  ANO-MSG-LIBELLE                  PIC X(61).              CQOMC0WK
      ******************************************************************CQOMC0WK
      *                                                                 CQOMC0WK
      *    AJOUT DE ZONES POUR APPEL DE MODULES A TRAVERS CLIMB         CQOMC0WK
      *                                                                 CQOMC0WK
       01  CQSRVB-ZTECH.                                                CQOMC0WK
           05  CQSRVB-MODULE               PIC X(15).                   CQOMC0WK
           05  CQSRVB-NBPAR                PIC X.                       CQOMC0WK
      *    05  CQSRVB-LENGTH               COMP-6.                      CQOMC0WK
           05  CQSRVB-LENGTH               PIC S9(00009) COMP-5 SYNC.   CQOMC0WK
      *    05  CQSRVB-LENGTH2              COMP-6.                      CQOMC0WK
           05  CQSRVB-LENGTH2              PIC S9(00009) COMP-5 SYNC.   CQOMC0WK
      *                                                                 CQOMC0WK
      *    AJOUT DE ZONES POUR APPEL DE MODULES A TRAVERS CLIMB SDP     CQOMC0WK
      *                                                                 CQOMC0WK
       01  SQSRVB-ZTECH.                                                CQOMC0WK
           05  SQSRVB-MODULE               PIC X(15).                   CQOMC0WK
           05  SQSRVB-NBPAR                PIC X.                       CQOMC0WK
      *    05  SQSRVB-LENGTH               COMP-6.                      CQOMC0WK
           05  SQSRVB-LENGTH               PIC S9(00009) COMP-5 SYNC.   CQOMC0WK
      *    05  SQSRVB-LENGTH2              COMP-6.                      CQOMC0WK
           05  SQSRVB-LENGTH2              PIC S9(00009) COMP-5 SYNC.   CQOMC0WK
      ******************************************************************CQOMC0WK
      ****************************************************************  CQOMC0WK
      *      VALEUR INTERNE DU SIGNE EURO                               CQOMC0WK
      *             IBM  ==>  9F HEXA                                   CQOMC0WK
      *             BULL ==>  A4 HEXA                                   CQOMC0WK
      ****************************************************************  CQOMC0WK
      *    IBM =======                                                  CQOMC0WK
      *01  LT-CAR-EURO        PIC X  VALUE X'9F'.                       CQOMC0WK
      *    BULL=======                                                  CQOMC0WK
      *01  LT-CAR-EURO        PIC X  VALUE ""165"".                     CQOMC0WK
       01  LT-CAR-EURO        PIC X  VALUE X"A4"  .                     CQOMC0WK
      *                                                                 CQOMC0WK
      *END_OF_COPY_FILE                                                 CQOMC0WK
                                                                        
      *COPY WSHWZ001.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/WSHWZ001.CPY"   WSHWZ001
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WSHWZ001
      *LibFac  liberFactory GCOS to Linux Translator  *                 WSHWZ001
      *LibFac  From input file: WSHWZ001              *                 WSHWZ001
      *LibFac  On   the   date: 2015/03/24            *                 WSHWZ001
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           WSHWZ001
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WSHWZ001
      ***************************************************************** WSHWZ001
      *                 DB DEBUG PROGRAM AREA                         * WSHWZ001
      *                                                               * WSHWZ001
      *    THIS AREA IS USED TO KEEP TRACK OF ALL THE  PARAGRAPHS     * WSHWZ001
      *    THAT WERE PERFORMED DURING  A  TEST  EXECUTION.   EACH     * WSHWZ001
      *    PARAGRAPH  MOVES ITS PARAGRAPH NAME   INTO  THE  TRACE     * WSHWZ001
      *    PARAGRAPH TABLE.  THIS TABLE CAN BE DISPLAYED BY           * WSHWZ001
      *    THE PROGRAMMER USING THE INSTALL1 DEBUGGER      .          * WSHWZ001
      *    THIS ASSISTS IN  DEBUGGING  BY  ALLOWING  THE              * WSHWZ001
      *    PROGRAMMER TO FOLLOW THE LOGICAL PATH EXECUTED  DURING     * WSHWZ001
      *    A TEST.  A DESCRIPTION OF EACH OF THE FIELDS  IN  THIS     * WSHWZ001
      *    AREA FOLLOWS.                                              * WSHWZ001
      *                                                               * WSHWZ001
      *            1)  TAG FIELDS : GIVES  A  TITLE  TO  EACH  OF     * WSHWZ001
      *                THE DEBUG FIELDS.   THESE  TITLES  CAN  BE     * WSHWZ001
      *                VIEWED ON-LINE.                                * WSHWZ001
      *                                                               * WSHWZ001
      *            2)  DB-LAST-PARAGRAPH-EXECUTED :    THE   LAST     * WSHWZ001
      *                PARAGRAPH EXECUTED BEFORE PROGRAM TERMINA-     * WSHWZ001
      *                TION.                                          * WSHWZ001
      *                                                               * WSHWZ001
      *            3)  DB-TRACE-INDEX : INDEX FOR THE TABLE.          * WSHWZ001
      *                                                               * WSHWZ001
      *            4)  DB-RESET-COUNT : NUMBER OF TIMES THE TABLE     * WSHWZ001
      *                HAS BEEN FILLED AND RESET.                     * WSHWZ001
      *                                                               * WSHWZ001
      *            5)  DB-TRACE-PARAGRAPH-TABLE :  CONTAINS   THE     * WSHWZ001
      *                PARAGRAPH NUMBERS FOR EACH PARAGRAPH  THAT     * WSHWZ001
      *                IS EXECUTED.  THIS ALLOWS  THE  PROGRAMMER     * WSHWZ001
      *                TO VIEW THE ORDER IN WHICH THE  PARAGRAPHS     * WSHWZ001
      *                WERE EXECUTED.                                 * WSHWZ001
      *                                                               * WSHWZ001
      *    NOTE: THIS PROGRAM DEBUG FEATURE IS ACTIVE  ONLY  WHEN     * WSHWZ001
      *    THE PROGRAM IS IN DEBUG MODE.                              * WSHWZ001
      *                                                               * WSHWZ001
      ***************************************************************** WSHWZ001
      /                                                                 WSHWZ001
      D01  TAG-DEBUG-PROGRAM-AREA                PIC X(24) VALUE        WSHWZ001
      D         "DEBUG PROGRAM AREA".                                   WSHWZ001
      D                                                                 WSHWZ001
      D01  DB-DEBUG-PROGRAM-AREA.                                       WSHWZ001
      D  03 TAG-DEBUG-LAST-PARAGRAPH             PIC X(20) VALUE        WSHWZ001
      D            "DEBUG LAST PARAGRAPH".                              WSHWZ001
      D  03 DB-LAST-PARAGRAPH-EXECUTED           PIC X(30) VALUE SPACE. WSHWZ001
      D  03 TAG-DEBUG-TRACE-INDEX                PIC X(16) VALUE        WSHWZ001
      D            "DEBUG TRACE INDX".                                  WSHWZ001
      D  03 DB-TRACE-INDEX                       PIC S9(4) COMP-5 SYNC  WSHWZ001
      D                                                    VALUE +1.    WSHWZ001
      D  03 DB-TRACE-INCREMENT                   PIC S9(4) COMP-5 SYNC  WSHWZ001
      D                                                    VALUE +4.    WSHWZ001
      D  03 DB-TRACE-POSITION                    PIC S9(4) COMP-5 SYNC  WSHWZ001
      D                                                    VALUE +1.    WSHWZ001
      D  03 DB-TRACE-LINE-LENGTH                 PIC S9(4) COMP-5 SYNC  WSHWZ001
      D                                                    VALUE +80.   WSHWZ001
      D  03 TAG-RESET-COUNT                      PIC X(16) VALUE        WSHWZ001
      D            "DEBUG RESET CNT ".                                  WSHWZ001
      D  03 DB-BINARY-ONE                        PIC S9(4) COMP-5 SYNC  WSHWZ001
                                                                        WSHWZ001
      D                                                    VALUE +1.    WSHWZ001
      D  03 DB-BINARY-ZERO                       PIC S9(4) COMP-5 SYNC  WSHWZ001
      D                                                    VALUE +0.    WSHWZ001
                                                                        WSHWZ001
      D  03 DB-RESET-COUNT                       PIC S9(4) COMP-5 SYNC  WSHWZ001
      D                                                    VALUE +0.    WSHWZ001
      D  03 TAG-RESET-COUNT                      PIC X(16) VALUE        WSHWZ001
      D            "MAX TABLE ENTRY ".                                  WSHWZ001
      D  03 DB-MAX-TRACE-TABLE-ENTRIES           PIC S9(4) COMP-5 SYNC  WSHWZ001
      D                                               VALUE +500.       WSHWZ001
      D                                                                 WSHWZ001
      ***************************************************************** WSHWZ001
      *                                                               * WSHWZ001
      *            TRACE PARAGRAPH TABLE                              * WSHWZ001
      *           -----------------------                             * WSHWZ001
      *                                                               * WSHWZ001
      ***************************************************************** WSHWZ001
      D01  TAG-TRACE-PARAGRAPH-TABLE             PIC X(20) VALUE        WSHWZ001
      D        "TRACE PARAGRAPH TAB ".                                  WSHWZ001
      *                                                                 WSHWZ001
      D01  DB-TRACE-PARAGRAPH-TABLE.                                    WSHWZ001
      D  03 FILLER                               PIC X(15000) VALUE     WSHWZ001
      D                                                  LOW-VALUES.    WSHWZ001
      D01  DB-TRACE-PARA-TAB-RED REDEFINES DB-TRACE-PARAGRAPH-TABLE.    WSHWZ001
      D  03 DB-TRACE-PARA-TAB-OCC                OCCURS 500.            WSHWZ001
      D    05 DB-TRACE-PARAGRAPH-ENTRY           PIC X(30).             WSHWZ001
      D01  DB-TRACE-LINE                         PIC X(132).            WSHWZ001
                                                                        WSHWZ001
      *END_OF_COPY_FILE                                                 WSHWZ001
      *COPY WSHWZ003.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/WSHWZ003.CPY"   WSHWZ003
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WSHWZ003
      *LibFac  liberFactory GCOS to Linux Translator  *                 WSHWZ003
      *LibFac  From input file: WSHWZ003              *                 WSHWZ003
      *LibFac  On   the   date: 2015/03/24            *                 WSHWZ003
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           WSHWZ003
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WSHWZ003
      ***************************************************************** WSHWZ003
      *                                                               * WSHWZ003
      *   A P P L I C A T I O N   P R O C E S S I N G   S T A T U S   * WSHWZ003
      *   ----------------------------------------------------------  * WSHWZ003
      *                                                               * WSHWZ003
      *  THE APPLICATION-PROC-STATUS-FIELD IS USED TO DETERMINE THE   * WSHWZ003
      *  STATUS OF THE CURRENT EXECUTION OF THE APPLICATION.          * WSHWZ003
      *                                                               * WSHWZ003
      * MODIF LE 29/04/97                                             * WSHWZ003
      ***************************************************************** WSHWZ003
                                                                        WSHWZ003
       01  AS-APPL-PROC-STATUS-AREA.                                    WSHWZ003
      *  03 AS-APPL-PROC-STATUS                  PIC S9(4) COMP-1.      WSHWZ003
      *  03 AS-APPL-PROC-STATUS                  COMP-6.                WSHWZ003
         03 AS-APPL-PROC-STATUS                                         WSHWZ003
       PIC S9(00009) COMP-5 SYNC                       .                WSHWZ003
           88 AS-APPL-PROC-OK                            VALUE +0000.   WSHWZ003
           88 AS-APPL-PROC-ERROR                         VALUE +0001    WSHWZ003
                                                               +0002    WSHWZ003
                                                               +0003.   WSHWZ003
           88 AS-APPL-PROC-FLD-ERROR                     VALUE +0001.   WSHWZ003
           88 AS-APPL-PROC-REL-ERROR                     VALUE +0002.   WSHWZ003
           88 AS-APPL-PROC-INF-ERROR                     VALUE +0003.   WSHWZ003
                                                                        WSHWZ003
      *END_OF_COPY_FILE                                                 WSHWZ003
      *COPY WSHWZ007.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/WSHWZ007.CPY"   WSHWZ007
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WSHWZ007
      *LibFac  liberFactory GCOS to Linux Translator  *                 WSHWZ007
      *LibFac  From input file: WSHWZ007              *                 WSHWZ007
      *LibFac  On   the   date: 2015/03/24            *                 WSHWZ007
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           WSHWZ007
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WSHWZ007
      ***************************************************************** WSHWZ007
      *                                                               * WSHWZ007
      *            C I C S - E R R O R - A N A L Y S I S              * WSHWZ007
      *          -----------------------------------------            * WSHWZ007
      *                                                               * WSHWZ007
      *  ANALYSIS AREA FOR THE EIBRCODE RETURNED FROM A  TP  COMMAND. * WSHWZ007
      *  THE FIRST BYTE OF THE EIBRCODE CONTAINS THE NUMERICAL VALUE  * WSHWZ007
      *  WHICH INDICATES THE EXECUTION STATUS OF THE COMMAND.         * WSHWZ007
      *                                                               * WSHWZ007
      ***************************************************************** WSHWZ007
       01  CEA-TP-ERROR-ANALYSIS.                                       WSHWZ007
         03 CEA-WORK-AREA.                                              WSHWZ007
           05 CEA-FILLER                         PIC X(1)               WSHWZ007
                                                   VALUE LOW-VALUES.    WSHWZ007
           05 CEA-EIBRCODE                       PIC X(6).              WSHWZ007
         03 CEA-ANALYZE-AREA                   REDEFINES CEA-WORK-AREA. WSHWZ007
           05 CEA-TP-RETURN-CODE               PIC S9(4) COMP-1.        WSHWZ007
      *    05 CEA-TP-RETURN-CODE                         BINARY-SHORT.  WSHWZ007
              88 CEA-TP-RETURN-NORMAL            VALUE +0.              WSHWZ007
              88 CEA-TP-RETURN-QZERO             VALUE +1.              WSHWZ007
              88 CEA-TP-RETURN-ITEMERR           VALUE +1.              WSHWZ007
              88 CEA-TP-RETURN-DSIDERR           VALUE +1.              WSHWZ007
              88 CEA-TP-RETURN-PGMIDERR          VALUE +1.              WSHWZ007
              88 CEA-TP-RETURN-INVREQ            VALUE +1 +2 +8 +32     WSHWZ007
                                                         +224 +255.     WSHWZ007
              88 CEA-TP-RETURN-QIDERR            VALUE +2.              WSHWZ007
              88 CEA-TP-RETURN-ILLOGIC           VALUE +2.              WSHWZ007
              88 CEA-TP-RETURN-MAPFAIL           VALUE +4.              WSHWZ007
              88 CEA-TP-RETURN-EOF               VALUE +4 +193.         WSHWZ007
              88 CEA-TP-RETURN-IOERR             VALUE +4 +7 +128.      WSHWZ007
              88 CEA-TP-RETURN-NOTOPEN           VALUE +5 +8 +12.       WSHWZ007
              88 CEA-TP-RETURN-LENGERR           VALUE +6 +225.         WSHWZ007
              88 CEA-TP-RETURN-INVMPSZ           VALUE +8.              WSHWZ007
              88 CEA-TP-RETURN-NOSPACE           VALUE +8 +16 +131.     WSHWZ007
              88 CEA-TP-RETURN-ENDFILE           VALUE +15.             WSHWZ007
              88 CEA-TP-RETURN-TRANSIDERR        VALUE +17.             WSHWZ007
              88 CEA-TP-RETURN-TERMIDERR         VALUE +18 +230.        WSHWZ007
              88 CEA-TP-RETURN-ENQBUSY           VALUE +32.             WSHWZ007
              88 CEA-TP-RETURN-NOTFND            VALUE +129.            WSHWZ007
              88 CEA-TP-RETURN-DUPREC            VALUE +130.            WSHWZ007
              88 CEA-TP-RETURN-DUPKEY            VALUE +132.            WSHWZ007
              88 CEA-TP-RETURN-QBUSY             VALUE +192.            WSHWZ007
              88 CEA-TP-RETURN-SYSIDERR          VALUE +208.            WSHWZ007
              88 CEA-TP-RETURN-ISCINVREQ         VALUE +209.            WSHWZ007
              88 CEA-TP-RETURN-NOTAUTH           VALUE +214.            WSHWZ007
                                                                        WSHWZ007
      *END_OF_COPY_FILE                                                 WSHWZ007
                                                                        
      *COPY HMEML020.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/HMEML020.CPY"   HMEML020
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 HMEML020
      *LibFac  liberFactory GCOS to Linux Translator  *                 HMEML020
      *LibFac  From input file: HMEML020              *                 HMEML020
      *LibFac  On   the   date: 2015/03/24            *                 HMEML020
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           HMEML020
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 HMEML020
      ***************************************************************   HMEML020
      *   COPYBOOK : HMEML020                                           HMEML020
      *   DATE CREATED : 23/01/95                                       HMEML020
      *   DESCRIPTION : LINKAGE COPYBOOK FOR HMEMC010.                  HMEML020
      *                 THIS COPYBOOK SHOULD BE USED TO CALL THE        HMEML020
      *                 SHARED MEMORY MANAGER MODULE HMEMC010.          HMEML020
      * ------------------------------------------------------------    HMEML020
      * MODIF  15/05/97 - AJOUT W-CQFNDMEM                              HMEML020
      * MODIF  24/04/07 - BUGS DJ275 - AJOUT ZONES POUR CALL AVEC LE    HMEML020
      *                   BON NOMBRE DE PARAMETRES                      HMEML020
      ***************************************************************   HMEML020
       01 HMEM-FUNCTION-CODE                  PIC X(2) VALUE "RI".      HMEML020
         88 HMEM-FLAG-RETRIEVE-IN-CORE     VALUE "RI".                  HMEML020
      *01 HMEM-RETURN-CODE                    COMP-6.                   HMEML020
       01 HMEM-RETURN-CODE                    PIC S9(00009) COMP-5 SYNC.HMEML020
       01 HMEM-SEGMENT-SIZES.                                           HMEML020
      *  03 HMEM-INDEX-SEGMENT-SIZE           COMP-6.                   HMEML020
         03 HMEM-INDEX-SEGMENT-SIZE           PIC S9(00009) COMP-5 SYNC.HMEML020
      *  03 HMEM-CTB-INDEX-SEGMENT-SIZE       COMP-6.                   HMEML020
         03 HMEM-CTB-INDEX-SEGMENT-SIZE       PIC S9(00009) COMP-5 SYNC.HMEML020
      *  03 HMEM-DATA-SEGMENT-SIZE            COMP-6.                   HMEML020
         03 HMEM-DATA-SEGMENT-SIZE            PIC S9(00009) COMP-5 SYNC.HMEML020
       01 HMEM-RECORD-TYPE                    PIC X(3).                 HMEML020
       01 HMEM-KEY                            PIC X(9).                 HMEML020
       01 HMEM-RECORD-POINTER                 USAGE IS POINTER.         HMEML020
      *01 HMEM-RECORD-LENGTH                  COMP-6.                   HMEML020
       01 HMEM-RECORD-LENGTH                  PIC S9(00009) COMP-5 SYNC.HMEML020
       01 HMEM-CTB-KEY.                                                 HMEML020
         03 HMEM-CTB-USER-KEY.                                          HMEML020
           05 HMEM-CTB-USER-KEY-ID            PIC X(17).                HMEML020
           05 HMEM-CTB-USER-KEY-EFF-DATE      PIC S9(7) COMP-2.         HMEML020
      *    05 HMEM-CTB-USER-KEY-EFF-DATE                BINARY-LONG.    HMEML020
         03 HMEM-CTB-EFF-DATE-FLAG            PIC X(1).                 HMEML020
           88 HMEM-CTB-EFF-DATE-USED       VALUE "Y".                   HMEML020
           88 HMEM-CTB-EFF-DATE-NOT-USED   VALUE "N".                   HMEML020
      *  03 HMEM-CTB-NUM-REC-REQUESTED        COMP-6.                   HMEML020
         03 HMEM-CTB-NUM-REC-REQUESTED        PIC S9(00009) COMP-5 SYNC.HMEML020
      *  03 HMEM-CTB-NUM-REC-RETURNED         COMP-6.                   HMEML020
         03 HMEM-CTB-NUM-REC-RETURNED         PIC S9(00009) COMP-5 SYNC.HMEML020
      *01 W-CQFNDMEM                          COMP-6.                   HMEML020
       01 W-CQFNDMEM                          PIC S9(00009) COMP-5 SYNC.HMEML020
                                                                        HMEML020
                                                                        HMEML020
      *END_OF_COPY_FILE                                                 HMEML020
                                                                        
      *COPY CTIDBAT.                                                    
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copyb/CTIDBAT.CPY"   CTIDBAT
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CTIDBAT
      *LibFac  liberFactory GCOS to Linux Translator  *                 CTIDBAT
      *LibFac  From input file: CTIDBAT               *                 CTIDBAT
      *LibFac  On   the   date: 2015/03/23            *                 CTIDBAT
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           CTIDBAT
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CTIDBAT
      ***************************************************************   CTIDBAT
      *                                                             *   CTIDBAT
      *   ZONE DE COMMUNICATION TECHNIQUE ARCHITECTURE INTERNE      *   CTIDBAT
      *                    C R I S T A L                            *   CTIDBAT
      *      ****   VERSION ACTIVE EN BATCH (WORKING)   ****        *   CTIDBAT
      *                                                             *   CTIDBAT
      ***************************************************************   CTIDBAT
       01  CTIC-CTIDCOM.                                                CTIDBAT
           03  CTIC-LGIGAD                          PIC 9(09).          CTIDBAT
           03  CTIC-IDORGA                          PIC X(05).          CTIDBAT
           03  CTIC-PTANCR                          PIC X(01).          CTIDBAT
           03  CTIC-FLGDSS                          PIC X(01).          CTIDBAT
           03  CTIC-EPAIGU                          PIC X(08).          CTIDBAT
           03  CTIC-IDENT-DATABASE.                                     CTIDBAT
              05 CTIC-RFM-MODEL.                                        CTIDBAT
                 07  CTIC-MORFMB                    PIC X(32).          CTIDBAT
                 07  CTIC-MORFMO                    PIC X(32).          CTIDBAT
                 07  CTIC-FILACC                    PIC X(03).          CTIDBAT
              05 CTIC-DBSP-IDENT.                                       CTIDBAT
                 07 CTIC-DBSP-TBL OCCURS 10.                            CTIDBAT
                    09  CTIC-DBSP-TBL-DBSS          PIC X(12).          CTIDBAT
                    09  CTIC-DBSP-TBL-APPGRP        PIC X(32).          CTIDBAT
           03  FILLER                               PIC X(29).          CTIDBAT
      *END_OF_COPY_FILE                                                 CTIDBAT
      *COPY CQLIBERW.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/CQLIBERW.CPY"   CQLIBERW
      ***************************************************************** CQLIBERW
      * COPY CQLIBERW                                                 * CQLIBERW
      *     VARIABLES TECHNIQUES LIEES A L'ARCHITECTURE LIBER-TP      * CQLIBERW
      *     OU LIBER-BATCH                                            * CQLIBERW
      ***************************************************************** CQLIBERW
                                                                        CQLIBERW
      ***************************************************************** CQLIBERW
      *     DONNEES POUR LA RECUPERATION DES ABORTS 'ILLEGAL EIS'     * CQLIBERW
      *     UTILISEES PAR LE SQUELETTE WSHBBDP2                       * CQLIBERW
      ***************************************************************** CQLIBERW
       01  CQIPR-FLAG          PIC S9(9) COMP-5 SYNC.                   CQLIBERW
       01  CQIPR-IC            PIC X(6).                                CQLIBERW
       01  CQIPR-SEGID         PIC X(4).                                CQLIBERW
                                                                        CQLIBERW
      ***************************************************************** CQLIBERW
      *     DONNEES POUR LES TESTS DE SWITCH-WORD                     * CQLIBERW
      ***************************************************************** CQLIBERW
       77 LTP-SWITCH-TEST-RESULT       PIC S9(11) BINARY.               CQLIBERW
      *END_OF_COPY_FILE                                                 CQLIBERW
      *COPY VARG-DATA.                                                  
      *COPY"/distrib/libertp/version_en_cours/COBCPY/TP8/VARG-DATA.CPY" 
       01  VARG-LENGTH-BLOCK.                                           
           03  VARG-NUM-ARGS   PIC 99 VALUE 0.                          
           03  VARG-CBL-ARG    PIC 9(8) OCCURS 0 TO 32 TIMES            
                               DEPENDING ON VARG-NUM-ARGS.              
      *END_OF_COPY_FILE                                                 
                                                                        
      *COPY DFHEIBLK.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/DFHEIBLK.CPY"   DFHEIBLK
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 DFHEIBLK
      *LibFac  liberFactory GCOS to Linux Translator  *                 DFHEIBLK
      *LibFac  From input file: DFHEIBLK              *                 DFHEIBLK
      *LibFac  On   the   date: 2015/03/24            *                 DFHEIBLK
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           DFHEIBLK
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 DFHEIBLK
      *      COPYBOOK-NAME = DFHEIBLK                                   DFHEIBLK
       01  DFHEIBLK.                                                    DFHEIBLK
      ***************************************************************** DFHEIBLK
      * RECORD DFHEIBLK                       GENERATION DATE 13FEB89   DFHEIBLK
      *                                                                 DFHEIBLK
      ***************************************************************** DFHEIBLK
      * 08/21/91 * CR1865 * DETECTION OF VIP7700 TERMINAL (SEE CR1069)* DFHEIBLK
      *          *        *                                           * DFHEIBLK
      *          *        *                                           * DFHEIBLK
      ***************************************************************** DFHEIBLK
           05  EIBTIME                     PIC S9(7)     COMP-3.        DFHEIBLK
           05  EIBDATE                     PIC S9(7)     COMP-3.        DFHEIBLK
           05  EIBTRNID                    PIC X(4).                    DFHEIBLK
           05  EIBTRMID                    PIC X(12).                   DFHEIBLK
           05  EIBRCODE                    PIC X(6).                    DFHEIBLK
           05  EIBAID                      PIC X.                       DFHEIBLK
           05  EIBCPOSN                    PIC S9(4) COMP-1.            DFHEIBLK
      *    05  EIBCPOSN                              BINARY-SHORT.      DFHEIBLK
           05  EIBFN                       PIC X(2).                    DFHEIBLK
           05  EIBCALEN                    PIC S9(4) COMP-1.            DFHEIBLK
      *    05  EIBCALEN                              BINARY-SHORT.      DFHEIBLK
           05  EIBTASKN                    PIC S9(7)     COMP-3.        DFHEIBLK
           05  EIBTTYPE                    PIC X(2).                    DFHEIBLK
               88  EIB-DKU7107                         VALUE  "K7".     DFHEIBLK
               88  EIB-DKU7105                         VALUE  "K5".     DFHEIBLK
               88  EIB-VIP7804                         VALUE  "V4".     DFHEIBLK
               88  EIB-IBM3270                         VALUE  "I7".     DFHEIBLK
               88  EIB-VIP7700                         VALUE  "V7".     DFHEIBLK
                                                                        DFHEIBLK
      *END_OF_COPY_FILE                                                 DFHEIBLK
                                                                        
      *$$DATA-DEFINE                                                    
      ***************************************************************** 
      *                                                               * 
      *                 APPLICATION WORKING STORAGE                   * 
      *                -*-*-*-*-*-*-*-*-*-*-*-*-*-*-                  * 
      *                                                               * 
      *    THIS IS THE APPLICATION SPECIFIC WORKING STORAGE AREA.     * 
      *    THE APPLICATION PROGRAMMER SHOULD ADD  ALL APPLICATION     * 
      *    SPECIFIC WORKING STORAGE FIELDS AFTER  THIS  POINT  IN     * 
      *    THE PROGRAM.  NOTE THAT THERE ARE  SOME  FIELD  GROUPS     * 
      *    ALREADY INCLUDED IN THIS AREA.  THESE FIELD GROUPS ARE     * 
      *    COMMONLY FOUND IN ON-LINE APPLICATIONS  AND  SERVE  AS     * 
      *    STARTING POINTS FOR THE APPLICATION  SPECIFIC  WORKING     * 
      *    STORAGE.                                                   * 
      *                                                               * 
      ***************************************************************** 
      ***************************************************************** 
      *                                                               * 
      *   THE VALUES OF SOME OF THESE LITERALS MUST BE CHANGED. THE   * 
      *   VARIABLES TO BE CHANGED ARE:                                * 
      *                                                               * 
      *     LT-NAME-OF-THIS-PROGRAM - CHANGE ALL 'CGS100' VALUES      * 
      *                     TO THE NAME OF THE APPLICATION PROGRAM    * 
      *                                                               * 
      *                                                               * 
      ***************************************************************** 
      *    COPY WSHWZ009.                                               
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/WSHWZ009.CPY"   WSHWZ009
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WSHWZ009
      *LibFac  liberFactory GCOS to Linux Translator  *                 WSHWZ009
      *LibFac  From input file: WSHWZ009              *                 WSHWZ009
      *LibFac  On   the   date: 2015/03/24            *                 WSHWZ009
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           WSHWZ009
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WSHWZ009
      ***************************************************************** WSHWZ009
      *                                                               * WSHWZ009
      *                 L I T E R A L S                               * WSHWZ009
      *              ----------------------                           * WSHWZ009
      *                                                               * WSHWZ009
      *    THIS AREA DEFINES ALL THE LITERALS AND  CONSTANTS USED     * WSHWZ009
      *    BY THIS PROGRAM.  THOSE THAT ARE ALREADY  DEFINED  ARE     * WSHWZ009
      *    NEEDED BY THE MES SHELL PROGRAM.  INCLUDE THE LITERALS     * WSHWZ009
      *    THAT ARE USED BY THE APPLICATION.                          * WSHWZ009
      *                                                               * WSHWZ009
      ***************************************************************** WSHWZ009
                                                                        WSHWZ009
       01  LITERALS.                                                    WSHWZ009
                                                                        WSHWZ009
         03 LT-NO-ERROR-OCCURRED                 PIC S9(4) COMP-1       WSHWZ009
      *  03 LT-NO-ERROR-OCCURRED                           BINARY-SHORT WSHWZ009
                                                                        WSHWZ009
                                                   VALUE +0000.         WSHWZ009
         03 LT-FLD-ERROR-OCCURRED                PIC S9(4) COMP-1       WSHWZ009
      *  03 LT-FLD-ERROR-OCCURRED                          BINARY-SHORT WSHWZ009
                                                                        WSHWZ009
                                                   VALUE +0001.         WSHWZ009
         03 LT-REL-ERROR-OCCURRED                PIC S9(4) COMP-1       WSHWZ009
      *  03 LT-REL-ERROR-OCCURRED                          BINARY-SHORT WSHWZ009
                                                                        WSHWZ009
                                                   VALUE +0002.         WSHWZ009
         03 LT-APPL-PROCESSING-ERROR             PIC S9(4) COMP-1       WSHWZ009
      *  03 LT-APPL-PROCESSING-ERROR                       BINARY-SHORT WSHWZ009
                                                                        WSHWZ009
                                                   VALUE +0001.         WSHWZ009
         03 LT-INFORMATION-ERROR-OCCURRED        PIC S9(4) COMP-1       WSHWZ009
      *  03 LT-INFORMATION-ERROR-OCCURRED                  BINARY-SHORT WSHWZ009
                                                                        WSHWZ009
                                                   VALUE +0003.         WSHWZ009
         03 LT-APPL-REQUIRES-ABEND               PIC X(1) VALUE "1".    WSHWZ009
                                                                        WSHWZ009
      *END_OF_COPY_FILE                                                 WSHWZ009
         03 LT-NAME-OF-THIS-PROGRAM              PIC X(8)               
                                                   VALUE 'CGS100'.      
         03 LT-ID-GDD-ZZZ                        PIC X(3)               
                                                   VALUE 'ZZZ'.         
         03 LT-STATUT-9999                       PIC S9(4)              
                                                   VALUE 9999.          
         03 LT-TABEND-SGBD                       PIC X(1)               
                                                   VALUE 'D'.           
      *    COPY WCSERIGA.                                               
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/WCSERIGA.CPY"   WCSERIGA
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WCSERIGA
      *LibFac  liberFactory GCOS to Linux Translator  *                 WCSERIGA
      *LibFac  From input file: WCSERIGA              *                 WCSERIGA
      *LibFac  On   the   date: 2015/03/24            *                 WCSERIGA
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           WCSERIGA
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 WCSERIGA
      ******************************************************************WCSERIGA
      **                                                                WCSERIGA
      **   LITERAUX APPLICATIFS DES DIFFERENTES VALEURS DU              WCSERIGA
      **   DES CODES RETOUR DE L INTERFACE GENERALISEE                  WCSERIGA
      **                                                                WCSERIGA
      ******************************************************************WCSERIGA
      **                                                                WCSERIGA
      **  CETTE CLAUSE COPY SERA A COMPLETER EN FONCTION DES EVOLUTIONS WCSERIGA
      **  DE LA CELLULE DE DONNEES                                      WCSERIGA
      **                                                                WCSERIGA
      ******************************************************************WCSERIGA
                                                                        WCSERIGA
           03 LT-NAME-OF-IG                 PIC X(08) VALUE 'CDZZ0   '. WCSERIGA
                                                                        WCSERIGA
           03 LT-CRG-OK                     PIC X(2) VALUE '00'.        WCSERIGA
           03 LT-CRG-SQLCODE                PIC X(2) VALUE '99'.        WCSERIGA
                                                                        WCSERIGA
                                                                        WCSERIGA
           03 LT-CRD-OK                     PIC S9(04) VALUE  00   .    WCSERIGA
           03 LT-CRD-FONC-INCONNUE          PIC S9(04) VALUE  10   .    WCSERIGA
           03 LT-CRD-IDEN-INCONNUE          PIC S9(04) VALUE  11   .    WCSERIGA
           03 LT-CRD-GDD-INVAL              PIC S9(04) VALUE  12   .    WCSERIGA
           03 LT-CRD-NB-OCC-INVAL           PIC S9(04) VALUE  13   .    WCSERIGA
           03 LT-CRD-CODE-LEC-INVAL         PIC S9(04) VALUE  14   .    WCSERIGA
           03 LT-CRD-DSVCM-INVAL            PIC S9(04) VALUE  15   .    WCSERIGA
           03 LT-CRD-TAPPEL-INVAL           PIC S9(04) VALUE  16   .    WCSERIGA
           03 LT-CRD-DATE-INVAL             PIC S9(04) VALUE  17   .    WCSERIGA
           03 LT-CRD-APPEL-NON-AUT          PIC S9(04) VALUE  18   .    WCSERIGA
           03 LT-CRD-ERREUR-IGA             PIC S9(04) VALUE  19   .    WCSERIGA
           03 LT-CRD-ID-APPEL-INVAL         PIC S9(04) VALUE  20   .    WCSERIGA
                                                                        WCSERIGA
           03 LT-CRD-NON-TROUVE             PIC S9(04) VALUE  30   .    WCSERIGA
           03 LT-CRD-CREATION-IMPOSSIBLE    PIC S9(04) VALUE  31   .    WCSERIGA
           03 LT-CRD-SUPP-IMPOSSIBLE        PIC S9(04) VALUE  32   .    WCSERIGA
           03 LT-CRD-MODIF-IMPOSSIBLE       PIC S9(04) VALUE  33   .    WCSERIGA
           03 LT-CRD-CODE-ETAT-ERRONE       PIC S9(04) VALUE  34   .    WCSERIGA
           03 LT-CRD-DSM-EXISTE             PIC S9(04) VALUE  35   .    WCSERIGA
           03 LT-CRD-DSC-EXISTE             PIC S9(04) VALUE  36   .    WCSERIGA
           03 LT-CRD-DSV-EXISTE             PIC S9(04) VALUE  37   .    WCSERIGA
           03 LT-CRD-CLE-INEXISTANTE        PIC S9(04) VALUE  38   .    WCSERIGA
           03 LT-CRD-DSM-EFFACE             PIC S9(04) VALUE  39   .    WCSERIGA
           03 LT-CRD-DSC-EFFACE             PIC S9(04) VALUE  40   .    WCSERIGA
                                                                        WCSERIGA
                                                                        WCSERIGA
      *END_OF_COPY_FILE                                                 WCSERIGA
      ***************************************************************** 
      *    CUSTOMISATION PROJET CRISTAL DE LA WORKING STORAGE         * 
      *                  SOUS FORME DE CLAUSE COPY APPLICATIVE DE     * 
      *                  WORKING                                      * 
      *                                                               * 
      *    LA CLAUSE COPY WCSERIGA CONTIENT TOUTES LES VALEURS        * 
      *    DES CODES RETOUR DE L'INTERFACE GENERALISEE                * 
      *                                                               * 
      *    ATTENTION : CETTE WORKING EST DECOMPOSEE EN DEUX PARTIES : * 
      *          - LITERAUX                                           * 
      *          - VARIABLES                                          * 
      *                                                               * 
      ***************************************************************** 
      *    COPY CGS100W.                                                
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/CGS100W.CPY"    CGS100W
      ***************************************************************** CGS100W
      *                                                               * CGS100W
      *  CGS100W  Attention le vrai pilote est le CGS101 !!!!!!!!!!!  * CGS100W
      *  -----------------------------------------------------------  * CGS100W
      *                                                               * CGS100W
      *                                                               * CGS100W
      ***************************************************************** CGS100W
                                                                        CGS100W
      ****************************************************************  CGS100W
      *   LITERAUX SPECIFIQUES AU PROGRAMME                          *  CGS100W
      ****************************************************************  CGS100W
                                                                        CGS100W
       01 WS-VARIABLES-SPECIFIQUES.                                     CGS100W
                                                                        CGS100W
          05 WS-DD-PMVT-ORI                  PIC S9(9) COMP-3.          CGS100W
          05 WS-DF-PMVT-ORI                  PIC S9(9) COMP-3.          CGS100W
          05 WS-DD-PMVT-1                    PIC S9(9) COMP-3.          CGS100W
          05 WS-DF-PMVT-1                    PIC S9(9) COMP-3.          CGS100W
          05 WS-DD-PMVT-2                    PIC S9(9) COMP-3.          CGS100W
          05 WS-DF-PMVT-2                    PIC S9(9) COMP-3.          CGS100W
                                                                        CGS100W
      *-- Zone de working pour travail sur dates                        CGS100W
          05 WS-RG-DATES.                                               CGS100W
             10 WS-TABLE-DT.                                            CGS100W
                15 WS-TAB-DATES OCCURS 10                               CGS100W
                INDEXED BY IX-DATE IX-DT2.                              CGS100W
                   20 WS-DATE          PIC 9(8).                        CGS100W
                   20 WS-R-DATE REDEFINES WS-DATE.                      CGS100W
                      25 WS-DTMOIS     PIC 9(6).                        CGS100W
                      25 WS-R-DTMOIS REDEFINES WS-DTMOIS.               CGS100W
                         30 WS-DTAN    PIC 9(4).                        CGS100W
                         30 WS-DTMM    PIC 99.                          CGS100W
                      25 WS-DTJJ       PIC 9(2).                        CGS100W
                   20 WS-R2-DATE REDEFINES WS-DATE.                     CGS100W
                      25 FILLER        PIC 9(4).                        CGS100W
                      25 WS-DT-JMO     PIC 9(4).                        CGS100W
      ****************************************************************  CGS100W
      * INTERFACE avec l'IGAD                                        *  CGS100W
      ****************************************************************  CGS100W
          04 WS-IG-INTERFACE-SV                  PIC X(9710).           CGS100W
          04 WS-IG-INTERFACE.                                           CGS100W
             05 WS-IG-INTERFACE2.                                       CGS100W
                06 WS-IG-RACINE.                                        CGS100W
                   10 WS-CODE-ENTITE-LOGIQUE   PIC X(3).                CGS100W
                   10 WS-CODE-FONCTION         PIC X(3).                CGS100W
                     88 FL-ZFONCT-D0C                 VALUE 'D0C'.      CGS100W
                     88 FL-ZFONCT-L0C                 VALUE 'L0C'.      CGS100W
                     88 FL-ZFONCT-LPU                 VALUE 'LPU'.      CGS100W
                     88 FL-ZFONCT-LP1                 VALUE 'LP1'.      CGS100W
                     88 FL-ZFONCT-M0C                 VALUE 'M0C'.      CGS100W
                   10 WS-ID-GDD                PIC X(3).                CGS100W
                   10 WS-NBRE-OCCUR            PIC S9(3) COMP-3.        CGS100W
                   10 WS-LECTURE-INIT-SUIV-PREC PIC X(1).               CGS100W
                   10 WS-INDIC-SOLDE           PIC S9(1) COMP-3.        CGS100W
                   10 WS-IDENT-APPELANT        PIC X(8).                CGS100W
                   10 WS-TYPE-APPELANT         PIC X(1).                CGS100W
                   10 WS-CODE-DSVCM            PIC X(1).                CGS100W
                   10 WS-DATE-GDD-1            PIC S9(9) COMP-3.        CGS100W
                   10 WS-DATE-GDD-2            PIC S9(9) COMP-3.        CGS100W
                   10 WS-ZRTGEN                PIC S9(3) COMP-3.        CGS100W
                   10 WS-ZRTDET                PIC S9(5) COMP-3.        CGS100W
                06 WS-IG-ARGUMENT.                                      CGS100W
                   10 WS-CLE-INF               PIC X(100).              CGS100W
                   10 WS-CLE-SUP               PIC X(100).              CGS100W
                   10 WS-NDOMAT                PIC S9(7) COMP-3.        CGS100W
                   10 FILLER                   PIC X(212).              CGS100W
                06 WS-IG-DONNEES              PIC X(6346).              CGS100W
                                                                        CGS100W
       01 LT-LITERAUX-SPECIFIQUES.                                      CGS100W
          05 LT-CGS101            PIC X(8) VALUE 'CGS101'.              CGS100W
          05 LT-CGS102            PIC X(8) VALUE 'CGS102'.              CGS100W
          05 LT-IX-AZIS-PMVT-2-MAX           PIC S9(3) COMP-3 VALUE 25. CGS100W
      *---------------------------------------------------------------  CGS100W
      *                Version : V03600                                 CGS100W
      *                                                                 CGS100W
      * Chantier : 2145T     Début : 14/12/2011     Fin : 20/12/2011    CGS100W
      *                                                                 CGS100W
      * Texte  :                                                        CGS100W
      *                                                                 CGS100W
      *                                                                 CGS100W
      *                                                                 CGS100W
      * DMFS  :                                                         CGS100W
      *       :                                                         CGS100W
      *---------------------------------------------------------------  CGS100W
      *---------------------------------------------------------------  CGS100W
      *                Version : V04200                                 CGS100W
      *                                                                 CGS100W
      * Chantier : H8793     Début : 18/11/2013     Fin : 18/11/2013    CGS100W
      *                                                                 CGS100W
      * Texte  : en cas d"explosion générale, on fait appel à l'IGAD danCGS100W
      *          paragraphe AG242-LECT-BASE-TABAK avec cd fonction 'd0c'CGS100W
      *          n'est pas prévu par l'équipe des données pour lire cddoCGS100W
      *                                                                 CGS100W
      * DMFS  :                                                         CGS100W
      *       :                                                         CGS100W
      *---------------------------------------------------------------  CGS100W
      *---------------------------------------------------------------  CGS100W
      *                Version : V04300                                 CGS100W
      *                                                                 CGS100W
      * Chantier : 2296E     Début : 09/04/2014     Fin : 09/04/2014    CGS100W
      *                                                                 CGS100W
      * Texte  : a998.ccr2296e:Réforme CA-Gestion compteurs-à compter 07CGS100W
      *          passage du CLCA à la PrePare pour dossiers avec naiss./CGS100W
      *          à compter 07/2014-inciter parents à partager Congé PareCGS100W
      *                                                                 CGS100W
      * DMFS  :                                                         CGS100W
      *       :                                                         CGS100W
      *---------------------------------------------------------------  CGS100W
      *END_OF_COPY_FILE                                                 CGS100W
                                                                        
       01 LT-END-OF-WS.                                                 
           03 LT-FINISH-OF-WS                      PIC X(40)            
      *        VALUE "***** END WORKING STORAGE &PRGRMID. *****".       
               VALUE "***** END WORKING STORAGE CGS100 *****".          
                                                                        
      ***************************************************************** 
      *                                                               * 
      *           L I N K A G E         S E C T I O N                 * 
      *                                                               * 
      ***************************************************************** 
       LINKAGE SECTION.                                                 
                                                                        
      *COPY TP-STORAGE.                                                 
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copyb/TP-STORAGE.CPY"
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 
      *LibFac  liberFactory GCOS to Linux Translator  *                 
      *LibFac  From input file: TP-STORAGE            *                 
      *LibFac  On   the   date: 2015/03/23            *                 
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 
      ***************************************************************** 
      ****                                                              
      **** TP-STORAGE BATCH                                             
      ****                                                              
      **** A VIDE POUR COMPATIBILITE MODELE BATCH ET TP                 
      ***************************************************************** 
      *END_OF_COPY_FILE                                                 
                                                                        
      *COPY CTIDCOM.                                                    
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copyb/CTIDCOM.CPY"   CTIDCOM
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CTIDCOM
      *LibFac  liberFactory GCOS to Linux Translator  *                 CTIDCOM
      *LibFac  From input file: CTIDCOM               *                 CTIDCOM
      *LibFac  On   the   date: 2015/03/23            *                 CTIDCOM
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           CTIDCOM
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CTIDCOM
      ***************************************************************** CTIDCOM
      ****                                                              CTIDCOM
      **** CTIDCOM    BATCH                                             CTIDCOM
      ****                                                              CTIDCOM
      **** A VIDE POUR COMPATIBILITE MODELE BATCH ET TP                 CTIDCOM
      ***************************************************************** CTIDCOM
      *END_OF_COPY_FILE                                                 CTIDCOM
                                                                        
       01 DFHCOMMAREA.                                                  
                                                                        
      *$$LINKAGE                                                        
      *    COPY  CGS100.                                                
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/CGS100.CPY"     CGS100
      ***************************************************************** CGS100
      *                                                               * CGS100
      *   COPYBOOK NAME  : CGS100                                     * CGS100
      *                                                               * CGS100
      *   COPYBOOK DESC  : MESSAGE SERVICE EXAMEN DR                  * CGS100
      *                                                               * CGS100
      *   GENERATION DATE: 12/06/16                                   * CGS100
      *              TIME: 09:49:21                                   * CGS100
      ***************************************************************** CGS100
         02 S100-CGS100R.                                               CGS100
           06 S100-CGS100.                                              CGS100
             10 S100-C1ENTSRV.                                          CGS100
               14 S100-ESRV-LGSERV               PIC X(5).              CGS100
               14 S100-ESRV-PISRVC               PIC X(12).             CGS100
               14 S100-ESRV-CRTSRV               PIC X(2).              CGS100
               14 S100-ESRV-PINTRV               PIC X(1).              CGS100
               14 S100-ESRV-LGPFIX               PIC X(3).              CGS100
               14 S100-ESRV-LOCCUR               PIC X(4).              CGS100
               14 S100-ESRV-NOCUTI               PIC X(3).              CGS100
               14 S100-ESRV-NTTOCU               PIC X(3).              CGS100
               14 S100-ESRV-IDORGA               PIC X(5).              CGS100
               14 S100-ESRV-APPLEXT              PIC X(1).              CGS100
               14 S100-ESRV-IND32K               PIC X(1).              CGS100
               14 S100-ESRV-PVRSER               PIC X(2).              CGS100
             10 S100-CQTECAPP.                                          CGS100
               14 S100-PIOCDE                    PIC X(28).             CGS100
               14 S100-ZTYPAP                    PIC X(1).              CGS100
                 88 S100-FL-ZTYPAP-BATCH           VALUE 'B'.           CGS100
                 88 S100-FL-ZTYPAP-TP              VALUE 'T'.           CGS100
                 88 S100-FL-ZTYPAP-BATCH-TP        VALUE 'A'.           CGS100
               14 S100-ZDSVCM                    PIC X(1).              CGS100
                 88 S100-FL-ZDSVCM-DSV             VALUE 'V'.           CGS100
                 88 S100-FL-ZDSVCM-DSC             VALUE 'C'.           CGS100
                 88 S100-FL-ZDSVCM-DSM             VALUE 'M'.           CGS100
               14 S100-ZFONCT                    PIC X(3).              CGS100
               14 S100-CRTSRV                    PIC X(2).              CGS100
               14 S100-CABEND                    PIC X(4).              CGS100
               14 S100-TABEND                    PIC X(1).              CGS100
               14 S100-ZAPPLT                    PIC X(8).              CGS100
               14 S100-NDOMAT                    PIC S9(7) COMP-3.      CGS100
             10 S100-CGS100U.                                           CGS100
               14 S100-CORGTR                    PIC X(2).              CGS100
                 88 S100-FL-DRT-POTI-TRR           VALUE 'DP'.          CGS100
                 88 S100-FL-ECHE-TRR               VALUE 'EC'.          CGS100
                 88 S100-FL-DRT-REEL-TRR           VALUE 'DR'.          CGS100
                 88 S100-FL-PRLD-TRR               VALUE 'PR'.          CGS100
                 88 S100-FL-SIMU-TRR               VALUE 'SI'.          CGS100
                 88 S100-FL-REN-APL-TRR            VALUE 'RA'.          CGS100
                 88 S100-FL-EF-TRR                 VALUE 'EF'.          CGS100
                 88 S100-FL-DRT-EF-TRR             VALUE 'RE'.          CGS100
               14 S100-ESESSI                    PIC S9(9) COMP-3.      CGS100
               14 S100-HSESSI                    PIC S9(7) COMP-3.      CGS100
               14 S100-ELIQUI                    PIC S9(9) COMP-3.      CGS100
               14 S100-HLIQUI                    PIC S9(7) COMP-3.      CGS100
               14 S100-EPTAEX                    PIC S9(9) COMP-3.      CGS100
               14 S100-FPTAEX                    PIC S9(9) COMP-3.      CGS100
      *END_OF_COPY_FILE                                                 CGS100
                                                                        
      *COPY WAZIS.                                                      
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/WAZIS.CPY"      WAZIS
      ****** ********************************************************** WAZIS
      *                                                               * WAZIS
      *   COPYBOOK NAME  : WAZIS                                      * WAZIS
      *                                                               * WAZIS
      *   COPYBOOK DESC  : AJOUT INFOS EN LINKAGE SERVICE             * WAZIS
      *   DONNEES GENERALES                                           * WAZIS
      *     ATTENTION LA TAILLES EXACT DE COPY DOIT TOUJOURS = 500    * WAZIS
      ***************************************************************** WAZIS
      ***************************************************************** WAZIS
      ***  ! ! ! ! ! ! ! !    A T T E N T I O N    ! ! ! ! ! ! ! !  *** WAZIS
      ***  En TP, les VALUE sont à prendre à titre indicatif        *** WAZIS
      ***  Ds modules de la LOCCOM, INITIALIZE d'un niveau 01 avec  *** WAZIS
      ***  AZIS pour 500 + 29500 caract.  ==>  Annulat° des VALUE)  *** WAZIS
      ***  ! ! ! ! ! ! ! !    A T T E N T I O N    ! ! ! ! ! ! ! !  *** WAZIS
      ***************************************************************** WAZIS
      ***************************************************************** WAZIS
       01  WAZIS.                                                       WAZIS
         02 AZIS-PARTIE-FIXE.                                           WAZIS
           11 AZIS-D4A2MJ                        PIC S9(9) COMP-3.      WAZIS
      *---------------------------------------------------------------* WAZIS
      *  la zone azis-erreur contient l'éventuelle info erreur        * WAZIS
      * 1-acces db2 decoupé comme suit:                               * WAZIS
      *   entrée                                                        WAZIS
      *     numero dossier             7 pos                            WAZIS
      *     code chaine                3 pos                            WAZIS
      *     type probleme              1 pos                            WAZIS
      *     type destinataire erreur   1 pos                            WAZIS
      *     programme appelant igad    8 pos                            WAZIS
      *     copy procedure appelant    8 pos                            WAZIS
      *     entite logique db2 ddé     3 pos                            WAZIS
      *     gdd   ddé                  3 pos                            WAZIS
      *     fonction igad ddé          3 pos                            WAZIS
      *     nbre occurences ddé        3 pos                            WAZIS
      *     type appelant b,t          1 pos                            WAZIS
      *     code image v,m,c           1 pos                            WAZIS
      *     date debut demandé         8 pos                            WAZIS
      *     date fin   demandé         8 pos                            WAZIS
      *   sortie                                                        WAZIS
      *     code retour général        5 pos                            WAZIS
      *     code retour détaillé       3 pos                            WAZIS
      * 2-applicatif                                                  * WAZIS
      *   entrée                                                        WAZIS
      *     numero dossier             7 pos                            WAZIS
      *     code chaine                3 pos                            WAZIS
      *     type probleme              1 pos                            WAZIS
      *     type destinataire erreur   1 pos                            WAZIS
      *     programme appelant igad    8 pos                            WAZIS
      *     copy procedure appelant    8 pos                            WAZIS
      *     date debut demandé         8 pos                            WAZIS
      *       ex:pour accèder à un paramètre à une date x               WAZIS
      *          si celui est non trouvé il peut etre utile de connaitreWAZIS
      *          la date x de recherche                                 WAZIS
      *   sortie                                                        WAZIS
      *     code cabend                1 pos                            WAZIS
      *     code tabend                1 pos                            WAZIS
      *     code crtsrv                1 pos                            WAZIS
      * 3-cette zone doit pouvoir aussi accueillir d'autres sujets    * WAZIS
      *    param etc...                                               * WAZIS
      *    voir la possibilité d'ecrire cà sur une table db2 erreurs  * WAZIS
      *---------------------------------------------------------------* WAZIS
           11 AZIS-ERREUR                        PIC X(93).             WAZIS
           11 AZIS-ERREUR-IGAD REDEFINES AZIS-ERREUR.                   WAZIS
             13 AZIS-ERREUR-IGAD-ENTREE.                                WAZIS
                15 AZIS-ERREUR-IGAD-NDOMATX.                            WAZIS
                  17 AZIS-ERREUR-IGAD-NDOMAT     PIC 9(007).            WAZIS
                15 AZIS-ERREUR-IGAD-TYPPBM       PIC X(001).            WAZIS
                15 AZIS-ERREUR-IGAD-TYPDES       PIC X(001).            WAZIS
                15 AZIS-ERREUR-IGAD-CTYTTT       PIC X(003).            WAZIS
                15 AZIS-ERREUR-IGAD-ZAPPLT       PIC X(008).            WAZIS
                15 AZIS-ERREUR-IGAD-COPROC       PIC X(008).            WAZIS
                15 AZIS-ERREUR-IGAD-ZENTLG       PIC X(003).            WAZIS
                15 AZIS-ERREUR-IGAD-ZIDGDD       PIC X(003).            WAZIS
                15 AZIS-ERREUR-IGAD-ZFONCT       PIC X(003).            WAZIS
                15 AZIS-ERREUR-IGAD-ZNOCCD       PIC 9(003).            WAZIS
                15 AZIS-ERREUR-IGAD-ZTYPAP       PIC X(001).            WAZIS
                15 AZIS-ERREUR-IGAD-ZDSVCM       PIC X(001).            WAZIS
                15 AZIS-ERREUR-IGAD-ZDTDEB       PIC 9(008).            WAZIS
                15 AZIS-ERREUR-IGAD-ZDTFIN       PIC 9(008).            WAZIS
             13 AZIS-ERREUR-IGAD-SORTIE.                                WAZIS
                15 AZIS-ERREUR-IGAD-ZRTGEN       PIC 9(005).            WAZIS
                15 AZIS-ERREUR-IGAD-ZRTDET       PIC 9(003).            WAZIS
                15 AZIS-F-COMPLEMENT-1           PIC X(027).            WAZIS
           11 AZIS-ERREUR-APPL REDEFINES AZIS-ERREUR.                   WAZIS
             13 AZIS-ERREUR-APPL-ENTREE.                                WAZIS
                15 AZIS-ERREUR-APPL-NDOMATX.                            WAZIS
                  17 AZIS-ERREUR-APPL-NDOMAT     PIC 9(007).            WAZIS
                15 AZIS-ERREUR-APPL-TYPPBM       PIC X(001).            WAZIS
                15 AZIS-ERREUR-APPL-TYPDES       PIC X(001).            WAZIS
                15 AZIS-ERREUR-APPL-CTYTTT       PIC X(003).            WAZIS
                15 AZIS-ERREUR-APPL-ZAPPLT       PIC X(008).            WAZIS
                15 AZIS-ERREUR-APPL-COPROC       PIC X(008).            WAZIS
                15 AZIS-ERREUR-APPL-ZDTDEB       PIC 9(008).            WAZIS
                15 AZIS-ERREUR-appl-ZTYPAP       PIC X(001).            WAZIS
             13 AZIS-ERREUR-APPL-SORTIE.                                WAZIS
                15 AZIS-ERREUR-APPL-CABEND       PIC X(004).            WAZIS
                15 AZIS-ERREUR-APPL-TABEND       PIC X(001).            WAZIS
                15 AZIS-ERREUR-APPL-CRTSRV       PIC X(001).            WAZIS
                15 AZIS-F-COMPLEMENT-2           PIC X(050).            WAZIS
      *---------------------------------------------------------------* WAZIS
      *   la zone point-chemin contient la structure d'appel          * WAZIS
      *    programme sous-programme                                   * WAZIS
      *    index inutilisable du fait du passage programme sous progr. *WAZIS
      *     donc utilisation d' indice                                * WAZIS
      *---------------------------------------------------------------* WAZIS
            11 AZIS-CHEMIN.                                             WAZIS
              15 AZIS-CHEMIN-ZAPPLTS OCCURS 10                          WAZIS
                                     INDEXED BY IX-AZIS-CHEMIN1.        WAZIS
                17 AZIS-CHEMIN-ZAPPLT            PIC X(008).            WAZIS
              15 IN-AZIS-CHEMIN1                 PIC S9(3) COMP-3.      WAZIS
      ***     15 IN-AZIS-CHEMIN1                 PIC 99.                WAZIS
      *---------------------------------------------------------------* WAZIS
      *****11  AZIS-PARAMETRES.                                         WAZIS
      ******  15 AZIS-ORIGIN0X.                                         WAZIS
      *  Passé **17 azis-origin0               pic s9(8) binary.        WAZIS
      *  dans   17 AZIS-ORIGIN0                PIC S9(7) COMP-3.        WAZIS
      *  W   15 AZIS-DB4A2M1-X.                                         WAZIS
      *  A      17 AZIS-DB4A2M1                  PIC S9(9) COMP-3.      WAZIS
      *  Z   15 AZIS-DB4A2M-X.                                          WAZIS
      *  I      17 AZIS-DB4A2M                   PIC S9(9) COMP-3.      WAZIS
      *  S   15 AZIS-DMCDOS                      PIC S9(9) COMP-3.      WAZIS
      *  2   15 AZIS-EXPLO-GENE                  PIC X value '2'.       WAZIS
      *****   15 AZIS-DRV-EXPLO                  PIC X value '2'.       WAZIS
      *---------------------------------------------------------------* WAZIS
            11 AZIS-WS-NDOMATX.                                         WAZIS
               15 AZIS-WS-NDOMAT   PIC S9(7) COMP-3 VALUE 0.            WAZIS
      *                                                                 WAZIS
            11 AZIS-TAB-PMVT-2.                                         WAZIS
               15 AZIS-LIGNE-PMVT-2 OCCURS 25 INDEXED BY IX-AZIS-PMVT-2.WAZIS
      *---         Mois de la PMVT avec neccessitée de ré-exécution     WAZIS
                   20 AZIS-PMVT-2-MOIS            PIC S9(9) COMP-3.     WAZIS
      *---         Flag notant si ré-exécution necessaire               WAZIS
                   20 AZIS-PMVT-2-A-FAIRE         PIC X.                WAZIS
                      88 AZIS-FL-PMVT-2-A-FAIRE       VALUE 'O'.        WAZIS
                      88 AZIS-FL-PMVT-2-PAS-A-FAIRE   VALUE 'N'.        WAZIS
                                                                        WAZIS
      *---     Rang de la PMVT (premiere ou deuxieme)                   WAZIS
            11 AZIS-RG-PMVT                      PIC X.                 WAZIS
               88 AZIS-FL-RG-PMVT-1                    VALUE '1'.       WAZIS
               88 AZIS-FL-RG-PMVT-2                    VALUE '2'.       WAZIS
            11 AZIS-FILLER                       PIC X(165).            WAZIS
      *---------------------------------------------------------------  WAZIS
      *                Version : V00530                                 WAZIS
      *                                                                 WAZIS
      * Chantier : MO111     Début : 01/09/1998     Fin : 15/09/1998    WAZIS
      *                                                                 WAZIS
      * Texte  : zone de communication inter-service AZIS               WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      * DMFS  :                                                         WAZIS
      *       :                                                         WAZIS
      *---------------------------------------------------------------  WAZIS
      *                Version : V00530                                 WAZIS
      *                                                                 WAZIS
      * Chantier : 0111O     Début : 29/10/1998     Fin : 13/01/1999    WAZIS
      *                                                                 WAZIS
      * Texte  : BANALISEE INTER-SERVICES                               WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      * DMFS  :                                                         WAZIS
      *       :                                                         WAZIS
      *---------------------------------------------------------------  WAZIS
      *                Version : V00530                                 WAZIS
      *                                                                 WAZIS
      * Chantier : 59165     Début : 15/12/1998     Fin : 15/12/1998    WAZIS
      *                                                                 WAZIS
      * Texte  : Zones déclarées en BINARY passées en COMP-3 (Pb BULL)  WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      * DMFS  :                                                         WAZIS
      *       :                                                         WAZIS
      *---------------------------------------------------------------  WAZIS
      *---------------------------------------------------------------  WAZIS
      *                Version : V01010                                 WAZIS
      *                                                                 WAZIS
      * Chantier : 0590B     Début : 30/10/2002     Fin : 18/11/2002    WAZIS
      *                                                                 WAZIS
      * Texte  :                                                        WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      * DMFS  :                                                         WAZIS
      *       :                                                         WAZIS
      *---------------------------------------------------------------  WAZIS
      *---------------------------------------------------------------  WAZIS
      *                Version : V03320                                 WAZIS
      *                                                                 WAZIS
      * Chantier : 2050A     Début : 10/01/2011     Fin : 10/01/2011    WAZIS
      *                                                                 WAZIS
      * Texte  :                                                        WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      * DMFS  :                                                         WAZIS
      *       :                                                         WAZIS
      *---------------------------------------------------------------  WAZIS
      *---------------------------------------------------------------  WAZIS
      *                Version : V03410                                 WAZIS
      *                                                                 WAZIS
      * Chantier : 2050E     Début : 14/04/2011     Fin : 26/05/2011    WAZIS
      *                                                                 WAZIS
      * Texte  :                                                        WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      * DMFS  :                                                         WAZIS
      *       :                                                         WAZIS
      *---------------------------------------------------------------  WAZIS
      *---------------------------------------------------------------  WAZIS
      *                Version : V03500                                 WAZIS
      *                                                                 WAZIS
      * Chantier : 2050H     Début : 29/07/2011     Fin : 17/08/2011    WAZIS
      *                                                                 WAZIS
      * Texte  :                                                        WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      *                                                                 WAZIS
      * DMFS  :                                                         WAZIS
      *       :                                                         WAZIS
      *---------------------------------------------------------------  WAZIS
      *END_OF_COPY_FILE                                                 WAZIS
      *COPY WAZIS2.                                                     
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/WAZIS2.CPY"     WAZIS2
      ***************************************************************** WAZIS2
      *                                                               * WAZIS2
      *   COPYBOOK NAME  : WAZIS2                                     * WAZIS2
      *                                                               * WAZIS2
      *   COPYBOOK DESC  : AJOUT INFOS EN LINKAGE SERVICE             * WAZIS2
      *   DONNEES SPECIFIQUES                                         * WAZIS2
      *   TAILLES DES DONNEE 1278                                     * WAZIS2
      ***************************************************************** WAZIS2
      ***************************************************************** WAZIS2
      ***  ! ! ! ! ! ! ! !    A T T E N T I O N    ! ! ! ! ! ! ! !  *** WAZIS2
      ***  En TP, les VALUE sont à prendre à titre indicatif => les *** WAZIS2
      ***  valeurs réelles / Données sont calculées applicativement *** WAZIS2
      ***        (Voir AZDRV0AK - Pavés Initialisation AZIS )       *** WAZIS2
      ***  Ds modules de la LOCCOM, INITIALIZE d'un niveau 01 avec  *** WAZIS2
      ***  AZIS pour 500 + 29500 caract.  ==>  Annulat° des VALUE)  *** WAZIS2
      ***  ! ! ! ! ! ! ! !    A T T E N T I O N    ! ! ! ! ! ! ! !  *** WAZIS2
      ***************************************************************** WAZIS2
      ***************************************************************** WAZIS2
         02 AZIS-PARTIE-SPEC.                                           WAZIS2
      *    11 azis2-filler                        pic x(29500).         WAZIS2
      *    11 azis-donnees redefines azis2-filler.                      WAZIS2
      *---------------------------------------------------------------* WAZIS2
      * Parametres Généraux AZIS                                        WAZIS2
      *    origine de la banalisee                                      WAZIS2
      *    date debut  +  date fin  P.Mouvementée GESDRO                WAZIS2
      *    date Mois en Cours                                           WAZIS2
      *    flag explosion generale et explosion CDDRV                   WAZIS2
      *---------------------------------------------------------------* WAZIS2
              13 AZIS-PARAMETRES.                                       WAZIS2
                 15 AZIS-ORIGIN0X.                                      WAZIS2
                    17 AZIS-ORIGIN0                PIC S9(7) COMP-3.    WAZIS2
                 15 AZIS-DB4A2M1-X.                                     WAZIS2
                    17 AZIS-DB4A2M1                  PIC S9(9) COMP-3.  WAZIS2
                 15 AZIS-DB4A2M-X.                                      WAZIS2
                    17 AZIS-DB4A2M                   PIC S9(9) COMP-3.  WAZIS2
                 15 AZIS-DMCDOS                      PIC S9(9) COMP-3.  WAZIS2
                 15 AZIS-EXPLO-GENE                  PIC X value '2'.   WAZIS2
                 15 AZIS-DRV-EXPLO                   PIC X value '2'.   WAZIS2
      *---------------------------------------------------------------* WAZIS2
      * Parametres AZIS liés à la CDDRV                                 WAZIS2
      *    longueur maillon                                             WAZIS2
      *    longueur datas                                               WAZIS2
      * Zones de travail liées à la CDDRV                               WAZIS2
      *---------------------------------------------------------------* WAZIS2
              13 AZIS-DRV-PARAMETRES.                                   WAZIS2
                 15 AZIS-DRV-LNG-TOT        PIC S9(3) COMP-3 VALUE  34. WAZIS2
                 15 AZIS-DRV-LNG-DATA       PIC S9(3) COMP-3 VALUE  16. WAZIS2
                 15 AZIS-DRV-OCC            PIC 99.                     WAZIS2
                 15 IN-AZIS-DRV-TDE-LIM     PIC S9(7) COMP-3.           WAZIS2
                 15 IN-AZIS-DRV-TDE-MAX     PIC S9(7) COMP-3 VALUE 30.  WAZIS2
                 15 IN-AZIS-DRV-TDS-LIM     PIC S9(7) COMP-3.           WAZIS2
                 15 IN-AZIS-DRV-TDS-MAX     PIC S9(7) COMP-3 VALUE 30.  WAZIS2
              13 AZIS-WORKING.                                          WAZIS2
                 15 AZIS-WORKING-GENE.                                  WAZIS2
                    17 AZIS-WS-ORIX.                                    WAZIS2
                       19 AZIS-WS-ORI       PIC S9(7) COMP-3.           WAZIS2
                    17 AZIS-WS-ORI-SVX.                                 WAZIS2
                       19 AZIS-WS-ORI-SV    PIC S9(7) COMP-3.           WAZIS2
                    17 AZIS-WS-ORI-SVS      PIC S9(7) COMP-3.           WAZIS2
                    17 AZIS-WS-PRX.                                     WAZIS2
                       19 AZIS-WS-PR        PIC S9(7) COMP-3.           WAZIS2
                    17 AZIS-WS-RESUL        PIC S9(3) COMP-3.           WAZIS2
                    17 AZIS-WS-RESTE        PIC 99.                     WAZIS2
                    17 AZIS-DRV-WS-I-DATREC PIC 9(8).                   WAZIS2
                    17 AZIS-DRV-DATREC-RED REDEFINES                    WAZIS2
                                           AZIS-DRV-WS-I-DATREC.        WAZIS2
                       19 AZIS-DRV-DATREC-SSAAMM PIC 9(6).              WAZIS2
                       19 AZIS-DRV-DATREC-JJ     PIC 99.                WAZIS2
                    17 AZIS-WS-DATE         PIC 9(8).                   WAZIS2
                    17 AZIS-WS-DATE-RED REDEFINES AZIS-WS-DATE.         WAZIS2
                       19 AZIS-WS-SSAAMM    PIC 9(6).                   WAZIS2
                       19 AZIS-WS-SSAAMM-RED REDEFINES AZIS-WS-SSAAMM.  WAZIS2
                          21 AZIS-WS-SSAA   PIC 9(4).                   WAZIS2
                          21 AZIS-WS-MM     PIC 99.                     WAZIS2
                             88 AZIS-MM-31  VALUE 01 03 05 07 08 10 12. WAZIS2
                             88 AZIS-MM-30  VALUE 04 06 09 11.          WAZIS2
                             88 AZIS-MM-JANVIER  VALUE 01.              WAZIS2
                             88 AZIS-MM-FEVRIER  VALUE 02.              WAZIS2
                             88 AZIS-MM-MARS     VALUE 03.              WAZIS2
                             88 AZIS-MM-AVRIL    VALUE 04.              WAZIS2
                             88 AZIS-MM-MAI      VALUE 05.              WAZIS2
                             88 AZIS-MM-JUILLET  VALUE 07.              WAZIS2
                             88 AZIS-MM-OCTOBRE  VALUE 10.              WAZIS2
                             88 AZIS-MM-DECEMBRE VALUE 12.              WAZIS2
                       19 AZIS-WS-JJ        PIC 99.                     WAZIS2
                    17 AZIS-DFIN-DECH       PIC S9(9) COMP-3 VALUE 0.   WAZIS2
                    17 AZIS-WS-ERR          PIC X.                      WAZIS2
                       88 FL-AZIS-WS-ERR-OUI VALUE 'O'.                 WAZIS2
                       88 FL-AZIS-WS-ERR-NON VALUE 'N'.                 WAZIS2
                    17 AZIS-WS-ECRI         PIC X.                      WAZIS2
                    17 AZIS-WS-DEL          PIC X.                      WAZIS2
                    17 AZIS-WS-EXPLO        PIC X.                      WAZIS2
                       88 FL-AZIS-WS-EXPLO-2 VALUE '2'.                 WAZIS2
                    17 AZIS-WS-PEL          PIC X.                      WAZIS2
                    17 AZIS-WS-DERMAIL      PIC X.                      WAZIS2
                 15 AZIS-DRV-WORKING.                                   WAZIS2
                    17 AZIS-DRV-WS-CLE-INF.                             WAZIS2
                       19 AZIS-DRV-WS-I-CLE.                            WAZIS2
                         21 AZIS-DRV-WS-I-PELEM.                        WAZIS2
                           23 AZIS-DRV-WS-I-CNAPRS   PIC X(3).          WAZIS2
                           23 AZIS-DRV-WS-I-RSNAPR   PIC X(2).          WAZIS2
                         21 AZIS-DRV-WS-I-CNADES     PIC X(1).          WAZIS2
                         21 AZIS-DRV-WS-I-NDESTI     PIC S9(7) COMP-3.  WAZIS2
                         21 AZIS-DRV-WS-I-CNAPER     PIC X(1).          WAZIS2
                         21 AZIS-DRV-WS-I-NFICPN     PIC S9(7) COMP-3.  WAZIS2
                       19 AZIS-DRV-WS-I-DATMIN       PIC S9(9) COMP-3.  WAZIS2
                    17 AZIS-DRV-WS-CLE-SUP.                             WAZIS2
                       19 AZIS-DRV-WS-S-CLE.                            WAZIS2
                         21 AZIS-DRV-WS-S-PELEM.                        WAZIS2
                           23 AZIS-DRV-WS-S-CNAPRS   PIC X(3).          WAZIS2
                           23 AZIS-DRV-WS-S-RSNAPR   PIC X(2).          WAZIS2
                         21 AZIS-DRV-WS-S-CNADES     PIC X(1).          WAZIS2
                         21 AZIS-DRV-WS-S-NDESTI     PIC S9(7) COMP-3.  WAZIS2
                         21 AZIS-DRV-WS-S-CNAPER     PIC X(1).          WAZIS2
                         21 AZIS-DRV-WS-S-NFICPN     PIC S9(7) COMP-3.  WAZIS2
                       19 AZIS-DRV-WS-S-DATMAX       PIC S9(9) COMP-3.  WAZIS2
                 15 AZIS-DRV-WS-MAILLON.                                WAZIS2
                    17 AZIS-DRV-WS-MAILLON-CLE.                         WAZIS2
                       19 AZIS-DRV-WS-MA-PRAVTX.                        WAZIS2
                         21 AZIS-DRV-WS-MA-PRAVT    PIC S9(7) COMP-3.   WAZIS2
                       19 AZIS-DRV-WS-MA-PRAPSX.                        WAZIS2
                         21 AZIS-DRV-WS-MA-PRAPS    PIC S9(7) COMP-3.   WAZIS2
                       19 AZIS-DRV-WS-MA-EDROIT     PIC S9(9) COMP-3.   WAZIS2
                       19 AZIS-DRV-WS-MA-FDROIT     PIC S9(9) COMP-3.   WAZIS2
                    17 AZIS-DRV-WS-MAILLON-DATA     PIC X(16).          WAZIS2
                 15 AZIS-DRV-WS-MAILLON-SUP.                            WAZIS2
                    17 AZIS-DRV-WS-MAILLON-SUP-CLE.                     WAZIS2
                       19 AZIS-DRV-WS-MA-SU-PRAVTX.                     WAZIS2
                         21 AZIS-DRV-WS-MA-SU-PRAVT PIC S9(7) COMP-3.   WAZIS2
                       19 AZIS-DRV-WS-MA-SU-PRAPSX.                     WAZIS2
                         21 AZIS-DRV-WS-MA-SU-PRAPS PIC S9(7) COMP-3.   WAZIS2
                       19 AZIS-DRV-WS-MA-SU-EDROIT PIC S9(9) COMP-3.    WAZIS2
                       19 AZIS-DRV-WS-MA-SU-FDROIT PIC S9(9) COMP-3.    WAZIS2
                    17 AZIS-DRV-WS-MAILLON-SUP-DATA PIC X(16).          WAZIS2
              13 AZIS-FIXE.                                             WAZIS2
                15 AZIS-TD-ENTREE.                                      WAZIS2
                   17 AZIS-DRV-TDE.                                     WAZIS2
                     19 AZIS-DRV-TD-ENTREE OCCURS 30                    WAZIS2
                                           INDEXED BY IX-AZIS-DRV-TDE1  WAZIS2
                                                      IX-AZIS-DRV-TDE2  WAZIS2
                                                      IX-AZIS-DRV-TDE3. WAZIS2
                       20 AZIS-DRV-TDE-G.                               WAZIS2
                         21 AZIS-DRV-TDE-CLE.                           WAZIS2
                           23 AZIS-DRV-TDE-PELEM.                       WAZIS2
                             25 AZIS-DRV-TDE-CNAPRS   PIC X(3).         WAZIS2
                             25 AZIS-DRV-TDE-RSNAPR   PIC X(2).         WAZIS2
                           23 AZIS-DRV-TDE-CNADES     PIC X(1).         WAZIS2
                           23 AZIS-DRV-TDE-NDESTI     PIC S9(7) COMP-3. WAZIS2
                           23 AZIS-DRV-TDE-CNAPER     PIC X(1).         WAZIS2
                           23 AZIS-DRV-TDE-NFICPN     PIC S9(7) COMP-3. WAZIS2
                         21 AZIS-DRV-TDE-PRDEBX.                        WAZIS2
                            23 AZIS-DRV-TDE-PRDEB     PIC S9(7) COMP-3. WAZIS2
                         21 AZIS-DRV-TDE-PRFINX.                        WAZIS2
                            23 AZIS-DRV-TDE-PRFIN     PIC S9(7) COMP-3. WAZIS2
                15 AZIS-TD-SORTIE.                                      WAZIS2
                   17 AZIS-DRV-TDS.                                     WAZIS2
                     19 AZIS-DRV-TD-SORTIE OCCURS 30                    WAZIS2
                                           INDEXED BY IX-AZIS-DRV-TDS1  WAZIS2
                                                      IX-AZIS-DRV-TDS2  WAZIS2
                                                      IX-AZIS-DRV-TDS3. WAZIS2
                        20 AZIS-DRV-TDS-G.                              WAZIS2
                           21 AZIS-DRV-TDS-CLE.                         WAZIS2
                              23 AZIS-DRV-TDS-PELEM.                    WAZIS2
                                25 AZIS-DRV-TDS-CNAPRS PIC X(3).        WAZIS2
                                25 AZIS-DRV-TDS-RSNAPR PIC X(2).        WAZIS2
                              23 AZIS-DRV-TDS-CNADES  PIC X(1).         WAZIS2
                              23 AZIS-DRV-TDS-NDESTI  PIC S9(7) COMP-3. WAZIS2
                              23 AZIS-DRV-TDS-CNAPER  PIC X(1).         WAZIS2
                              23 AZIS-DRV-TDS-NFICPN  PIC S9(7) COMP-3. WAZIS2
                           21 AZIS-DRV-TDS-PRDEBX.                      WAZIS2
                              23 AZIS-DRV-TDS-PRDEB   PIC S9(7) COMP-3. WAZIS2
                           21 AZIS-DRV-TDS-PRFINX.                      WAZIS2
                              23 AZIS-DRV-TDS-PRFIN   PIC S9(7) COMP-3. WAZIS2
              13 AZIS-FIXE-RESERVE                    PIC X(22).        WAZIS2
              13 AZIS-VARIABLE.                                         WAZIS2
                15 AZIS-BAN                   PIC X(27900).             WAZIS2
      *-------------------------------------------------------------    WAZIS2
      *****PARTIE EN REDEFINITION POUR SIMULATION DROIT                 WAZIS2
      *-------------------------------------------------------------    WAZIS2
      **        15 AZIM-BAN REDEFINES AZIS-BAN.                         WAZIS2
      **          17 AZIM-FIXE.                                         WAZIS2
      *----------------------------------                               WAZIS2
      *  la pelem ou la pgene à etudier                                 WAZIS2
      *----------------------------------                               WAZIS2
      **            20 AZIM-CNAPRG.                                     WAZIS2
      **              25 AZIM-CNAPRS          PIC X(3).                 WAZIS2
      **              25 AZIM-RSNAPR          PIC X(2).                 WAZIS2
      *----------------------------------                               WAZIS2
      *  INFOS GENERALES (indices etc ...)                              WAZIS2
      *----------------------------------                               WAZIS2
      **            20 AZIM-IN1X.                                       WAZIS2
      **               30 AZIM-IN1            PIC 99.                   WAZIS2
      **            20 AZIM-IN2               PIC 99.                   WAZIS2
      **            20 AZIM-IN3               PIC 99.                   WAZIS2
      **            20 AZIM-IN4               PIC 99.                   WAZIS2
      **            20 AZIM-IN5               PIC 99.                   WAZIS2
      **            20 AZIM-OT1               PIC 99.                   WAZIS2
      **            20 AZIM-OT2               PIC 99.                   WAZIS2
      **            20 AZIM-OT3               PIC 99.                   WAZIS2
      **            20 AZIM-BIDON-X           PIC X(10).                WAZIS2
      **            20 AZIM-BIDON-9           PIC 9(05).                WAZIS2
      **            20 AZIM-ZENTLG-ZIDGDD-ZFONCT-SIMU.                  WAZIS2
      **               30 AZIM-ZENTLG-SIMU    PIC XXX.                  WAZIS2
      **               30 AZIM-ZIDGDD-SIMU    PIC XXX.                  WAZIS2
      **               30 AZIM-ZFONCT-SIMU    PIC XXX.                  WAZIS2
      **            20 AZIM-MODULE            PIC X(20).                WAZIS2
      **            20 AZIM-D1                PIC 9(08).                WAZIS2
      **            20 AZIM-D1-R REDEFINES   AZIM-D1.                   WAZIS2
      **               25 AZIM-D1-SSAA        PIC 9(04).                WAZIS2
      **               25 AZIM-D1-MM          PIC 9(02).                WAZIS2
      **               25 AZIM-D1-JJ          PIC 9(02).                WAZIS2
      **            20 AZIM-D2                PIC 9(08).                WAZIS2
      **            20 AZIM-D2-R REDEFINES   AZIM-D2.                   WAZIS2
      **               25 AZIM-D2-SSAA        PIC 9(04).                WAZIS2
      **               25 AZIM-D2-MM          PIC 9(02).                WAZIS2
      **               25 AZIM-D2-JJ          PIC 9(02).                WAZIS2
      *             20 AZIM-D3                PIC 9(08).                WAZIS2
      *             20 AZIM-D3-R REDEFINES   AZIM-D3.                   WAZIS2
      *                25 AZIM-D3-SSAA        PIC 9(04).                WAZIS2
      *                25 AZIM-D3-MM          PIC 9(02).                WAZIS2
      *                25 AZIM-D3-JJ          PIC 9(02).                WAZIS2
      *             20 AZIM-RESERVE           PIC X(20).                WAZIS2
      *-------------------------------------------------------------    WAZIS2
      *          17 AZIM-AUTRE.                                         WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * mois en cours du dossier doit toujours etre > à mois traité     WAZIS2
      *------------------------------------------------------------     WAZIS2
      *            20 AZIM-DOS.                                         WAZIS2
      *               30 AZIM-DOS-ZZZ-DMCDOS  PIC S9(9) COMP-3.         WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * infos doh                                                       WAZIS2
      *-------------                                                    WAZIS2
      *            20 AZIM-DOHS.                                        WAZIS2
      *              21 AZIM-DOH OCCURS 01.                             WAZIS2
      *                25 AZIM-DOH-LIMS.                                WAZIS2
      *                  26 AZIM-DOH-LIM OCCURS 03.                     WAZIS2
      *                     30 AZIM-DOH-LIM-CDHIS001 PIC S9(9) COMP-3.  WAZIS2
      *                     30 AZIM-DOH-LIM-CDHIS002 PIC S9(9) COMP-3.  WAZIS2
      *                     30 AZIM-DOH-LIM-CTLMAT PIC X(3).            WAZIS2
      *                25 AZIM-DOH-RSFS.                                WAZIS2
      *                  26 AZIM-DOH-RSF OCCURS 01.                     WAZIS2
      *                     30 AZIM-DOH-RSF-CRESFA PIC X(2).            WAZIS2
      *                                                                 WAZIS2
      *            20 AZIM-DOH-XXX-1          PIC 99.                   WAZIS2
      *            20 AZIM-DOH-LIM-1          PIC 99.                   WAZIS2
      *            20 AZIM-RESERVE            PIC X(18).                WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * code allocataire prestation                                     WAZIS2
      *----------------------------                                     WAZIS2
      *            20 AZIM-AL0.                                         WAZIS2
      *               30 AZIM-AL0-ALC-CFAPRS  PIC X(3).                 WAZIS2
      *            20 AZIM-RESERVE            PIC X(10).                WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * les personnes constituant le dossier exprimé en nombre          WAZIS2
      *-------------------------------------------------------          WAZIS2
      *            20 AZIM-DP0.                                         WAZIS2
      *               30 AZIM-DP0-CDP-MON     PIC 9(1).                 WAZIS2
      *               30 AZIM-DP0-CDP-MME     PIC 9(1).                 WAZIS2
      *               30 AZIM-DP0-CDP-ENF     PIC 9(2).                 WAZIS2
      *               30 AZIM-DP0-CDP-AUT     PIC 9(1).                 WAZIS2
      *            20 AZIM-RESERVE            PIC X(20).                WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * les statiques indispensables des personnes                      WAZIS2
      *---------------------------------------------                    WAZIS2
      *            20 AZIM-PESS.                                        WAZIS2
      *              21 AZIM-PES OCCURS 10.                             WAZIS2
      *                 30 AZIM-PES-ZZZ-DNAIPN PIC S9(9) COMP-3.        WAZIS2
      *            20 AZIM-RESERVE             PIC X(050).              WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * les gdds histo indispensables des personnes                     WAZIS2
      *---------------------------------------------                    WAZIS2
      *            20 AZIM-PEHS.                                        WAZIS2
      *              21 AZIM-PEH OCCURS 10.                             WAZIS2
      *                25 AZIM-PEH-AAHS.                                WAZIS2
      *                  26 AZIM-PEH-AAH OCCURS 05.                     WAZIS2
      *                    30 AZIM-PEH-AAH-CDHIS001 PIC S9(9) COMP-3.   WAZIS2
      *                    30 AZIM-PEH-AAH-CDHIS002 PIC S9(9) COMP-3.   WAZIS2
      *                    30 AZIM-PEH-AAH-CACTIV PIC X(3).             WAZIS2
      *                    30 AZIM-PEH-AAH-CREGIM PIC X(2).             WAZIS2
      *                 25 AZIM-PEH-NAAS.                               WAZIS2
      *                   26 AZIM-PEH-NAA OCCURS 05.                    WAZIS2
      **                    30 AZIM-PEH-NAA-CDHIS001 PIC S9(9) COMP-3.  WAZIS2
      *                     30 AZIM-PEH-NAA-CDHIS002 PIC S9(9) COMP-3.  WAZIS2
      *                     30 AZIM-PEH-NAA-CACTIV2 PIC X(3).           WAZIS2
      *                     30 AZIM-PEH-NAA-EACTPN3 PIC S9(9) COMP-3.   WAZIS2
      *                 25 AZIM-PEH-RSDS.                               WAZIS2
      *                   26 AZIM-PEH-RSD OCCURS 05.                    WAZIS2
      *                     30 AZIM-PEH-RSD-CDHIS001 PIC S9(9) COMP-3.  WAZIS2
      *                     30 AZIM-PEH-RSD-CDHIS002 PIC S9(9) COMP-3.  WAZIS2
      *                     30 AZIM-PEH-RSD-CRESPN PIC X(2).            WAZIS2
      *                                                                 WAZIS2
      *            20 AZIM-PEH-XXX-1          PIC 99.                   WAZIS2
      *            20 AZIM-PEH-AAH-1          PIC 99.                   WAZIS2
      **           20 AZIM-PEH-NAA-1          PIC 99.                   WAZIS2
      *            20 AZIM-PEH-RSD-1          PIC 99.                   WAZIS2
      *            20 AZIM-RESERVE             PIC X(187).              WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * les ressources indispensables des personnes                     WAZIS2
      *--------------------------------------------                     WAZIS2
      *            20 AZIM-PERS.                                        WAZIS2
      *               21 AZIM-PER OCCURS 10.                            WAZIS2
      *                 23 AZIM-PER-RESS.                               WAZIS2
      *                   24 AZIM-PER-RES OCCURS 05.                    WAZIS2
      *                     25 AZIM-PER-RES-CPTRSA PIC X(1).            WAZIS2
      **                    25 AZIM-PER-RESXS.                          WAZIS2
      *                        26 AZIM-PER-RESX OCCURS 10.              WAZIS2
      *                          30 AZIM-PER-RES-CNARES PIC X(3).       WAZIS2
      *                      30 AZIM-PER-RES-MTREPN PIC S9(7)V99 COMP-3.WAZIS2
      *                                                                 WAZIS2
      *            20 AZIM-PER-RES-1          PIC 99.                   WAZIS2
      *            20 AZIM-PER-RES-2          PIC 99.                   WAZIS2
      *            20 AZIM-PER-RES-3          PIC 99.                   WAZIS2
      *            20 AZIM-RESERVE            PIC X(194).               WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * les ressources indispensables eval forf                         WAZIS2
      *----------------------------------------                         WAZIS2
      *            20 AZIM-PER2S.                                       WAZIS2
      *              21 AZIM-PER2 OCCURS 03.                            WAZIS2
      **               23 AZIM-PER-EVAS.                                WAZIS2
      *                  24 AZIM-PER-EVA OCCURS 02.                     WAZIS2
      *                    25 AZIM-PER-EVA-CPTRSA PIC X(1).             WAZIS2
      *                    25 AZIM-PER-EVA-CDHIS001 PIC S9(9) COMP-3.   WAZIS2
      *                    25 AZIM-PER-EVA-CDHIS002 PIC S9(9) COMP-3.   WAZIS2
      *                    25 AZIM-PER-EVAXS.                           WAZIS2
      *                      26 AZIM-PER-EVAX OCCURS 02.                WAZIS2
      **                       30 AZIM-PER-EVA-CNARES1 PIC X(3).        WAZIS2
      *               30 AZIM-PER-EVA-MAEVFO   PIC S9(7)V99 COMP-3.     WAZIS2
      *               30 AZIM-PER-EVA-MMSEVF   PIC S9(7)V99 COMP-3.     WAZIS2
      *               30 AZIM-PER-EVA-CDEVA003 PIC S9(3)V99 COMP-3.     WAZIS2
      *               30 AZIM-PER-EVA-DMOIEF   PIC S9(9) COMP-3.        WAZIS2
      *                                                                 WAZIS2
      *            20 AZIM-PER-EVA-1          PIC 99.                   WAZIS2
      *            20 AZIM-PER-EVA-2          PIC 99.                   WAZIS2
      **           20 AZIM-PER-EVA-3          PIC 99.                   WAZIS2
      *            20 AZIM-RESERVE            PIC X(200).               WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * les infos logement indispensables si simu logement              WAZIS2
      *---------------------------------------------------              WAZIS2
      *            20 AZIM-LGH.                                         WAZIS2
      *               30 AZIM-LGH-CAL-CPLLGT  PIC X(2).                 WAZIS2
      *               30 AZIM-LGH-CAL-COCLGT  PIC X(2).                 WAZIS2
      *               30 AZIM-LGH-CAL-CZGLGT  PIC X(1).                 WAZIS2
      *               30 AZIM-LGH-CML-MMLRBT  PIC S9(5)V99 COMP-3.      WAZIS2
      *               30 AZIM-LGH-COL-QCOLGT  PIC S9(3) COMP-3.         WAZIS2
      *               30 AZIM-LGH-PPL-RPRETX  PIC S9(3) COMP-3.         WAZIS2
      *            20 AZIM-RESERVE            PIC X(100).               WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * les infos prets    indispensables si simu logement              WAZIS2
      *---------------------------------------------------              WAZIS2
      *            20 AZIM-PRSS.                                        WAZIS2
      *            21 AZIM-PRS OCCURS 5.                                WAZIS2
      *               30 AZIM-PRS-ZZZ-RPRETX  PIC S9(3) COMP-3.         WAZIS2
      *               30 AZIM-PRS-ZZZ-DSICON  PIC S9(9) COMP-3.         WAZIS2
      *               30 AZIM-PRS-ZZZ-CNAPRE  PIC X(1).                 WAZIS2
      *            20 AZIM-PRS-ZZZ-1          PIC 99.                   WAZIS2
      *            20 AZIM-PRS-ZZZ-2          PIC 99.                   WAZIS2
      *            20 AZIM-PRS-ZZZ-3          PIC 99.                   WAZIS2
      *            20 AZIM-RESERVE            PIC X(061).               WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * les infos grossesse si presence grossesse sur dossier           WAZIS2
      *------------------------------------------------------           WAZIS2
      *            20 AZIM-GR0.                                         WAZIS2
      *               30 AZIM-GR0-GRO-DDECGR  PIC S9(9) COMP-3.         WAZIS2
      *            20 AZIM-RESERVE            PIC X(100).               WAZIS2
      *-------------------------------------------------------------    WAZIS2
      *-------------------------------------------------------------    WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * les valeurs retournées de l examen de droit                     WAZIS2
      * à destination de l affichage                                    WAZIS2
      *---------------------------------------------                    WAZIS2
      *            20 AZIM-RETOUR.                                      WAZIS2
      *               30 AZIM-RETOUR-CNAPRG   PIC X(5).                 WAZIS2
      *               30 AZIM-RETOUR-MDROPI   PIC S9(05)V9(2) COMP-3.   WAZIS2
      *               30 AZIM-RETOUR-MASRES   PIC S9(07)V9(2) COMP-3.   WAZIS2
      *               30 AZIM-RETOUR-K        PIC S9(10)V9(2) COMP-3.   WAZIS2
      *               30 AZIM-RETOUR-L        PIC S9(10)V9(2) COMP-3.   WAZIS2
      **              30 AZIM-RETOUR-C        PIC S9(10)V9(2) COMP-3.   WAZIS2
      *               30 AZIM-RETOUR-L0       PIC S9(10)V9(7) COMP-3.   WAZIS2
      *            20 AZIM-RESERVE            PIC X(095).               WAZIS2
      *-------------------------------------------------------------    WAZIS2
      *-------------------------------------------------------------    WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * zone de manoeuvre                                               WAZIS2
      *-------------------                                              WAZIS2
      *            20 AZIM-MANOEUVRE.                                   WAZIS2
      *               30 AZIM-MANOEU-NFIPCNX.                           WAZIS2
      *                  35 AZIM-MANOEU-NFIPCN PIC 9(7).                WAZIS2
      *-------------------------------------------------------------    WAZIS2
      * zone pour CBSDAD                                                WAZIS2
      *-------------------                                              WAZIS2
       01 WAZIS-DAD REDEFINES WAZIS.                                    WAZIS2
          05 WAZIS-DADX OCCURS 1000 INDEXED BY IX-WAZIS-DADX.           WAZIS2
             10 WAZIS-DAD-CAS    PIC X(20).                             WAZIS2
      * cas d exclusion recherche                                       WAZIS2
             10 WAZIS-DAD-CAS-X0 REDEFINES WAZIS-DAD-CAS.               WAZIS2
                15 WAZIS-DAD-X0-X0      PIC XX.                         WAZIS2
                15 FILLER               PIC X.                          WAZIS2
                15 WAZIS-DAD-X0-ZENTLG  PIC XXX.                        WAZIS2
                15 WAZIS-DAD-X0-ZIDGDD  PIC XXX.                        WAZIS2
                15 WAZIS-F-RESERVE-1    PIC X(11).                      WAZIS2
      * cas d inclusion recherche                                       WAZIS2
             10 WAZIS-DAD-CAS-I0 REDEFINES WAZIS-DAD-CAS.               WAZIS2
                15 WAZIS-DAD-I0-I0      PIC XX.                         WAZIS2
                15 FILLER               PIC X.                          WAZIS2
      *         * code entité logique                                   WAZIS2
                15 WAZIS-DAD-I0-ZENTLG  PIC XXX.                        WAZIS2
      *         * code gdd                                              WAZIS2
                15 WAZIS-DAD-I0-ZIDGDD  PIC XXX.                        WAZIS2
      *         * position relative dans le gdd                         WAZIS2
                15 WAZIS-DAD-I0-POREDO  PIC XXX.                        WAZIS2
                15 WAZIS-F-RESERVE-2    PIC X(8).                       WAZIS2
      * cas précision choix image amplitude de temps                    WAZIS2
             10 WAZIS-DAD-CAS-IA REDEFINES WAZIS-DAD-CAS.               WAZIS2
                15 WAZIS-DAD-IA-IA      PIC XX.                         WAZIS2
                15 FILLER               PIC X.                          WAZIS2
      *         * image vcm souhaitée pour le couple zentlg zidgdd      WAZIS2
                15 WAZIS-DAD-IA-ZDSVCM  PIC X.                          WAZIS2
      *         * date début recherche pour le couple zentlg zidgdd     WAZIS2
                15 WAZIS-DAD-IA-ZDTDEB  PIC S9(9) COMP-3.               WAZIS2
      *         * date fin   recherche pour le couple zentlg zidgdd     WAZIS2
                15 WAZIS-DAD-IA-ZDTFIN  PIC S9(9) COMP-3.               WAZIS2
                15 WAZIS-F-RESERVE-3    PIC X(6).                       WAZIS2
      * pour jouer sur les positions DRV CDHIS001 et 2                  WAZIS2
          05 WAZIS-IN01  PIC 9999.                                      WAZIS2
          05 WAZIS-IN02  PIC 9999.                                      WAZIS2
      * pour jouer sur wazis en origine:longueur                        WAZIS2
      *   05 WAZIS-IN3  PIC 99999.                                      WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *                Version : V03310                                 WAZIS2
      *                                                                 WAZIS2
      * Chantier : 1R331     Début : 17/11/2010     Fin : 17/11/2010    WAZIS2
      *                                                                 WAZIS2
      * Texte  :                                                        WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      * DMFS  :                                                         WAZIS2
      *       :                                                         WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *                Version : V03320                                 WAZIS2
      *                                                                 WAZIS2
      * Chantier : 2050A     Début : 22/12/2010     Fin : 10/01/2011    WAZIS2
      *                                                                 WAZIS2
      * Texte  :                                                        WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      * DMFS  :                                                         WAZIS2
      *       :                                                         WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *                Version : V03400                                 WAZIS2
      *                                                                 WAZIS2
      * Chantier : 2050C     Début : 08/02/2011     Fin : 14/02/2011    WAZIS2
      *                                                                 WAZIS2
      * Texte  :                                                        WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      * DMFS  :                                                         WAZIS2
      *       :                                                         WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *                Version : V03410                                 WAZIS2
      *                                                                 WAZIS2
      * Chantier : 2050E     Début : 14/04/2011     Fin : 20/05/2011    WAZIS2
      *                                                                 WAZIS2
      * Texte  :                                                        WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      * DMFS  :                                                         WAZIS2
      *       :                                                         WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *                Version : V03500                                 WAZIS2
      *                                                                 WAZIS2
      * Chantier : 2050H     Début : 28/07/2011     Fin : 28/07/2011    WAZIS2
      *                                                                 WAZIS2
      * Texte  :                                                        WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      *                                                                 WAZIS2
      * DMFS  :                                                         WAZIS2
      *       :                                                         WAZIS2
      *---------------------------------------------------------------  WAZIS2
      *END_OF_COPY_FILE                                                 WAZIS2
      ***************************************************************** 
      *                                                               * 
      *         P R O C E D U R E    D I V I S I O N                  * 
      *                                                               * 
      ***************************************************************** 
      *COPY CDARRIV2.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copyb/CDARRIV2.CPY"  CDARRIV2
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CDARRIV2
      *LibFac  liberFactory GCOS to Linux Translator  *                 CDARRIV2
      *LibFac  From input file: CDARRIV2              *                 CDARRIV2
      *LibFac  On   the   date: 2015/03/23            *                 CDARRIV2
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           CDARRIV2
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CDARRIV2
      ******************************************************************CDARRIV2
      *** CDARRIV2                                                   ***CDARRIV2
      ***   P R O C E D U R E    D I V I S I O N   - BATCH -         ***CDARRIV2
      ***                                                            ***CDARRIV2
      ******************************************************************CDARRIV2
       PROCEDURE DIVISION  USING  DFHCOMMAREA                           CDARRIV2
                                  WAZIS.                                CDARRIV2
      *                                                                 CDARRIV2
      *END_OF_COPY_FILE                                                 CDARRIV2
                                                                        
      *COPY WSHSER02.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copyb/WSHSER02.CPY"  WSHSER02
      ***************************************************************** WSHSER02
      * MISE EN PLACE APPEL MODULE DE PERFORMANCE                       WSHSER02
      * DVN1417 06/11/92                                                WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *          P R O C E D U R E - D I V I S I O N                  * WSHSER02
      *            S H E L L   D E   S E R V I C E                    * WSHSER02
      *  -----------------------------------------------------------  * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
       S0000-MAINLINE.                                                  WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *  S 0 0 0 0 - M A I N L I N E                                  * WSHSER02
      *  -----------------------------------------------------------  * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE LT-BINARY-ONE TO DB-TRACE-INDEX.                        WSHSER02
      *    MOVE LT-BINARY-ZERO TO DB-RESET-COUNT.                       WSHSER02
      *    MOVE 'S0000B' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
           PERFORM S0500-HOUSEKEEPING.                                  WSHSER02
                                                                        WSHSER02
           IF  AS-APPL-PROC-OK                                          WSHSER02
             PERFORM S1000-PROCESS-REQUEST-ID                           WSHSER02
           END-IF.                                                      WSHSER02
                                                                        WSHSER02
           PERFORM S9500-WRAP-UP.                                       WSHSER02
                                                                        WSHSER02
           PERFORM S9800-EXIT-PROGRAM.                                  WSHSER02
                                                                        WSHSER02
       S0500-HOUSEKEEPING.                                              WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *  S 0 5 0 0 - H O U S E K E E P I N G                          * WSHSER02
      *  -----------------------------------------------------------  * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S0500B' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
           MOVE LT-NO-ERROR-OCCURRED TO AS-APPL-PROC-STATUS.            WSHSER02
                                                                        WSHSER02
      *SPEC-BULL* COMMENT-ON=6       TRAITEMENT SPECIFIQUE BULL         WSHSER02
      * -> DE MDIX4002 : 6 LIGNES A METTRE EN COMMENTAIRE               WSHSER02
      *    PERFORM   AA000-LECTURE-PERF.                                WSHSER02
      *    IF PERF-WS-NBA1PO > 0                                        WSHSER02
      *       INITIALIZE PERF-CQOMC00                                   WSHSER02
      *       MOVE 'A' TO PERF-CCOD1C0                                  WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *SPEC-BULL* COMMENT-OFF=6      TRAITEMENT SPECIFIQUE BULL         WSHSER02
      *    IF PERF-TRT-OUI                                              WSHSER02
      *       MOVE SPACE TO PERF-CQOMC00                                WSHSER02
      *       MOVE "A"   TO PERF-CCOD1C0                                WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *    INITIALIZE DFHEIBLK.                                         WSHSER02
      *SPEC-LINUX* COMMENT-OFF=6                                        WSHSER02
      * -> DE MDIX4002 : 6 LIGNES A METTRE HORS COMMENTAIRE             WSHSER02
           IF PERF-TRT-OUI                                              WSHSER02
              MOVE SPACE TO PERF-CQOMC00                                WSHSER02
              MOVE "A"   TO PERF-CCOD1C0                                WSHSER02
              PERFORM AA001-APPEL-PERF                                  WSHSER02
           END-IF.                                                      WSHSER02
           INITIALIZE DFHEIBLK.                                         WSHSER02
                                                                        WSHSER02
           PERFORM A0600-HOUSEKEEPING.                                  WSHSER02
                                                                        WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S0500E' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
       S1000-PROCESS-REQUEST-ID.                                        WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *  S 1 0 0 0 - P R O C E S S - R E Q U E S T - I D              * WSHSER02
      *  -----------------------------------------------------------  * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S1000B' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
           PERFORM S3000-PROCESS-ROUTINE.                               WSHSER02
                                                                        WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S1000E' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
                                                                        WSHSER02
       S3000-PROCESS-ROUTINE.                                           WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *  S 3 0 0 0 - P R O C E S S - R O U T I N E                    * WSHSER02
      *  -----------------------------------------------------------  * WSHSER02
      *                                                               * WSHSER02
      *    THE PROCESS PARAGRAPH PERFORMS THE FOLLOWING FUNCTIONS:    * WSHSER02
      *                                                               * WSHSER02
      *            1)  DATA VALIDATION                                * WSHSER02
      *            2)  RELATIONAL VALIDATION                          * WSHSER02
      *            3)  PROCESS THE APPLICATION                        * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S3000B' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
           PERFORM A4000-DATA-VALIDATION.                               WSHSER02
                                                                        WSHSER02
           IF  AS-APPL-PROC-OK                                          WSHSER02
                                                                        WSHSER02
               PERFORM A4500-RELATIONAL-VALIDATION                      WSHSER02
                                                                        WSHSER02
           END-IF.                                                      WSHSER02
                                                                        WSHSER02
           IF  AS-APPL-PROC-OK                                          WSHSER02
                                                                        WSHSER02
               PERFORM A5000-PROCESS-DATA                               WSHSER02
                                                                        WSHSER02
           END-IF.                                                      WSHSER02
                                                                        WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S3000E' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
                                                                        WSHSER02
       S7200-LINK-IG.                                                   WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *  S 7 2 0 0 - L I N K - I G                                    * WSHSER02
      *  ------------------------------------------------------       * WSHSER02
      *                                                               * WSHSER02
      *    CE PARAGRAPHE PERMET L'APPEL A L'INTERFACE GENERALISEE     * WSHSER02
      *    QUI FAIT ELLE  MEME APPEL A L'I/O MODULE ADEQUAT.          * WSHSER02
      *                                                               * WSHSER02
      *    ENLEVER LES COMMENTAIRES DES QUE LA PRECOMPILATION CICS    * WSHSER02
      *    FONCTIONNERA                                               * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S7200B' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
      *SPEC-BULL* COMMENT-ON=6       TRAITEMENT SPECIFIQUE BULL         WSHSER02
      * -> DE MDIX4002 : 6 LIGNES A METTRE EN COMMENTAIRE               WSHSER02
      *    IF PERF-WS-NBA1PO > 0                                        WSHSER02
      *       INITIALIZE PERF-CQOMC00                                   WSHSER02
      *       MOVE 'I' TO PERF-CCOD1C0                                  WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *    CALL 'CDZZ0'       USING WS-IG-INTERFACE.                    WSHSER02
      *SPEC-BULL* COMMENT-OFF=9      TRAITEMENT SPECIFIQUE BULL         WSHSER02
      *    IF PERF-TRT-OUI                                              WSHSER02
      *       MOVE SPACE TO PERF-CQOMC00                                WSHSER02
      *       MOVE "I"   TO PERF-CCOD1C0                                WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *    MOVE  LENGTH OF WS-IG-INTERFACE  TO  CTIC-LGIGAD.            WSHSER02
      *    CALL "CDZZ0"        USING CTIC-CTIDCOM                       WSHSER02
      *                              WS-IG-INTERFACE                    WSHSER02
      *    END-CALL.                                                    WSHSER02
      *SPEC-LINUX* COMMENT-OFF=9                                        WSHSER02
      * -> DE MDIX4002 : 9 LIGNES A METTRE HORS COMMENTAIRE             WSHSER02
           IF PERF-TRT-OUI                                              WSHSER02
              MOVE SPACE TO PERF-CQOMC00                                WSHSER02
              MOVE "I"   TO PERF-CCOD1C0                                WSHSER02
              PERFORM AA001-APPEL-PERF                                  WSHSER02
           END-IF.                                                      WSHSER02
           MOVE  LENGTH OF WS-IG-INTERFACE  TO  CTIC-LGIGAD.            WSHSER02
           CALL "CDZZ0"        USING CTIC-CTIDCOM                       WSHSER02
                                     WS-IG-INTERFACE                    WSHSER02
           END-CALL.                                                    WSHSER02
                                                                        WSHSER02
                                                                        WSHSER02
      *SPEC-BULL* COMMENT-ON=5       TRAITEMENT SPECIFIQUE BULL         WSHSER02
      * -> DE MDIX4002 : 5 LIGNES A METTRE EN COMMENTAIRE               WSHSER02
      *    IF PERF-WS-NBA1PO > 0                                        WSHSER02
      *       INITIALIZE PERF-CQOMC00                                   WSHSER02
      *       MOVE 'J' TO PERF-CCOD1C0                                  WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *SPEC-BULL* COMMENT-OFF=5      TRAITEMENT SPECIFIQUE BULL         WSHSER02
      *    IF PERF-TRT-OUI                                              WSHSER02
      *       MOVE SPACE TO PERF-CQOMC00                                WSHSER02
      *       MOVE "J"   TO PERF-CCOD1C0                                WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *SPEC-LINUX* COMMENT-OFF=5                                        WSHSER02
      * -> DE MDIX4002 : 5 LIGNES A METTRE HORS COMMENTAIRE             WSHSER02
           IF PERF-TRT-OUI                                              WSHSER02
              MOVE SPACE TO PERF-CQOMC00                                WSHSER02
              MOVE "J"   TO PERF-CCOD1C0                                WSHSER02
              PERFORM AA001-APPEL-PERF                                  WSHSER02
           END-IF.                                                      WSHSER02
                                                                        WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S7200E' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
       S9500-WRAP-UP.                                                   WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *  S 9 5 0 0 - W R A P - U P                                    * WSHSER02
      *  -----------------------------------------------------------  * WSHSER02
      *                                                               * WSHSER02
      *    THIS PARAGRAPH PERFORMS THE APPLICATION WRAP UP PARAGRAPH  * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S9500B' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
           PERFORM A9600-WRAP-UP.                                       WSHSER02
                                                                        WSHSER02
      *SPEC-BULL* COMMENT-ON=5       TRAITEMENT SPECIFIQUE BULL         WSHSER02
      * -> DE MDIX4002 : 5 LIGNES A METTRE EN COMMENTAIRE               WSHSER02
      *    IF PERF-WS-NBA1PO > 0                                        WSHSER02
      *       INITIALIZE PERF-CQOMC00                                   WSHSER02
      *       MOVE 'F' TO PERF-CCOD1C0                                  WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
                                                                        WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S9500E' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
       S9800-EXIT-PROGRAM.                                              WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *  S 9 8 0 0 - E X I T - P R O G R A M                          * WSHSER02
      *  -----------------------------------------------------------  * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
      *SPEC-BULL* COMMENT-ON=5       TRAITEMENT SPECIFIQUE BULL         WSHSER02
      * -> DE MDIX4002 : 5 LIGNES A METTRE EN COMMENTAIRE               WSHSER02
      *    IF PERF-WS-NBA1PO > 0                                        WSHSER02
      *       INITIALIZE PERF-CQOMC00                                   WSHSER02
      *       MOVE 'X' TO PERF-CCOD1C0                                  WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *SPEC-BULL* COMMENT-OFF=5      TRAITEMENT SPECIFIQUE BULL         WSHSER02
      *    IF PERF-TRT-OUI                                              WSHSER02
      *       MOVE SPACE TO PERF-CQOMC00                                WSHSER02
      *       MOVE "X"   TO PERF-CCOD1C0                                WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *SPEC-LINUX* COMMENT-OFF=5                                        WSHSER02
      * -> DE MDIX4002 : 5 LIGNES A METTRE HORS COMMENTAIRE             WSHSER02
           IF PERF-TRT-OUI                                              WSHSER02
              MOVE SPACE TO PERF-CQOMC00                                WSHSER02
              MOVE "X"   TO PERF-CCOD1C0                                WSHSER02
              PERFORM AA001-APPEL-PERF                                  WSHSER02
           END-IF.                                                      WSHSER02
                                                                        WSHSER02
             EXIT PROGRAM.                                              WSHSER02
                                                                        WSHSER02
                                                                        WSHSER02
       S9900-HANDLE-PROCESSING-ERROR.                                   WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *  S 9 9 0 0 - H A N D L E - P R O C E S S I N G - E R R O R    * WSHSER02
      *  -----------------------------------------------------------  * WSHSER02
      *                                                               * WSHSER02
      *    THIS PARAGRAPH  UPDATES  THE CCR THAT AN ERROR OCCURRED    * WSHSER02
      *    AND STORES THE DFHEIBLK IN THE ABEND HANDLER.  IT THEN     * WSHSER02
      *    RETURNS CONTROL TO THE CCP.                                * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S9900B' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
      *                                                                 WSHSER02
      *    MOVE LT-APPL-REQUIRES-ABEND TO CCRA-APPL-RETURN-CODE.        WSHSER02
      *    MOVE DFHEIBLK TO ABHI-EIB-DATA-BLOCK.                        WSHSER02
      *                                                                 WSHSER02
      *    EXEC CICS RETURN                                             WSHSER02
      *    END-EXEC.                                                    WSHSER02
                                                                        WSHSER02
      *SPEC-BULL* COMMENT-ON=5       TRAITEMENT SPECIFIQUE BULL         WSHSER02
      * -> DE MDIX4002 : 5 LIGNES A METTRE EN COMMENTAIRE               WSHSER02
      *    IF PERF-WS-NBA1PO > 0                                        WSHSER02
      *       INITIALIZE PERF-CQOMC00                                   WSHSER02
      *       MOVE 'E' TO PERF-CCOD1C0                                  WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *SPEC-BULL* COMMENT-OFF=5      TRAITEMENT SPECIFIQUE BULL         WSHSER02
      *    IF PERF-TRT-OUI                                              WSHSER02
      *       MOVE SPACE TO PERF-CQOMC00                                WSHSER02
      *       MOVE "E"   TO PERF-CCOD1C0                                WSHSER02
      *       PERFORM AA001-APPEL-PERF                                  WSHSER02
      *    END-IF.                                                      WSHSER02
      *SPEC-LINUX* COMMENT-OFF=5                                        WSHSER02
      * -> DE MDIX4002 : 5 LIGNES A METTRE HORS COMMENTAIRE             WSHSER02
           IF PERF-TRT-OUI                                              WSHSER02
              MOVE SPACE TO PERF-CQOMC00                                WSHSER02
              MOVE "E"   TO PERF-CCOD1C0                                WSHSER02
              PERFORM AA001-APPEL-PERF                                  WSHSER02
           END-IF.                                                      WSHSER02
                                                                        WSHSER02
      ***************************************************************** WSHSER02
      *    MOVE 'S9900E' TO DB-LAST-PARAGRAPH-EXECUTED.                 WSHSER02
      *    PERFORM SDDDD-PROGRAM-DEBUG.                                 WSHSER02
      ***************************************************************** WSHSER02
                                                                        WSHSER02
       SDDDD-PROGRAM-DEBUG.                                             WSHSER02
      ***************************************************************** WSHSER02
      *                                                               * WSHSER02
      *  S D D D D - P R O G R A M - D E B U G                        * WSHSER02
      *  -----------------------------------------------------------  * WSHSER02
      *                                                               * WSHSER02
      *    THIS PARAGRAPH ESTABLISHES A TRACE OF  ALL  THE  PARA-     * WSHSER02
      *    GRAPHS EXECUTED SIMILAR TO  THE  COBOL  "READY  TRACE"     * WSHSER02
      *    FUNCTION.   THE  NUMBER  OF  EACH  PARAGRAPH  THAT  IS     * WSHSER02
      *    PERFORMED IS MOVED TO  A  TABLE  IN  WORKING  STORAGE.     * WSHSER02
      *    THIS TABLE CAN BE VIEWED ON-LINE USING THE  PWB  DEBUG     * WSHSER02
      *    FACILITY  OR  EDF  TO  DETERMINE  THE  LAST  PARAGRAPH     * WSHSER02
      *    EXECUTED.  NOTE THE TRACE PARAGRAPH TABLE  IS  CLEARED     * WSHSER02
      *    WHEN THE END IS REACHED.                                   * WSHSER02
      *                                                               * WSHSER02
      ***************************************************************** WSHSER02
      *                                                                 WSHSER02
      *     MOVE DB-LAST-PARAGRAPH-EXECUTED TO                          WSHSER02
      *         DB-TRACE-PARAGRAPH-ENTRY (DB-TRACE-INDEX).              WSHSER02
      *                                                                 WSHSER02
      *     ADD LT-BINARY-ONE TO DB-TRACE-INDEX.                        WSHSER02
      *                                                                 WSHSER02
      *     IF  DB-TRACE-INDEX GREATER DB-MAX-TRACE-TABLE-ENTRIES       WSHSER02
      *                                                                 WSHSER02
      *         MOVE LOW-VALUES TO DB-TRACE-PARAGRAPH-TABLE             WSHSER02
      *         MOVE LT-BINARY-ONE TO DB-TRACE-INDEX                    WSHSER02
      *         ADD LT-BINARY-ONE TO DB-RESET-COUNT.                    WSHSER02
      *---------------------------------------------------------------  WSHSER02
      *                Version : V02100                                 WSHSER02
      *                                                                 WSHSER02
      * Chantier : 1651T     Début : 22/02/2007     Fin : 22/02/2007    WSHSER02
      *                                                                 WSHSER02
      * Texte  : Traitement Specifique Bull                             WSHSER02
      *                                                                 WSHSER02
      *                                                                 WSHSER02
      *                                                                 WSHSER02
      * DMFS  :                                                         WSHSER02
      *       :                                                         WSHSER02
      *---------------------------------------------------------------  WSHSER02
      *END_OF_COPY_FILE                                                 WSHSER02
                                                                        
      *COPY CQOMCSAK.                                                   
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copyb/CQOMCSAK.CPY"  CQOMCSAK
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CQOMCSAK
      *LibFac  liberFactory GCOS to Linux Translator  *                 CQOMCSAK
      *LibFac  From input file: CQOMCSAK              *                 CQOMCSAK
      *LibFac  On   the   date: 2015/03/24            *                 CQOMCSAK
      * COMPOSANT TRAITE PAR ConstruireSpx.pl                           CQOMCSAK
      *LibFac * * * * * * * * * * * * * * * * * * * * *                 CQOMCSAK
      * PERF CQOMCSAK 15.06.94                                          CQOMCSAK
      *                                                                 CQOMCSAK
      * APPEL AU MODULE DE MESURE DE CONSOMMATION (SQUELETTE SERVICE)   CQOMCSAK
      *                                                                 CQOMCSAK
       AA001-APPEL-PERF.                                                CQOMCSAK
           MOVE "B"                TO PERF-ZTYPAP.                      CQOMCSAK
           MOVE "1"                TO PERF-CCOD1C.                      CQOMCSAK
           MOVE "SERVICE"          TO PERF-PTYENT.                      CQOMCSAK
           MOVE "WSHSER01"         TO PERF-PIDENT.                      CQOMCSAK
           MOVE LT-NAME-OF-THIS-PROGRAM TO PERF-PIDMOD.                 CQOMCSAK
           CALL "CQOMCM" USING PERF-CQOMC00.                            CQOMCSAK
           if perf-trt-retour = "N"                                     CQOMCSAK
              set perf-trt-non     to true                              CQOMCSAK
           end-if.                                                      CQOMCSAK
       AA001-APPEL-PERF-FIN.                                            CQOMCSAK
           EXIT.                                                        CQOMCSAK
      *************************************************************     CQOMCSAK
      *END_OF_COPY_FILE                                                 CQOMCSAK
                                                                        
      *COPY &PRGRMID.A.                                                 
      *COPY CGS100A.                                                    
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copy/CGS100A.CPY"    CGS100A
      ***************************************************************** CGS100A
      * T235M.CGS100A                                                 * CGS100A
      *                                                               * CGS100A
      *                                                               * CGS100A
      ***************************************************************** CGS100A
      /                                                                 CGS100A
       A0600-HOUSEKEEPING.                                              CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *  A 0 6 0 0 - H O U S E K E E P I N G                          * CGS100A
      *  -----------------------------------------------------------  * CGS100A
      *                                                               * CGS100A
      *  INITIALISATION DE LA WORKING STORAGE APPLICATIVE.            * CGS100A
      *  PRISE EN COMPTE DES DONNEES DU MESSSAGE EN ENTREE.           * CGS100A
      *-A0x-                                                          * CGS100A
      ***************************************************************** CGS100A
      D    display '(CGS100A)-A00- A0600-HOUSEKEEPING '                 CGS100A
      D    display '(CGS100A)-A01-NOUV-DOSSIER-A-TRAITER '              CGS100A
      D    display '(CGS100A) ****************************************' CGS100A
      D    display '(CGS100A) *    Début copie recue du client       *' CGS100A
      D    display '(CGS100A) ****************************************' CGS100A
      d    display '(CGS100A)-A02- S100-ESRV-LGSERV  : '                CGS100A
      d       s100-esrv-lgserv                                          CGS100A
      D                     ' S100-ESRV-PISRVC  : ' S100-ESRV-PISRVC    CGS100A
      D    display '(CGS100A)-A03- S100-ESRV-CRTSRV  : '                CGS100A
      d       s100-esrv-crtsrv                                          CGS100A
      D                     '    S100-ESRV-PINTRV  : ' S100-ESRV-PINTRV CGS100A
      D    display '(CGS100N) S100-ESRV-LGPFIX  : ' S100-ESRV-LGPFIX    CGS100A
      D                     '   S100-ESRV-LOCCUR  : ' S100-ESRV-LOCCUR  CGS100A
      D    display '(CGS100N) S100-ESRV-NOCUTI  : ' S100-ESRV-NOCUTI    CGS100A
      D                   '   S100-ESRV-NTTOCU  : ' S100-ESRV-NTTOCU    CGS100A
      D    display '(CGS100N) S100-ESRV-IDORGA  : ' S100-ESRV-IDORGA    CGS100A
      D                     ' S100-ESRV-APPLEXT : ' S100-ESRV-APPLEXT   CGS100A
      D    display '(CGS100N) S100-ESRV-PVRSER  : ' S100-ESRV-PVRSER    CGS100A
      D                   '   S100-PIOCDE       : ' S100-PIOCDE         CGS100A
      D    display '(CGS100N) S100-ZTYPAP       : ' S100-ZTYPAP         CGS100A
      D                 '     S100-ZDSVCM       : ' S100-ZDSVCM         CGS100A
      D    display '(CGS100N) S100-ZFONCT       : ' S100-ZFONCT         CGS100A
      D                   '   S100-CRTSRV       : ' S100-CRTSRV         CGS100A
      D    display '(CGS100N) S100-CABEND       : ' S100-CABEND         CGS100A
      D                    '  S100-TABEND       : ' S100-TABEND         CGS100A
      D    display '(CGS100N) S100-ZAPPLT       : ' S100-ZAPPLT         CGS100A
      D                    '  S100-NDOMAT       : ' S100-NDOMAT         CGS100A
      D    display '(CGS100N) S100-CORGTR       : ' S100-CORGTR         CGS100A
      D    display '(CGS100N) S100-ESESSI       : ' S100-ESESSI         CGS100A
      D                     ' S100-HSESSI       : ' S100-HSESSI         CGS100A
      D    display '(CGS100N) S100-ELIQUI       : ' S100-ELIQUI         CGS100A
      D                     ' S100-HLIQUI       : ' S100-HLIQUI         CGS100A
      D    display '(CGS100N) S100-EPTAEX       : ' S100-EPTAEX         CGS100A
      D                     ' S100-FPTAEX       : ' S100-FPTAEX         CGS100A
      D    display '(CGS100A) ***** Fin  copie recue du client  ******' CGS100A
      D       '*******************************'                         CGS100A
                                                                        CGS100A
      *--- Alerte PMVT à zéro !!!!!!!!!!                                CGS100A
      D    IF S100-EPTAEX = ZERO                                        CGS100A
      D       Display '(CGS100A) S100-ZAPPLT:' S100-ZAPPLT              CGS100A
      D                       '  S100-NDOMAT:' S100-NDOMAT              CGS100A
      D       Display '(CGS100A) S100-EPTAEX:' S100-EPTAEX              CGS100A
      D                       '  S100-FPTAEX:' S100-FPTAEX              CGS100A
      D       DISPLAY '(CGS100A) !!!  DATE DEBUT PMVT A ZERO  !!!!!!!!' CGS100A
      D               '   !!!!!!!   !!!!!!   !!!!!   '                  CGS100A
      D    END-IF.                                                      CGS100A
                                                                        CGS100A
      D    IF S100-FPTAEX = ZERO                                        CGS100A
      D       Display '(CGS100A) S100-ZAPPLT:' S100-ZAPPLT              CGS100A
      D                       '  S100-NDOMAT:' S100-NDOMAT              CGS100A
      D       Display '(CGS100A) S100-EPTAEX:' S100-EPTAEX              CGS100A
      D                        ' S100-FPTAEX:' S100-FPTAEX              CGS100A
      D       DISPLAY '(CGS100A) !!!  DATE FIN PMVT A ZERO    !!!!!!!!' CGS100A
      D               '   !!!!!!!   !!!!!!   !!!!!   '                  CGS100A
      D    END-IF.                                                      CGS100A
                                                                        CGS100A
      D    IF S100-FPTAEX = S100-EPTAEX                                 CGS100A
      D       Display '(CGS100A) S100-ZAPPLT:' S100-ZAPPLT              CGS100A
      D                       '  S100-NDOMAT:' S100-NDOMAT              CGS100A
      D       Display '(CGS100A) S100-EPTAEX:' S100-EPTAEX              CGS100A
      D                        ' S100-FPTAEX:' S100-FPTAEX              CGS100A
      D       DISPLAY '(CGS100A) !!!  DT FIN PMVT = DT DEB PMVT  !!!!!' CGS100A
      D               '   !!!!!!!   !!!!!!   !!!!!   '                  CGS100A
      D    END-IF.                                                      CGS100A
      *--- ------------------ !!!!!!!!!!                                CGS100A
                                                                        CGS100A
                                                                        CGS100A
      *    IF S100-CORGTR = 'S1'                                        CGS100A
      *       PERFORM A5500-CALL-CGS102                                 CGS100A
      *    ELSE                                                         CGS100A
              SET AZIS-FL-RG-PMVT-1 TO TRUE                             CGS100A
      *---    Initialisation LS-TAB-PMVT-2                              CGS100A
              MOVE S100-EPTAEX TO AZIS-PMVT-2-MOIS (1)                  CGS100A
              SET AZIS-FL-PMVT-2-PAS-A-FAIRE (1) TO TRUE                CGS100A
              MOVE S100-EPTAEX TO WS-DATE(2)                            CGS100A
              MOVE S100-FPTAEX TO WS-DATE(3)                            CGS100A
      d       display '(CGS100A)-A0L- Date de deb : ' WS-DTMOIS(2)      CGS100A
      d          ' AAAAMM de S100-EPTAEX '                              CGS100A
      d       display '(CGS100A)-A0M- Date de fin : ' WS-DTMOIS(3)      CGS100A
      d          ' AAAAMM de S100-FPTAEX '                              CGS100A
      *       display '(CGS100A) PM: ' S100-EPTAEX '-' S100-FPTAEX      CGS100A
              PERFORM VARYING IX-AZIS-PMVT-2 FROM 1 BY 1                CGS100A
                 UNTIL IX-AZIS-PMVT-2 > LT-IX-AZIS-PMVT-2-MAX           CGS100A
                 OR    WS-DTMOIS(2) = WS-DTMOIS(3)                      CGS100A
                 MOVE AZIS-PMVT-2-MOIS (IX-AZIS-PMVT-2) TO WS-DATE(2)   CGS100A
                 ADD 1 TO WS-DTMM(2)                                    CGS100A
                 IF WS-DTMM(2) > 12                                     CGS100A
                    MOVE 1 TO WS-DTMM(2)                                CGS100A
                    ADD  1 TO WS-DTAN(2)                                CGS100A
                 END-IF                                                 CGS100A
                 IF IX-AZIS-PMVT-2 < 25                                 CGS100A
                    SET IX-AZIS-PMVT-2 UP BY 1                          CGS100A
                    MOVE WS-DATE(2) TO AZIS-PMVT-2-MOIS(IX-AZIS-PMVT-2) CGS100A
                    SET AZIS-FL-PMVT-2-PAS-A-FAIRE(IX-AZIS-PMVT-2)      CGS100A
                                                             TO TRUE    CGS100A
                    SET IX-AZIS-PMVT-2 DOWN BY 1                        CGS100A
                 END-IF                                                 CGS100A
              END-PERFORM                                               CGS100A
      D       PERFORM A9900-DISPLAY-TAB-PMVT-2                          CGS100A
                                                                        CGS100A
              PERFORM A9900-TRT-PMVT                                    CGS100A
                                                                        CGS100A
      D       PERFORM A9900-DISPLAY-TAB-PMVT-2                          CGS100A
              PERFORM VARYING IX-AZIS-PMVT-2 FROM 1 BY 1                CGS100A
                 UNTIL IX-AZIS-PMVT-2 > LT-IX-AZIS-PMVT-2-MAX           CGS100A
                    OR AZIS-FL-PMVT-2-A-FAIRE(IX-AZIS-PMVT-2)           CGS100A
                 CONTINUE                                               CGS100A
              END-PERFORM                                               CGS100A
                                                                        CGS100A
              IF IX-AZIS-PMVT-2 NOT > LT-IX-AZIS-PMVT-2-MAX             CGS100A
              IF AZIS-FL-PMVT-2-A-FAIRE(IX-AZIS-PMVT-2)                 CGS100A
                 IF  S100-CABEND = SPACES                               CGS100A
                 AND S100-TABEND = SPACES                               CGS100A
      d             display '(CGS100A)-A0N- BIS PASSAGE 2ieme PMVT'     CGS100A
                    SET AZIS-FL-RG-PMVT-2 TO TRUE                       CGS100A
                    PERFORM A9900-TRT-PMVT                              CGS100A
      D          ELSE                                                   CGS100A
      d             DISPLAY '(CGS100A)-A0P- BIS Retour premiere PMVT'   CGS100A
      D                     ' INcorrect'                                CGS100A
      D             DISPLAY '(CGS100N) BIS   ==> pas de deuxieme PMVT'  CGS100A
                 END-IF                                                 CGS100A
              END-IF                                                    CGS100A
              END-IF                                                    CGS100A
      D       DISPLAY 'CGS100 ON EST LA 2'                              CGS100A
              PERFORM A9900-RETOUR-CLIENT.                              CGS100A
      *    END-IF.                                                      CGS100A
                                                                        CGS100A
       A9900-TRT-PMVT.                                                  CGS100A
      ***************************************************************** CGS100A
      * Traitement de la PMVT                                         * CGS100A
      *-A1x-                                                            CGS100A
      ***************************************************************** CGS100A
           IF S100-EPTAEX >= 20020101                                   CGS100A
           OR S100-FPTAEX <= 20011231                                   CGS100A
             PERFORM A9900-APPEL-CGS100                                 CGS100A
           ELSE                                                         CGS100A
             PERFORM A9900-TRAIT-PMVT-A-CHEVAL                          CGS100A
           END-IF.                                                      CGS100A
                                                                        CGS100A
       A9900-APPEL-CGS100.                                              CGS100A
      ***************************************************************** CGS100A
      * Appel pilote PEL                                              * CGS100A
      *-A2x-                                                            CGS100A
      ***************************************************************** CGS100A
      D    display '(CGS100A)-A20- A9900-APPEL-CGS100 '                 CGS100A
      D    display '(CGS100A) ****************************************' CGS100A
      D    display '(CGS100A) *     Appel CGS101                     *' CGS100A
      D    display '(CGS100A) ****************************************' CGS100A
      D    display '(CGS100A)-A21- Periode mouvementee : ' s100-eptaex  CGS100A
      D                                         ' - ' S100-FPTAEX.      CGS100A
      D    display '(CGS100A) ****************************************' CGS100A
      D    display '(CGS100A)-A22- Début copie transmis au CGS101    *' CGS100A
      D    display '(CGS100A) ****************************************' CGS100A
      D    display '(CGS100A)-A23- S100-ESRV-LGSERV  : '                CGS100A
      D       s100-esrv-lgserv                                          CGS100A
      D    display '(CGS100N) S100-ESRV-PISRVC  : ' S100-ESRV-PISRVC    CGS100A
      D    display '(CGS100N) S100-ESRV-CRTSRV  : ' S100-ESRV-CRTSRV    CGS100A
      D    display '(CGS100N) S100-ESRV-PINTRV  : ' S100-ESRV-PINTRV    CGS100A
      D    display '(CGS100N) S100-ESRV-LGPFIX  : ' S100-ESRV-LGPFIX    CGS100A
      D    display '(CGS100N) S100-ESRV-LOCCUR  : ' S100-ESRV-LOCCUR    CGS100A
      D    display '(CGS100N) S100-ESRV-NOCUTI  : ' S100-ESRV-NOCUTI    CGS100A
      D    display '(CGS100N) S100-ESRV-NTTOCU  : ' S100-ESRV-NTTOCU    CGS100A
      D    display '(CGS100N) S100-ESRV-IDORGA  : ' S100-ESRV-IDORGA    CGS100A
      D    display '(CGS100N) S100-ESRV-APPLEXT : ' S100-ESRV-APPLEXT   CGS100A
      D    display '(CGS100N) S100-ESRV-PVRSER  : ' S100-ESRV-PVRSER    CGS100A
      D    display '(CGS100N) S100-PIOCDE       : ' S100-PIOCDE         CGS100A
      D    display '(CGS100N) S100-ZTYPAP       : ' S100-ZTYPAP         CGS100A
      D    display '(CGS100N) S100-ZDSVCM       : ' S100-ZDSVCM         CGS100A
      D    display '(CGS100N) S100-ZFONCT       : ' S100-ZFONCT         CGS100A
      D    display '(CGS100N) S100-CRTSRV       : ' S100-CRTSRV         CGS100A
      D    display '(CGS100N) S100-CABEND       : ' S100-CABEND         CGS100A
      D    display '(CGS100N) S100-TABEND       : ' S100-TABEND         CGS100A
      D    display '(CGS100N) S100-ZAPPLT       : ' S100-ZAPPLT         CGS100A
      D    display '(CGS100N) S100-NDOMAT       : ' S100-NDOMAT         CGS100A
      D    display '(CGS100N) S100-CORGTR       : ' S100-CORGTR         CGS100A
      D    display '(CGS100N) S100-ESESSI       : ' S100-ESESSI         CGS100A
      D    display '(CGS100N) S100-HSESSI       : ' S100-HSESSI         CGS100A
      D    display '(CGS100N) S100-ELIQUI       : ' S100-ELIQUI         CGS100A
      D    display '(CGS100N) S100-HLIQUI       : ' S100-HLIQUI         CGS100A
      D    display '(CGS100N) S100-EPTAEX       : ' S100-EPTAEX         CGS100A
      D    display '(CGS100N) S100-FPTAEX       : ' S100-FPTAEX         CGS100A
      D    display '(CGS100N) ****************************************' CGS100A
      D    display '(CGS100A)-A2W-  Fin  copie transmis au CGS101    *' CGS100A
      D    display '(CGS100N) ****************************************' CGS100A
                                                                        CGS100A
           PERFORM A5500-CALL-CGS101.                                   CGS100A
                                                                        CGS100A
      D    display '(CGS100A) ****************************************' CGS100A
      D    display '(CGS100A)-A2Y-  Retour CGS101                    *' CGS100A
      D    display '(CGS100A) ****************************************'.CGS100A
                                                                        CGS100A
                                                                        CGS100A
       A9900-RETOUR-CLIENT.                                             CGS100A
      ***************************************************************** CGS100A
      * Rend la main au client                                        * CGS100A
      *-A3x-                                                            CGS100A
      ***************************************************************** CGS100A
      d    display '(CGS100A)-A30- A9900-RETOUR-CLIENT '                CGS100A
      d    display '(CGS100A)-A31- *** Rend la main au client  *****'   CGS100A
      d       '***********************************'                     CGS100A
           PERFORM S9800-EXIT-PROGRAM.                                  CGS100A
                                                                        CGS100A
       A9900-TRAIT-PMVT-A-CHEVAL.                                       CGS100A
      ***************************************************************** CGS100A
      *        A9900-TRAIT-PMVT-A-CHEVAL.                             * CGS100A
      *-A4x-                                                            CGS100A
      ***************************************************************** CGS100A
           MOVE S100-EPTAEX    TO WS-DD-PMVT-ORI                        CGS100A
           MOVE S100-FPTAEX    TO WS-DF-PMVT-ORI                        CGS100A
           MOVE WS-DD-PMVT-ORI TO WS-DD-PMVT-1                          CGS100A
           MOVE 20011231       TO WS-DF-PMVT-1                          CGS100A
           MOVE 20020101       TO WS-DD-PMVT-2                          CGS100A
           MOVE WS-DF-PMVT-ORI TO WS-DF-PMVT-2                          CGS100A
      d    display '(CGS100A)-A41- Eclatement de la PMVT '              CGS100A
      d       ws-dd-pmvt-ori                                            CGS100A
      D                                         ' - ' WS-DF-PMVT-ORI    CGS100A
      D    DISPLAY '(CGS100N) en ' WS-DD-PMVT-1 ' - ' WS-DF-PMVT-1      CGS100A
      D                     ' et ' WS-DD-PMVT-2 ' - ' WS-DF-PMVT-2      CGS100A
      D    DISPLAY '(CGS100N) Premier appel'                            CGS100A
           MOVE WS-DD-PMVT-1 TO S100-EPTAEX                             CGS100A
           MOVE WS-DF-PMVT-1 TO S100-FPTAEX                             CGS100A
      *                                                                 CGS100A
      *--- Envoi info a AZIS -------*                                   CGS100A
           MOVE 'DEU' TO S100-ZFONCT                                    CGS100A
                                                                        CGS100A
           PERFORM A9900-APPEL-CGS100                                   CGS100A
           IF  S100-CABEND = SPACES                                     CGS100A
           AND S100-TABEND = SPACES                                     CGS100A
      d       display '(CGS100A)-A44- Retour premier appel correct'     CGS100A
      D       DISPLAY '(CGS100N)    ==> deuxieme appel'                 CGS100A
              MOVE WS-DD-PMVT-2 TO S100-EPTAEX                          CGS100A
              MOVE WS-DF-PMVT-2 TO S100-FPTAEX                          CGS100A
      *                                                                 CGS100A
      *---    Envoi info a AZIS -------*                                CGS100A
              MOVE 'DEU' TO S100-ZFONCT                                 CGS100A
      *                                                                 CGS100A
              PERFORM A9900-APPEL-CGS100                                CGS100A
      D    ELSE                                                         CGS100A
      D       DISPLAY '(CGS100N) Retour premier appel INcorrect'        CGS100A
      D       DISPLAY '(CGS100N)    ==> pas de deuxieme appel'          CGS100A
           END-IF.                                                      CGS100A
      *                                                                 CGS100A
      *    Stockage des dates en cas de Bis Passage 2ème PMVT           CGS100A
           MOVE WS-DD-PMVT-ORI TO S100-EPTAEX.                          CGS100A
           MOVE WS-DF-PMVT-ORI TO S100-FPTAEX.                          CGS100A
                                                                        CGS100A
      DA9900-DISPLAY-TAB-PMVT-2.                                        CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *-A5x-                                                            CGS100A
      ***************************************************************** CGS100A
      D    PERFORM VARYING IX-AZIS-PMVT-2 FROM 1 BY 1                   CGS100A
      D       UNTIL IX-AZIS-PMVT-2 > LT-IX-AZIS-PMVT-2-MAX              CGS100A
      D       SET WS-DTAN(1) TO IX-AZIS-PMVT-2                          CGS100A
      D       display '(CGS100A)-A51- LIGNE PMVT (' WS-DTAN(1)          CGS100A
      D               ') '  AZIS-PMVT-2-MOIS(IX-AZIS-PMVT-2)            CGS100A
      D       DISPLAY ' - ' AZIS-PMVT-2-A-FAIRE(IX-AZIS-PMVT-2)         CGS100A
      D    END-PERFORM.                                                 CGS100A
      /                                                                 CGS100A
       A4000-DATA-VALIDATION.                                           CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *  A 4 0 0 0 - D A T A - V A L I D A T I O N                    * CGS100A
      *  -----------------------------------------------------------  * CGS100A
      *                                                               * CGS100A
      *  VALIDATIONS ELEMENTAIRES.                                    * CGS100A
      *  VALIDATION INTER-DONNEES (SI NECESSAIRE)                     * CGS100A
      *                                                               * CGS100A
      ***************************************************************** CGS100A
                                                                        CGS100A
                                                                        CGS100A
      /                                                                 CGS100A
       A4500-RELATIONAL-VALIDATION.                                     CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *  A 4 5 0 0 - R E L A T I O N - V A L I D A T I O N            * CGS100A
      *  -----------------------------------------------------------  * CGS100A
      *                                                               * CGS100A
      *  NON APPLICABLE ICI                                           * CGS100A
      *                                                               * CGS100A
      ***************************************************************** CGS100A
                                                                        CGS100A
      /                                                                 CGS100A
       A5000-PROCESS-DATA.                                              CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *  A 5 0 0 0 - P R O C E S S - D A T A                          * CGS100A
      *  -----------------------------------------------------------  * CGS100A
      *                                                               * CGS100A
      *  CE PARAGRAPHE PERMET DE TRAITER TOUTES LES DONNEES           * CGS100A
      *           - PARTIE FIXE ET OCCURENCES EVENTUELLES             * CGS100A
      *                                                               * CGS100A
      ***************************************************************** CGS100A
                                                                        CGS100A
                                                                        CGS100A
      /                                                                 CGS100A
      *A5100-PROCESS-PFIXE.                                             CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *  A 5 1 0 0 - P R O C E S S - P F I X E                        * CGS100A
      *  -----------------------------------------------------------  * CGS100A
      *                                                               * CGS100A
      *  CE PARAGRAPHE PERMET LE TRAITEMENT EVENTUEL DE LA PARTIE FIXE* CGS100A
      *           - CALCULS DIVERS ENTRE DONNEES                      * CGS100A
      *           - APPEL INTERFACE GENERALISEE                       * CGS100A
      *      CF A4500-RELATIONAL-VALIDATION                           * CGS100A
      ***************************************************************** CGS100A
                                                                        CGS100A
      *d   display '(CGS100N) A5100-PROCESS-PFIXE du nouveau pilote'.   CGS100A
      /                                                                 CGS100A
      *A6100-CHARGEMENT-PFIXE.                                          CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *  A 6 1 0 0 - C H A R G E M E N T - P F I X E                  * CGS100A
      *  -----------------------------------------------------------  * CGS100A
      *                                                               * CGS100A
      *  CE PARAGRAPHE PERMET DE CHARGER DANS LA ZONE DE COMM.        * CGS100A
      *  EN RETOUR LES DIFFERENTES VALEURS DE LA PARTIE FIXE          * CGS100A
      *                                                               * CGS100A
      ***************************************************************** CGS100A
                                                                        CGS100A
      *d   display '(CGS100N) A6100-CHARGEMENT-PFIXE nouveau pilote'.   CGS100A
      *                                                                 CGS100A
                                                                        CGS100A
      *A7100-PROCESS-COMPUTE-PFS.                                       CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *  A 7 1 0 0 - P R O C E S S - C O M P U T E - P F S            * CGS100A
      *  -----------------------------------------------------------  * CGS100A
      *                                                               * CGS100A
      *  PREPARATION DES DONNEES EN MEMOIRE                           * CGS100A
      ***************************************************************** CGS100A
      *d   display '(CGS100N) A7100-PROCESS-COMPUTE-PFS ouveau pilote'. CGS100A
                                                                        CGS100A
      /                                                                 CGS100A
      *A7110-PROCESS-COMPUTE-PFS.                                       CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *  A 7 1 1 0 - P R O C E S S - C O M P U T E - P F S            * CGS100A
      *  -----------------------------------------------------------  * CGS100A
      *                                                               * CGS100A
      *     TRAITEMENT PRINCIPAL                                      * CGS100A
      ***************************************************************** CGS100A
                                                                        CGS100A
                                                                        CGS100A
       A9600-WRAP-UP.                                                   CGS100A
      ***************************************************************** CGS100A
      *                                                               * CGS100A
      *  A 9 6 0 0 - W R A P - U P                                    * CGS100A
      ***************************************************************** CGS100A
      D    display '(CGS100N) A9600-WRAP-UP du nouveau pilote'.         CGS100A
                                                                        CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V00720                                 CGS100A
      *                                                                 CGS100A
      * Chantier : D3918     Début : 08/06/2001     Fin : 08/06/2001    CGS100A
      *                                                                 CGS100A
      * Texte  : Mise en EXPLOSION systématique d'AZIS lorsque la Per. MCGS100A
      *          de GESDRO est "à cheval" sur 01/2002 et donc Examen/DroCGS100A
      *          fait en 2 passages dans CGS100 posant pb entre AZIS/CASCGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V00720                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 0904T     Début : 08/06/2001     Fin : 08/06/2001    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V00920                                 CGS100A
      *                                                                 CGS100A
      * Chantier : AQ015     Début : 20/03/2002     Fin : 21/03/2002    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V01010                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 0590B     Début : 21/10/2002     Fin : 18/11/2002    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V01110                                 CGS100A
      *                                                                 CGS100A
      * Chantier : DA609     Début : 21/02/2003     Fin : 21/02/2003    CGS100A
      *                                                                 CGS100A
      * Texte  : Lorsqu'il y a, sur la période mouvementée, deux changemCGS100A
      *          de logement, on doit bien repasser, une deuxième fois, CGS100A
      *          sur les périodes englobant ces deux logements.         CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V01400                                 CGS100A
      *                                                                 CGS100A
      * Chantier : AQ015     Début : 10/03/2004     Fin : 10/03/2004    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V01500                                 CGS100A
      *                                                                 CGS100A
      * Chantier : AQ015     Début : 20/07/2004     Fin : 20/07/2004    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V01500                                 CGS100A
      *                                                                 CGS100A
      * Chantier : AQ050     Début : 11/05/2004     Fin : 17/05/2004    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V02500                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 1R250     Début : 04/08/2008     Fin : 04/08/2008    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V03320                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 2050A     Début : 29/12/2010     Fin : 29/12/2010    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V03400                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 2050C     Début : 16/02/2011     Fin : 23/02/2011    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V03410                                 CGS100A
      *                                                                 CGS100A
      * Chantier : E4766     Début : 19/04/2011     Fin : 22/04/2011    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V03410                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 2050E     Début : 12/05/2011     Fin : 12/05/2011    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V03510                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 2050L     Début : 26/09/2011     Fin : 26/09/2011    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V03600                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 2152A     Début : 25/01/2012     Fin : 25/01/2012    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V03700                                 CGS100A
      *                                                                 CGS100A
      * Chantier : F9560     Début : 18/04/2012     Fin : 18/04/2012    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V03900                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 1R390     Début : 21/01/2013     Fin : 21/01/2013    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V04000                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 2238I     Début : 14/05/2013     Fin : 14/05/2013    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V04401                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 1R440     Début : 05/11/2014     Fin : 05/11/2014    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *---------------------------------------------------------------  CGS100A
      *                Version : V05100                                 CGS100A
      *                                                                 CGS100A
      * Chantier : 1R510     Début : 14/03/2017     Fin : 14/03/2017    CGS100A
      *                                                                 CGS100A
      * Texte  :                                                        CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      *                                                                 CGS100A
      * DMFS  :                                                         CGS100A
      *       :                                                         CGS100A
      *---------------------------------------------------------------  CGS100A
      *END_OF_COPY_FILE                                                 CGS100A
                                                                        
      *********  BEGIN PROCEDURAL COPYBOOKS *******                     
      *    COPY  CGS101AK.                                              
      *COPY"/distrib/appli/cristal/05200/ref/cobol/copyb/CGS101AK.CPY"  CGS101AK
      ***************************************************************** CGS101AK
      * CGS101AK                                                      * CGS101AK
      *  T238M.CGS101AK                                               * CGS101AK
      *             ---------------------------------------           * CGS101AK
      *                                                               * CGS101AK
      *  PROCEDURE AK POUR L'APPEL AU SERVICE CGS101  (BATCH)         * CGS101AK
      *                                                               * CGS101AK
      *                                                               * CGS101AK
      ***************************************************************** CGS101AK
      *                                                                 CGS101AK
       A5500-CALL-CGS101.                                               CGS101AK
           CALL 'CGS101' USING S100-CGS100R WAZIS.                      CGS101AK
      *                                                                 CGS101AK
      *                                                                 CGS101AK
      *---------------------------------------------------------------  CGS101AK
      *                Version : V00720                                 CGS101AK
      *                                                                 CGS101AK
      * Chantier : 0904T     Début : 21/03/2001     Fin : 22/03/2001    CGS101AK
      *                                                                 CGS101AK
      * Texte  :                                                        CGS101AK
      *                                                                 CGS101AK
      *                                                                 CGS101AK
      *                                                                 CGS101AK
      * DMFS  :                                                         CGS101AK
      *       :                                                         CGS101AK
      *---------------------------------------------------------------  CGS101AK
      *END_OF_COPY_FILE                                                 CGS101AK
      **********  END PROCEDURAL COPYBOOKS ********                     
