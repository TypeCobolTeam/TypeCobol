000100*----------------------------------------------------------------*00010000
000110* MSVCOUT                                                        *00020001
000150*----------------------------------------------------------------*00060001
000160 01  :MSVCOUT:.                                                   00070001
000170                                                                  00080001
000180*----------------------------------------------------------------*00090001
000280     05  .                                                        00190001
000290*[Dsc] Identifiant de cette description                           00200001
000300*[Cst]                                                            00210001
000310*[Typ] String                                                     00220001
000320*[MsgIdt]                                                         00230001
000330       10                          PIC X(008) VALUE 'MSVCOUT '.   00240001
000340                                                                  00250001
000350*[Dsc] Version courante de cette description                      00260001
000360*[Cst]                                                            00270001
000370*[MsgVrs]                                                         00280001
000380*[Typ] String                                                     00290001
000390       10                          PIC X(003) VALUE '000'.        00300001
000400                                                                  00310001
000410*[Dsc] Longueur effective de cette description                    00320001
000420*[Cst]                                                            00330001
000430*[Typ] Integer                                                    00340001
000440*[MsgLen]                                                         00350001
000450       10                          PIC 9(005) VALUE 03000.        00360001
000460                                                                  00370001
000470*----------------------------------------------------------------*00380001
000550     05  .                                                        00460001
000560                                                                  00470001
000570*[Dsc] Code retour logique normalisé                              00480001
000690*[Typ] String                                                     00600001
000700       10  :MSVCOUT:-RtnCod.                                      00610001
000710                                                                  00620001
000720*[Dsc] Code retour primaire                                       00630001
000750*[Typ] String                                                     00660001
000760         15  :MSVCOUT:-RtnCod-1               PIC X(004).         00670001
000770           88  :MSVCOUT:-RtnCod-OK               VALUE '0000'.    00680001
000780           88  :MSVCOUT:-RtnCod-STUB             VALUE 'STUB'.    00681002
000790                                                                  00690001
000800*[Dsc] Code retour secondaire                                     00700001
000810*[Typ] String                                                     00710001
000820         15  :MSVCOUT:-RtnCod-2               PIC X(004).         00720001
000830                                                                  00730001
000840*----------------------------------------------------------------*00740001
000910       10  .                                                      00810001
000920                                                                  00820001
000930*[Dsc] Table GESPAR référençant le message utilisateur            00830001
000940*[Typ] String                                                     00840001
000950         15  :MSVCOUT:-UserErr-RefTbl            PIC X(008).      00850001
000960                                                                  00860001
000970*[Dsc] Message d'erreur utilisateur                               00870001
001010*[Typ] String                                                     00910001
001020         15  :MSVCOUT:-UserErr.                                   00911001
001030             20 :MSVCOUT:-UserErr-Pgm            PIC X(008).      00912001
001040             20 :MSVCOUT:-UserErr-Cod-1          PIC X(004).      00913001
001050             20 :MSVCOUT:-UserErr-Cod-2          PIC X(004).      00914001
001060             20 FILLER                           PIC X(001).      00915001
001070             20 :MSVCOUT:-UserErr-ShrtDsc        PIC X(063).      00916001
001080                                                                  00917001
001090*[Dsc] Table GESPAR référençant le message                        00918001
001110*[Typ] String                                                     00919101
001120         15  :MSVCOUT:-TecErr-RefTbl             PIC X(008).      00919201
001130                                                                  00919301
001140*[Dsc] Message d'erreur Technique                                 00919401
001190*[Typ] String                                                     00919901
001200         15  :MSVCOUT:-TecErr.                                    00920001
001210             20 :MSVCOUT:-TecErr-Pgm             PIC X(008).      00930001
001220             20 :MSVCOUT:-TecErr-Fct             PIC X(008).      00940001
001230             20 :MSVCOUT:-TecErr-RtnCod          PIC X(004).      00950001
001240             20 FILLER                           PIC X(001).      00960001
001250             20 :MSVCOUT:-TecErr-ShrtDsc         PIC X(059).      00970001
001260             20 FILLER REDEFINES :MSVCOUT:-TecErr-ShrtDsc.        00980001
001270                25 :MSVCOUT:-TecErr-DB2Cod       PIC ZZ9+.        00990001
001280                25 FILLER                        PIC X(001).      01000001
001290                25 :MSVCOUT:-TecErr-DB2Msg       PIC X(054).      01010001
001300                                                                  01020001
001310*----------------------------------------------------------------*01030001
001360*[Dsc] Indicateur : nécessité de Rollback                         01080001
001370*[Typ] Boolean                                                    01090001
001380     05  :MSVCOUT:-RlbFlg                        PIC X(001).      01100001
001390         88  :MSVCOUT:-RlbFlg-YES                VALUE 'Y'.       01110001
001400                                                                  01120001
001410*----------------------------------------------------------------*01180001
001440     05  FILLER                                  PIC X(054).      01210001
001450                                                                  01220001
001460*----------------------------------------------------------------*01230001
001490     05  .                                                        01260001
001500*[Dsc] Compteur d'occurrences du tableau                          01270001
001510*[Typ] Integer                                                    01280001
001520         10  :MSVCOUT:-MsgNbr                    PIC 9(002).      01290001
001530             88  :MSVCOUT:-MsgNbr-Zero           VALUE ZERO.      01300001
001540             88  :MSVCOUT:-MsgNbr-Vld            VALUE 1 THRU 10. 01310001
001550             88  :MSVCOUT:-MsgNbr-Max            VALUE 10.        01320001
001560*[Dsc] Tableau de 10 messages de 150 caractères                   01330001
001570*[Typ] String                                                     01340001
001580         10  :MSVCOUT:-MsgArray.                                  01350001
001590*    --- Description d'une occurrence de la table                 01360001
001600           15  :MSVCOUT:-MsgElm    OCCURS  10  TIMES              01370001
001610                                   INDEXED BY :MSVCOUT:-MsgIdx.   01380001
001620*              --- Identifiant de phase ou séquence concernée     01390001
001630*[Typ] String                                                     01400001
001640             20  :MSVCOUT:-MsgElm-Seq            PIC X(008).      01410001
001650*              --- réserve pour usage futur                       01420001
001660             20  FILLER                          PIC X(004).      01430001
001670*              --- références du message                          01440001
001680*[Typ] String                                                     01450001
001690             20  :MSVCOUT:-MsgElm-Ref.                            01460001
001700                 25  :MSVCOUT:-MsgElm-RefTbl     PIC X(008).      01470001
001710                 25  :MSVCOUT:-MsgElm-RefPgm     PIC X(008).      01480001
001720                 25  :MSVCOUT:-MsgElm-Ref-1      PIC X(004).      01490001
001730                 25  :MSVCOUT:-MsgElm-Ref-2      PIC X(004).      01500001
001740*              --- Libellé du message                             01510001
001750*[Typ] String                                                     01520001
001760             20  :MSVCOUT:-MsgElm-LongDsc        PIC X(150).      01530001
001770*              --- réserve pour usage futur                       01540001
001780             20  FILLER                          PIC X(020).      01550001
001790                                                                  01560001
001800*    --- réserve pour usage futur                                 01570001
001810     05  FILLER                                  PIC X(100).      01580001
001820                                                                  01590001
001830                                                                  01600001
001840*----------------------------------------------------------------*01610001
001920     05  .                                                        01690001
001930*[Dsc] Compteur d'occurrences du tableau                          01700001
001940*[Typ] Integer                                                    01710001
001950         10  :MSVCOUT:-DataNbr                   PIC 9(002).      01720001
001960             88  :MSVCOUT:-DataNbr-Zero          VALUE ZERO.      01730001
001970             88  :MSVCOUT:-DataNbr-Vld           VALUE 1 THRU 10. 01740001
001980             88  :MSVCOUT:-DataNbr-Max           VALUE 10.        01750001
001990*[Dsc] Tableau des données liées aux messages d'erreur, alertes   01760001
002000*[Typ] String                                                     01770001
002010         10  :MSVCOUT:-DataArray.                                 01780001
002020*    --- Description d'une occurrence de la table                 01790001
002030           15  :MSVCOUT:-DataElm  OCCURS  10  TIMES               01800001
002040                                  INDEXED BY :MSVCOUT:-DataIdx.   01810001
002050*[Typ] String                                                     01820001
002060             20  :MSVCOUT:-DataElm-Name          PIC X(025).      01830001
002070*        --- réserve pour usage futur                             01840001
002080             20  FILLER                          PIC X(010).      01850001
002090                                                                  01860001
002100                                                                  01870001
002110*----------------------------------------------------------------*01880001
002140     05  FILLER                                  PIC X(223).      01910001
002150*----------------------------------------------------------------*01920001
002200*[Dsc] Eye-Catcher lié à cette description                        01970001
002210*[Cst]                                                            01980001
002220*[Typ] String                                                     01990001
002230*[MsgIdtFin]                                                      02000001
002240     05                             PIC X(08) VALUE '/MSVCOUT'.   02010001