000001 COPY TEXTNAME1.
000001 COPY TEXTNAME2 SUPPRESS.
000001 COPY TEXTNAME3 OF LIBRARY1.
000001 COPY TEXTNAME4 IN LIBRARY2.
000001 COPY TEXTNAME5 IN LIBRARY3 SUPPRESS .
000001 COPY TEXTNAME6 OF LIBRARY4 SUPPRESS.
000001 COPY TEXTNAME7 REPLACING == TOTO1 == BY ==TITI1==.
000001 COPY TEXTNAME8 REPLACING TOTO2 BY TITI2.
000001 COPY TEXTNAME9 REPLACING 'TOTO3' BY "TITI3".
000001 COPY TEXTNAME10 REPLACING DISPLAY BY MOVE.
000001 COPY TEXTNAME11 OF LIBRARY4 SUPPRESS
000001 REPLACING TOTO5 BY TITI6
000001 == TOTO6 TOTO7 == 
000001 BY == TITI6
000001 TITI7 ==.
000001 COPY TEXTNAME12 SUPPRESS. COPY TEXTNAME13.
000001 COPY TEXTNAME12 SUPPRESS. COPY 
000001 TEXTNAME13. COPY TEXTNAME14.

000001 COPY TEXTNAME1
000001 COPY TEXTNAME2 SUPPRESS COPY 
000001 TEXTNAME13 COPY TEXTNAME14.
000001 COPY TEXTNAME4 SUPPRESS IN LIBRARY2.
000001 COPY TEXTNAME6 OF SUPPRESS.
000001 COPY TEXTNAME7 REPLACING == TOTO1 = BY ==TITI1==
000001 COPY TEXTNAME11
000001 REPLACING TOTO5 TITI6
000001 REPLACING
000001 == TOTO6 TOTO7 == 
000001 .

000001 DISPLAY "error".

000030*REMARKS. COPY=(YIDCHB1                                           00030000
003790 01  YIDCAT1.                      COPY YIDCAT1.                  03464000
005020 01  INIPGM.              COPY YINIPGM SUPPRESS.                  04570000
006030*- cette copy est utilisée dans les paramètres d'appel du         05580000
001440*     GENT.COPY.COBOL(EPXX067W)                                 * 
001850     COPY  YEPSU00     REPLACING YEPSU00 BY  EPSZ0230E.           00001120
001750 COPY MSVCINP  REPLACING ==:MSVCINP:==  BY ==MSVCINP==.           01031302
001760 COPY MSVCOUT  REPLACING ==:MSVCOUT:==  BY ==MSVCOUT==.           01031402
001770                                                                  01031502
001780* -- Copies variables spécifiques aux requêtes                    01031602
001790 01 MAUC1XX                     PIC X.                            01031702
001800 COPY MAUC120  REPLACING ==:MAUC120:== BY ==MAUC120==.            01031802
000001 
000001 COPY TEST1 OF LIBRARY1
000001 replacing                                                                                                                           
000001   ==:createError(:== by                                           
000001      == set CCTERR-FCT-CRE-INN to true                            
000001        call 'CREATE-ERROR' using CCTERR CCTEXT CCTFAL CCTZON 
000001                                     CCTFRE                        
000001                                     ==                             
000001                                                                   
000001   ==:):== by                                                      
000001      ==omitted end-call ==                          
000001 . DISPLAY "OK". COPY TEST2 IN
000001 LIBRARY2 SUPPRESS
000001 REPLACING ==:TRAC:==  by                                              
000001      ==CCTLOG-LVL-I or TRC-I                                  
000001           call 'ZCALLPGM' using CCTZXLOG CCTEXT CCTFAL CCTZON
000001                                 CCTFRE CCTLOG  
000001                                                               
000001                        ==                                     
000001          ==:FIN-LOG:== by                                     
000001            ==   omitted                                       
000001              end-call                                         
000001              end-if==   
000001 .                                      

000001 COPY TEXTNAME1. COPY