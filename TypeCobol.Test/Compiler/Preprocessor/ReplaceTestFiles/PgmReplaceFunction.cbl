000000* Real sample found in source code
000001 replace                                                                                                                           
000002 ==:createError(:== by                                           
000003 == set CCTERR-FCT-CRE-INN to true                            
000004    call 'CREATE-ERROR' using CCTERR CCTEXT CCTFAL CCTZON 
000005                              CCTFRE                        
000006                              ==                             
000007                                                                  
000008 ==:):== by                                                      
000009 ==omitted end-call ==.                          
000010 
000011 :createError(:CErr-Rc1-xxx  Secondaire :):
000012
000013 replace
000014 ==:TRAC:==  by                                              
000015 ==CCTLOG-LVL-I or TRC-I                                  
000016   call 'ZCALLPGM' using CCTZXLOG CCTEXT CCTFAL CCTZON
000017                         CCTFRE CCTLOG  
000018                                                             
000019 ==                                     
000020 ==:FIN-LOG:== by                                     
000021 ==   omitted                                       
000022      end-call                                         
000023      end-if==.   
000024 
000025 if :TRAC:                                         
000026    content 'Début traitement xxxxx'    
000027    content '£CCTFRE' CCTFRE                      
000028    content '£CCTFAL' CCTFAL                    
000029 :FIN-LOG:

                                      
