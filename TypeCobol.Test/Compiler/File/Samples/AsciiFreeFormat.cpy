/----------------------------------------------------------------
01  :MSVCINP:.                                                   
                                                                 
*----------------------------------------------------------------
* Comportant TAGs (ou BALISEs) standards/normalisés apposées via  commentaires standards à respecter                             
*----------------------------------------------------------------
    05  .                                                        
      10                          PIC X(008) VALUE 
 'MSVCINP '.   
                                                                 
*[Dsc] Version courante de cette description                      
      10                          PIC X(003) VALUE '000'.        
*[Typ] String                                                     
D        15  :MSVCINP:-AppSessnId           PIC X(064).           
                                                                 
*----------------------------------------------------------------
* Réserve pour Usages Futurs                                      
*----------------------------------------------------------------
    05  FILLER                             PIC X(499).           
*----------------------------------------------------------------
* Eye-Catcher - balise de fin de la description, permettant de vérifier à minima la cohérence du contenu porté. 
*               Si balise de fin présente et non altérée...      
*----------------------------------------------------------------
*[Dsc] Eye-Catcher lié à cette description                        
*[Cst]                                                            
*[Typ] String                                                    
*[MsgIdtEnd]                                                      
d    05                            PIC X(008) VALUE '/MSVCINP'.   