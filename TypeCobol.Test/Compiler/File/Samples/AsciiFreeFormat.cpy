/----------------------------------------------------------------
01  :MSVCINP:.                                                   
                                                                 
*----------------------------------------------------------------
* Comportant TAGs (ou BALISEs) standards/normalis�s appos�es via  commentaires standards � respecter                             
*----------------------------------------------------------------
    05  .                                                        
      10                          PIC X(008) VALUE 
 'MSVCINP '.   
                                                                 
*[Dsc] Version courante de cette description                      
      10                          PIC X(003) VALUE '000'.        
*[Typ] String                                                     
D        15  :MSVCINP:-AppSessnId           PIC X(064).           
                                                                 
*----------------------------------------------------------------
* R�serve pour Usages Futurs                                      
*----------------------------------------------------------------
    05  FILLER                             PIC X(499).           
*----------------------------------------------------------------
* Eye-Catcher - balise de fin de la description, permettant de v�rifier � minima la coh�rence du contenu port�. 
*               Si balise de fin pr�sente et non alt�r�e...      
*----------------------------------------------------------------
*[Dsc] Eye-Catcher li� � cette description                        
*[Cst]                                                            
*[Typ] String                                                    
*[MsgIdtEnd]                                                      
d    05                            PIC X(008) VALUE '/MSVCINP'.   