/----------------------------------------------------------------
* MSVCINP               partie ALLER FIXE des MESSAGES �chang�s avec tout SERVICE APPLICATIF C14
*                                        Version  : 000          
*                                        Longueur : 01000 octets 
*----------------------------------------------------------------
01  :MSVCINP:.                                                   
                                                                 
*----------------------------------------------------------------
* En-t�te g�n�ralis� pour tout COPY de type MESSAGE           C14*
* Comportant TAGs (ou BALISEs) standards/normalis�s appos�es via  commentaires standards � respecter                             
* TAGs standards/normalis�s C14 d'en-t�te de COPY MESSAGE        
*  - [MsgIdt] : identifiant du message (=nom COPY associ�)       
*  - [MsgVrs] : version en cours de la description de cette partie du message                             
*  - [MsgLen] : Longueur effective de cette description
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