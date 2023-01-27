/----------------------------------------------------------------
* MSVCINP               partie ALLER FIXE des MESSAGES échangés avec tout SERVICE APPLICATIF C14
*                                        Version  : 000          
*                                        Longueur : 01000 octets 
*----------------------------------------------------------------
01  :MSVCINP:.                                                   
                                                                 
*----------------------------------------------------------------
* En-tête généralisé pour tout COPY de type MESSAGE           C14*
* Comportant TAGs (ou BALISEs) standards/normalisés apposées via  commentaires standards à respecter                             
* TAGs standards/normalisés C14 d'en-tête de COPY MESSAGE        
*  - [MsgIdt] : identifiant du message (=nom COPY associé)       
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