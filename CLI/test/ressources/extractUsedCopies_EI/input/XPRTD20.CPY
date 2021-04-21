000070 01  X:PRTD20:.                                                     
000080      05  FILLER                 PIC X(8) VALUE 'Y:PRTD20: '.       
000090      05  :PRTD20:-RECH            PIC X(2).                        
000100         88 :PRTD20:-VIDE          VALUE '  '.                      
000110*                                                                 
000120      05  :PRTD20:-I.                                               
000130*                                   LONGUEUR 44                   
000140         10 LDO-NO-DOSSIER       PIC S9(11)V USAGE COMP-3.        
000150         10 LDO-CD-ETA           PIC X(1).                        
000160            88  LDO-CD-ETA-SAI          VALUE 'S'.                
000170            88  LDO-CD-ETA-VAL          VALUE 'V'. 