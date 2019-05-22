000020     02  Group1.
000030         03  G1-Var1             PIC X(8).              
000040         03  G1-Var2             PIC X(12).             
000050     02  Group2.                                   
000060         03  G2-Var1             PIC X(04).             
000070             88  PGM-TEST  VALUE 'TEST'.                
000080         03  G2-Var2             PIC X(01).             
000090         03  G2-Var3.
000100             04  G2-Var4.                            
000110                 05  G2-Var5 PIC 9(07).            
000120                 05  FILLER      PIC X.                 
000130             04  G2-Var6         PIC X(05).             
000140             04  G2-Var7         PIC X(02).             
000150     02  FILLER.                                        
000160         03  G3-Var1             PIC X(8).              
000170         03  G3-Var2             PIC X(16).             
000180         03  G3-Var3             PIC X(8).              
000190         03  G3-Var4             PIC X(8). 