*Don't except any errors
 IDENTIFICATION DIVISION.           
 PROGRAM-ID. EnclosingPgm.
 PROCEDURE DIVISION                 
     GOBACK                         
     .
 IDENTIFICATION DIVISION.           
 PROGRAM-ID. NestedPgm    IS COMMON.
 DATA DIVISION.                     
 LINKAGE SECTION.                   
 01  myData  PIC X.
                                    
 PROCEDURE DIVISION.
     GOBACK                         
     .                              
                                    
 END PROGRAM TRANS-ADR-CLC.         
 END PROGRAM EnclosingPgm.
