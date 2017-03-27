      * 2 errors
      * Line 9[23,38] <28, Error, Directives> - Failed to load the text document referenced by a COPY directive : Could not find a Cobol source file named YPRTD20 in 
      * Line 10[23,38] <28, Error, Directives> - Failed to load the text document referenced by a COPY directive : Could not find a Cobol source file named YCONVEA in 
000000 IDENTIFICATION DIVISION.
000000 PROGRAM-ID. RemarksNonUsed.                   
000000 ENVIRONMENT DIVISION.
      *Remarks Directive with non used copy. Has to be conserve. 
000000*
      *REMARKS. COPY=(
      *        YXXXFAL
      *        YXXXFALS
      *        YPRTD20L
      *        YCONVEAL
      *        ).
                                
000000 DATA DIVISION.
000000 WORKING-STORAGE section.
000000    01 PRTD20L. COPY  YPRTD20L.   
000000    01 CONVEAL. COPY  YCONVEAL.   
000000    
000000
000000 PROCEDURE DIVISION.
000000     GOBACK
000000     .
000000 END PROGRAM MYPGM.
