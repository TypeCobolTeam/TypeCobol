Simplified Codegen for reference only. DO NOT ATTEMPT TO BUILD, DO NOT DEPLOY !
      *TypeCobol_Version:[[ParserVersion]]
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TypeCobol-Generated.
           05 TC-DPDCY01 pic X(08) value 'DPDCY01'.
           05 DPDCY01-Fct-b20bd03f-FunTest pic X(30)
                value 'Fct=b20bd03f-FunTest'.
           05 DPDCY01-Fct-fa50eaf6-FunTest pic X(30)
                value 'Fct=fa50eaf6-FunTest'.
                               
       01 alpha     pic X.
      *01 myVar     type DPDCY01::GroupType.
       01 myVar.
           02 subType.
             03 myPic pic X.
                                            
       PROCEDURE DIVISION.
       INIT-LIBRARY.
           Continue.
      *Must resolve dependency function acepting alphanumeric
      *call DPDCY01::FunTest input alpha.
       CALL 'zcallpgm' using TC-DPDCY01
                    DPDCY01-Fct-b20bd03f-FunTest
                                 alpha
           end-call
                                        .
      *Must resolve dependency function acepting Type01
      *call DPDCY01::FunTest input myVar::subType.
       CALL 'zcallpgm' using TC-DPDCY01
                    DPDCY01-Fct-fa50eaf6-FunTest
                                 subType IN myVar
           end-call
                                                 .
       end program PGMTEST.
