      *TypeCobol_Version:0.1(alpha)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Secondary.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01 TypeInSecondary TYPEDEF STRICT PUBLIC.
      *   05 VarSecondary TYPE Main::TypeInMain.
      *01 Result TYPE TypeInSecondary.
       01 Result.
           02 VarSecondary.
             03 VarMain.
               04 VarDependency2.
                 05 VarDependency1.
                   06 VarIntrinsic2.
                     07 VarIntrinsic1 PIC X.
                                      
       PROCEDURE DIVISION.
           DISPLAY VarIntrinsic1 OF VarIntrinsic2
                   OF VarDependency1
                   OF VarDependency2
                   OF VarMain
                   OF VarSecondary
                   OF Result
           .
       END PROGRAM Secondary.
