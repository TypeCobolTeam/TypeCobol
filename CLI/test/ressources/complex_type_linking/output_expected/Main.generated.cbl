      *TypeCobol_Version:0.1(alpha)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01 TypeInMain TYPEDEF STRICT PUBLIC.
      *   05 VarMain TYPE Dependency2::TypeInDependency2.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM Main.
