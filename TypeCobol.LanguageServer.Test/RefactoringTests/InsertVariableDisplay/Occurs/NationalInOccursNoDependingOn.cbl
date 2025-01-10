       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 three-levels.
          05 root-table-3 OCCURS 1000.
             10 table-level1-2 OCCURS 200.
                15 table-level2-1 OCCURS 30.
                   20 var1 PIC X.
                   20 var-national PIC N(20).
                   20 var2 PIC X.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.
------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.Refactor.InsertVariableDisplayRefactoringProcessor
------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 11, "character": 26 }
    },
    false,
    {
        "visitMode": 1, "index": 0, "subSelections": [
            {
                "visitMode": 1, "name": "three-levels", "subSelections": [
                    {
                        "visitMode": 1, "name": "root-table-3", "subSelections": [
                            {
                                "visitMode": 1, "name": "table-level1-2", "subSelections": [
                                    {
                                        "visitMode": 1, "name": "table-level2-1", "subSelections": [
                                            {
                                                "visitMode": 0, "name": "var-national"
                                            }
                                        ]
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
    }
]
------------------------------------------------------------------------------------
refactoring.label=Debug instructions successfully generated.
refactoring.source=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 three-levels.
          05 root-table-3 OCCURS 1000.
             10 table-level1-2 OCCURS 200.
                15 table-level2-1 OCCURS 30.
                   20 var1 PIC X.
                   20 var-national PIC N(20).
                   20 var2 PIC X.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D77 Idx-d7a64040-1 PIC 9(4) COMP.
      D77 Idx-d7a64040-2 PIC 9(3) COMP.
      D77 Idx-d7a64040-3 PIC 9(2) COMP.
      *</DBG>

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    PERFORM VARYING Idx-d7a64040-1 FROM 1 BY 1 UNTIL
      D    Idx-d7a64040-1 > 1000
      D      PERFORM VARYING Idx-d7a64040-2 FROM 1 BY 1 UNTIL
      D      Idx-d7a64040-2 > 200
      D        PERFORM VARYING Idx-d7a64040-3 FROM 1 BY 1 UNTIL
      D        Idx-d7a64040-3 > 30
      D          DISPLAY '        var-national (' Idx-d7a64040-1 ' '
      D          Idx-d7a64040-2 ' ' Idx-d7a64040-3 ') <' FUNCTION
      D          DISPLAY-OF (var-national (Idx-d7a64040-1 Idx-d7a64040-2
      D          Idx-d7a64040-3)) '>'
      D        END-PERFORM
      D      END-PERFORM
      D    END-PERFORM
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.