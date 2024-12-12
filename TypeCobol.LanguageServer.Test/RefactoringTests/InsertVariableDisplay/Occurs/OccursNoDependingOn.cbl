       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 one-level.
          05 root-table-1 OCCURS 10.
             10 var1 PIC X.
             10 var2 PIC X.
       01 two-levels.
          05 root-table-2 OCCURS 100.
             10 table-level1-1 OCCURS 20.
                15 var3 PIC X.
                15 var4 PIC X.
       01 three-levels.
          05 root-table-3 OCCURS 1000.
             10 table-level1-2 OCCURS 200.
                15 table-level2-1 OCCURS 30.
                   20 var5 PIC X.
                   20 var6 PIC X.
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
        "position": { "line": 19, "character": 26 }
    },
    false,
    {
        "visitMode": 1, "index": 0, "subSelections": [
            {
                "visitMode": 2, "name": "one-level"
            },
            {
                "visitMode": 2, "name": "two-levels"
            },
            {
                "visitMode": 2, "name": "three-levels"
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
       01 one-level.
          05 root-table-1 OCCURS 10.
             10 var1 PIC X.
             10 var2 PIC X.
       01 two-levels.
          05 root-table-2 OCCURS 100.
             10 table-level1-1 OCCURS 20.
                15 var3 PIC X.
                15 var4 PIC X.
       01 three-levels.
          05 root-table-3 OCCURS 1000.
             10 table-level1-2 OCCURS 200.
                15 table-level2-1 OCCURS 30.
                   20 var5 PIC X.
                   20 var6 PIC X.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D77 Idx-a7a7e5cc-1 PIC 9(4) COMP.
      D77 Idx-a7a7e5cc-2 PIC 9(3) COMP.
      D77 Idx-a7a7e5cc-3 PIC 9(2) COMP.
      *</DBG>

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'one-level'
      D    PERFORM VARYING Idx-a7a7e5cc-1 FROM 1 BY 1 UNTIL
      D    Idx-a7a7e5cc-1 > 10
      D      DISPLAY '  root-table-1 (' Idx-a7a7e5cc-1 ')'
      D      DISPLAY '    var1 (' Idx-a7a7e5cc-1 ') <' var1
      D      (Idx-a7a7e5cc-1) '>'
      D      DISPLAY '    var2 (' Idx-a7a7e5cc-1 ') <' var2
      D      (Idx-a7a7e5cc-1) '>'
      D    END-PERFORM
      D    DISPLAY 'two-levels'
      D    PERFORM VARYING Idx-a7a7e5cc-1 FROM 1 BY 1 UNTIL
      D    Idx-a7a7e5cc-1 > 100
      D      DISPLAY '  root-table-2 (' Idx-a7a7e5cc-1 ')'
      D      PERFORM VARYING Idx-a7a7e5cc-2 FROM 1 BY 1 UNTIL
      D      Idx-a7a7e5cc-2 > 20
      D        DISPLAY '    table-level1-1 (' Idx-a7a7e5cc-1 ' '
      D        Idx-a7a7e5cc-2 ')'
      D        DISPLAY '      var3 (' Idx-a7a7e5cc-1 ' ' Idx-a7a7e5cc-2
      D        ') <' var3 (Idx-a7a7e5cc-1 Idx-a7a7e5cc-2) '>'
      D        DISPLAY '      var4 (' Idx-a7a7e5cc-1 ' ' Idx-a7a7e5cc-2
      D        ') <' var4 (Idx-a7a7e5cc-1 Idx-a7a7e5cc-2) '>'
      D      END-PERFORM
      D    END-PERFORM
      D    DISPLAY 'three-levels'
      D    PERFORM VARYING Idx-a7a7e5cc-1 FROM 1 BY 1 UNTIL
      D    Idx-a7a7e5cc-1 > 1000
      D      DISPLAY '  root-table-3 (' Idx-a7a7e5cc-1 ')'
      D      PERFORM VARYING Idx-a7a7e5cc-2 FROM 1 BY 1 UNTIL
      D      Idx-a7a7e5cc-2 > 200
      D        DISPLAY '    table-level1-2 (' Idx-a7a7e5cc-1 ' '
      D        Idx-a7a7e5cc-2 ')'
      D        PERFORM VARYING Idx-a7a7e5cc-3 FROM 1 BY 1 UNTIL
      D        Idx-a7a7e5cc-3 > 30
      D          DISPLAY '      table-level2-1 (' Idx-a7a7e5cc-1 ' '
      D          Idx-a7a7e5cc-2 ' ' Idx-a7a7e5cc-3 ')'
      D          DISPLAY '        var5 (' Idx-a7a7e5cc-1 ' '
      D          Idx-a7a7e5cc-2 ' ' Idx-a7a7e5cc-3 ') <' var5
      D          (Idx-a7a7e5cc-1 Idx-a7a7e5cc-2 Idx-a7a7e5cc-3) '>'
      D          DISPLAY '        var6 (' Idx-a7a7e5cc-1 ' '
      D          Idx-a7a7e5cc-2 ' ' Idx-a7a7e5cc-3 ') <' var6
      D          (Idx-a7a7e5cc-1 Idx-a7a7e5cc-2 Idx-a7a7e5cc-3) '>'
      D        END-PERFORM
      D      END-PERFORM
      D    END-PERFORM
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.