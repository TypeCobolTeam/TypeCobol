       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group3.
          05 table3 OCCURS 10.
             10  OCCURS 20.
                15 var5 PIC X.
                15 FILLER PIC X(32).
                15 var6 PIC X.
       PROCEDURE DIVISION.
           GOBACK
           .
       END PROGRAM TCOMFL06.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 10, "character": 26 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 1, "name": "group3", "ch": [
                    {
                        "vm": 1, "name": "table3", "ch": [
                            {
                                "vm": 0, "idx": 0, "ch": [
                                    {
                                        "vm": 0, "idx": 1
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
-------------------------------------------------------------------------------------------------
refactoring.label=Debug instructions successfully generated.
refactoring.source=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group3.
          05 table3 OCCURS 10.
             10  OCCURS 20.
                15 var5 PIC X.
                15 FILLER PIC X(32).
                15 var6 PIC X.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D77 Idx-d4df4249-1 PIC 9(4) COMP-5.
      D77 Idx-d4df4249-2 PIC 9(4) COMP-5.
      *</DBG>

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    PERFORM VARYING Idx-d4df4249-1 FROM 1 BY 1 UNTIL
      D    Idx-d4df4249-1 > 10
      D      PERFORM VARYING Idx-d4df4249-2 FROM 1 BY 1 UNTIL
      D      Idx-d4df4249-2 > 20
      D        DISPLAY '    FILLER (' Idx-d4df4249-1 ' ' Idx-d4df4249-2
      D        ')'
      D        DISPLAY '      FILLER (' Idx-d4df4249-1 ' '
      D        Idx-d4df4249-2 ') <' table3 (Idx-d4df4249-1) (2 +
      D        (Idx-d4df4249-2 - 1) * 34:32) '>'
      D      END-PERFORM
      D    END-PERFORM
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.