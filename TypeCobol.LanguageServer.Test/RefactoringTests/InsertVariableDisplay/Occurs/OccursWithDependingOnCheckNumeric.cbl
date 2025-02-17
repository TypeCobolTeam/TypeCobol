       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 one-level.
          05 root-table-1-count PIC S9(2).
          05 root-table-1 OCCURS 10
                          DEPENDING ON root-table-1-count.
             10 var1 PIC X.
             10 var2 PIC X.
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
                "vm": 2, "name": "one-level"
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
       01 one-level.
          05 root-table-1-count PIC S9(2).
          05 root-table-1 OCCURS 10
                          DEPENDING ON root-table-1-count.
             10 var1 PIC X.
             10 var2 PIC X.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D77 Idx-d4df4249-1 PIC 9(4) COMP-5.
      *</DBG>

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'one-level'
      D    DISPLAY '  root-table-1-count <' root-table-1-count '>'
      D    IF root-table-1-count IS NUMERIC AND root-table-1-count >= 1
      D    AND <= 10
      D      PERFORM VARYING Idx-d4df4249-1 FROM 1 BY 1 UNTIL
      D      Idx-d4df4249-1 > root-table-1-count
      D        DISPLAY '  root-table-1 (' Idx-d4df4249-1 ')'
      D        DISPLAY '    var1 (' Idx-d4df4249-1 ') <' var1
      D        (Idx-d4df4249-1) '>'
      D        DISPLAY '    var2 (' Idx-d4df4249-1 ') <' var2
      D        (Idx-d4df4249-1) '>'
      D      END-PERFORM
      D    ELSE
      D      DISPLAY 'Cannot DISPLAY "root-table-1" because its DEPENDI'
      D              'NG ON "root-table-1-count" is either not numeric '
      D              'or out of range.'
      D    END-IF
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.