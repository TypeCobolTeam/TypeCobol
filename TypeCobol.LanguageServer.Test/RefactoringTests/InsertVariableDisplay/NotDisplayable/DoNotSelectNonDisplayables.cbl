       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 item1 PIC X.
          05 non-displayable-index INDEX.
          05 non-displayable-func-pointer FUNCTION-POINTER.
          05 non-displayable-proc-pointer PROCEDURE-POINTER.
          05 FILLER PIC X.
          05        PIC X.
          05 too-many_occurences PIC X OCCURS 70000.
          05 item2 PIC X.
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
        "position": { "line": 13, "character": 26 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 2, "name": "group1"
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
       01 group1.
          05 item1 PIC X.
          05 non-displayable-index INDEX.
          05 non-displayable-func-pointer FUNCTION-POINTER.
          05 non-displayable-proc-pointer PROCEDURE-POINTER.
          05 FILLER PIC X.
          05        PIC X.
          05 too-many_occurences PIC X OCCURS 70000.
          05 item2 PIC X.
       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'group1'
      D    DISPLAY '  item1 <' item1 '>'
      D    DISPLAY '  item2 <' item2 '>'
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.