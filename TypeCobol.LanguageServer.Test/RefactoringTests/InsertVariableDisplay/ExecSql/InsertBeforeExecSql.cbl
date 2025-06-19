       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
          05 label-id    PIC 9(4).
          05 label-value PIC X(200).
       PROCEDURE DIVISION.
           EXEC SQL
              SELECT
                 LABEL_VALUE
              INTO
                 :label-value
              FROM
                 T_LABELS
              WHERE
                 LABEL_ID = :label-id
           END-EXEC
           GOBACK
           .
       END PROGRAM TCOMFL06.
-------------------------------------------------------------------------------------------------
TypeCobol.LanguageServer.Commands.InsertVariableDisplay.InsertVariableDisplayRefactoringProcessor
-------------------------------------------------------------------------------------------------
[
    {
        "textDocument": { "uri": "file:/test.expected.cbl" },
        "position": { "line": 12, "character": 18 }
    },
    true,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 0, "name": "group1"
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
          05 label-id    PIC 9(4).
          05 label-value PIC X(200).
       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'group1 <' group1 '>'
      *</DBG>

           EXEC SQL
              SELECT
                 LABEL_VALUE
              INTO
                 :label-value
              FROM
                 T_LABELS
              WHERE
                 LABEL_ID = :label-id
           END-EXEC
           GOBACK
           .
       END PROGRAM TCOMFL06.