       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 group1.
      * FILLER-like name, with children
          05 pseudo-filler-1.
             10 data1        PIC X.
      * Anonymous, with children
          05.
             10 data2        PIC X.
      * FILLER keyword, with children
          05 FILLER.
             10 data3        PIC X.
      * FILLER-like name, no children
          05 pseudo-filler-2 PIC X.
      * Anonymous, no children
          05                 PIC X.
      * FILLER keyword, no children
          05 FILLER          PIC X.
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
        "position": { "line": 20, "character": 26 }
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
      * FILLER-like name, with children
          05 pseudo-filler-1.
             10 data1        PIC X.
      * Anonymous, with children
          05.
             10 data2        PIC X.
      * FILLER keyword, with children
          05 FILLER.
             10 data3        PIC X.
      * FILLER-like name, no children
          05 pseudo-filler-2 PIC X.
      * Anonymous, no children
          05                 PIC X.
      * FILLER keyword, no children
          05 FILLER          PIC X.
       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'group1'
      D    DISPLAY '  pseudo-filler-1'
      D    DISPLAY '    data1 <' data1 '>'
      D    DISPLAY '  FILLER (2:1)'
      D    DISPLAY '    data2 <' data2 '>'
      D    DISPLAY '  FILLER (3:1)'
      D    DISPLAY '    data3 <' data3 '>'
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.