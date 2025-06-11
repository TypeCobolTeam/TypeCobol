       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 three-levels.
          05  OCCURS 1000.
             10  OCCURS 200.
                15  OCCURS 30.
                   20 var1 PIC X.
                   20 FILLER PIC X(20).
                   20 var2 PIC X.
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
        "position": { "line": 11, "character": 26 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 1, "name": "three-levels", "ch": [
                    {
                        "vm": 1, "idx": 0, "ch": [
                            {
                                "vm": 1, "idx": 0, "ch": [
                                    {
                                        "vm": 1, "idx": 0, "ch": [
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
    }
]
-------------------------------------------------------------------------------------------------
refactoring.label=Debug instructions successfully generated.
refactoring.source=
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 three-levels.
          05  OCCURS 1000.
             10  OCCURS 200.
                15  OCCURS 30.
                   20 var1 PIC X.
                   20 FILLER PIC X(20).
                   20 var2 PIC X.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D77 Idx-d4df4249-1 PIC 9(4) COMP-5.
      D77 Idx-d4df4249-2 PIC 9(4) COMP-5.
      D77 Idx-d4df4249-3 PIC 9(4) COMP-5.
      *</DBG>

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    PERFORM VARYING Idx-d4df4249-1 FROM 1 BY 1 UNTIL
      D    Idx-d4df4249-1 > 1000
      D      PERFORM VARYING Idx-d4df4249-2 FROM 1 BY 1 UNTIL
      D      Idx-d4df4249-2 > 200
      D        PERFORM VARYING Idx-d4df4249-3 FROM 1 BY 1 UNTIL
      D        Idx-d4df4249-3 > 30
      D          DISPLAY '        FILLER (' Idx-d4df4249-1 ' '
      D          Idx-d4df4249-2 ' ' Idx-d4df4249-3 ') <' three-levels (2
      D          + (Idx-d4df4249-3 - 1) * 22 + (Idx-d4df4249-2 - 1) *
      D          660 + (Idx-d4df4249-1 - 1) * 132000:20) '>'
      D          DISPLAY '        -------------------------------------'
      D                  '---------------------------------------------'
      D                  '------------------------------'
      D        END-PERFORM
      D        DISPLAY '      -----------------------------------------'
      D                '-----------------------------------------------'
      D                '--------------------------'
      D      END-PERFORM
      D      DISPLAY '    ---------------------------------------------'
      D              '-------------------------------------------------'
      D              '----------------------'
      D    END-PERFORM
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.