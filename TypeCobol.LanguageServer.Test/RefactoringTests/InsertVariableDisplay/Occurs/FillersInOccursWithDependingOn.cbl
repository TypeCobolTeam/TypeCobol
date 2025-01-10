       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 three-levels.
          05 root-table-3-count   PIC S9(4) BINARY.
          05 table-level1-2-count PIC S9(3) BINARY.
          05 table-level2-1-count PIC S9(2) BINARY.
          05 root-table-3 OCCURS 1000
                          DEPENDING ON root-table-3-count.
             10 table-level1-2 OCCURS 200
                               DEPENDING ON table-level1-2-count.
                15 table-level2-1 OCCURS 30
                                  DEPENDING ON table-level2-1-count.
                   20 var1 PIC X.
                   20 FILLER PIC X(20).
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
        "position": { "line": 17, "character": 26 }
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
                                                "visitMode": 0, "index": 1
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
          05 root-table-3-count   PIC S9(4) BINARY.
          05 table-level1-2-count PIC S9(3) BINARY.
          05 table-level2-1-count PIC S9(2) BINARY.
          05 root-table-3 OCCURS 1000
                          DEPENDING ON root-table-3-count.
             10 table-level1-2 OCCURS 200
                               DEPENDING ON table-level1-2-count.
                15 table-level2-1 OCCURS 30
                                  DEPENDING ON table-level2-1-count.
                   20 var1 PIC X.
                   20 FILLER PIC X(20).
                   20 var2 PIC X.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D77 Idx-f21f0ca2-1 PIC 9(4) COMP.
      D77 Idx-f21f0ca2-2 PIC 9(3) COMP.
      D77 Idx-f21f0ca2-3 PIC 9(2) COMP.
      *</DBG>

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    IF root-table-3-count IS NUMERIC
      D      PERFORM VARYING Idx-f21f0ca2-1 FROM 1 BY 1 UNTIL
      D      Idx-f21f0ca2-1 > root-table-3-count
      D        IF table-level1-2-count IS NUMERIC
      D          PERFORM VARYING Idx-f21f0ca2-2 FROM 1 BY 1 UNTIL
      D          Idx-f21f0ca2-2 > table-level1-2-count
      D            IF table-level2-1-count IS NUMERIC
      D              PERFORM VARYING Idx-f21f0ca2-3 FROM 1 BY 1 UNTIL
      D              Idx-f21f0ca2-3 > table-level2-1-count
      D                DISPLAY '        FILLER (' Idx-f21f0ca2-1 ' '
      D                Idx-f21f0ca2-2 ' ' Idx-f21f0ca2-3 ') (2:20) <'
      D                table-level2-1 (Idx-f21f0ca2-1 Idx-f21f0ca2-2
      D                Idx-f21f0ca2-3) (2:20) '>'
      D              END-PERFORM
      D            ELSE
      D              DISPLAY 'Cannot DISPLAY "table-level2-1" because i'
      D                      'ts DEPENDING ON "table-level2-1-count" is'
      D                      ' not numeric.'
      D            END-IF
      D          END-PERFORM
      D        ELSE
      D          DISPLAY 'Cannot DISPLAY "table-level1-2" because its D'
      D                  'EPENDING ON "table-level1-2-count" is not num'
      D                  'eric.'
      D        END-IF
      D      END-PERFORM
      D    ELSE
      D      DISPLAY 'Cannot DISPLAY "root-table-3" because its DEPENDI'
      D              'NG ON "root-table-3-count" is not numeric.'
      D    END-IF
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.