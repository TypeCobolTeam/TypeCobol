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
                   20 var-national PIC N(20).
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
        "position": { "line": 17, "character": 26 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 1, "name": "three-levels", "ch": [
                    {
                        "vm": 1, "name": "root-table-3", "ch": [
                            {
                                "vm": 1, "name": "table-level1-2", "ch": [
                                    {
                                        "vm": 1, "name": "table-level2-1", "ch": [
                                            {
                                                "vm": 0, "name": "var-national"
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
                   20 var-national PIC N(20).
                   20 var2 PIC X.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D77 Idx-d4df4249-1 PIC 9(4) COMP-5.
      D77 Idx-d4df4249-2 PIC 9(4) COMP-5.
      D77 Idx-d4df4249-3 PIC 9(4) COMP-5.
      *</DBG>

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    IF root-table-3-count >= 1 AND <= 1000
      D      PERFORM VARYING Idx-d4df4249-1 FROM 1 BY 1 UNTIL
      D      Idx-d4df4249-1 > root-table-3-count
      D        IF table-level1-2-count >= 1 AND <= 200
      D          PERFORM VARYING Idx-d4df4249-2 FROM 1 BY 1 UNTIL
      D          Idx-d4df4249-2 > table-level1-2-count
      D            IF table-level2-1-count >= 1 AND <= 30
      D              PERFORM VARYING Idx-d4df4249-3 FROM 1 BY 1 UNTIL
      D              Idx-d4df4249-3 > table-level2-1-count
      D                DISPLAY '        var-national (' Idx-d4df4249-1
      D                ' ' Idx-d4df4249-2 ' ' Idx-d4df4249-3 ') <'
      D                FUNCTION DISPLAY-OF (var-national (Idx-d4df4249-1
      D                Idx-d4df4249-2 Idx-d4df4249-3)) '>'
      D              END-PERFORM
      D            ELSE
      D              DISPLAY 'Cannot DISPLAY "table-level2-1" because i'
      D                      'ts DEPENDING ON "table-level2-1-count" is'
      D                      ' out of range.'
      D            END-IF
      D          END-PERFORM
      D        ELSE
      D          DISPLAY 'Cannot DISPLAY "table-level1-2" because its D'
      D                  'EPENDING ON "table-level1-2-count" is out of '
      D                  'range.'
      D        END-IF
      D      END-PERFORM
      D    ELSE
      D      DISPLAY 'Cannot DISPLAY "root-table-3" because its DEPENDI'
      D              'NG ON "root-table-3-count" is out of range.'
      D    END-IF
      *</DBG>

           GOBACK
           .
       END PROGRAM TCOMFL06.