       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOMFL06.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 one-level.
          05 root-table-1-count PIC S9(2) BINARY.
          05 root-table-1 OCCURS 10
                          DEPENDING ON root-table-1-count.
             10 var1 PIC X.
             10 var2 PIC X.
       01 two-levels.
          05 root-table-2-count   PIC S9(3) BINARY.
          05 table-level1-1-count PIC S9(2) BINARY.
          05 root-table-2 OCCURS 100
                          DEPENDING ON root-table-2-count.
             10 table-level1-1 OCCURS 20
                               DEPENDING ON table-level1-1-count.
                15 var3 PIC X.
                15 var4 PIC X.
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
                   20 var5 PIC X.
                   20 var6 PIC X.
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
        "position": { "line": 31, "character": 26 }
    },
    false,
    {
        "vm": 1, "idx": 0, "ch": [
            {
                "vm": 2, "name": "one-level"
            },
            {
                "vm": 2, "name": "two-levels"
            },
            {
                "vm": 2, "name": "three-levels"
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
          05 root-table-1-count PIC S9(2) BINARY.
          05 root-table-1 OCCURS 10
                          DEPENDING ON root-table-1-count.
             10 var1 PIC X.
             10 var2 PIC X.
       01 two-levels.
          05 root-table-2-count   PIC S9(3) BINARY.
          05 table-level1-1-count PIC S9(2) BINARY.
          05 root-table-2 OCCURS 100
                          DEPENDING ON root-table-2-count.
             10 table-level1-1 OCCURS 20
                               DEPENDING ON table-level1-1-count.
                15 var3 PIC X.
                15 var4 PIC X.
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
                   20 var5 PIC X.
                   20 var6 PIC X.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D77 Idx-d4df4249-1 PIC 9(4) COMP.
      D77 Idx-d4df4249-2 PIC 9(3) COMP.
      D77 Idx-d4df4249-3 PIC 9(2) COMP.
      *</DBG>

       PROCEDURE DIVISION.
      *<DBG>InsertVariableDisplay 1959/09/18 11:09 TESTUSER
      D    DISPLAY 'one-level'
      D    DISPLAY '  root-table-1-count <' root-table-1-count '>'
      D    IF root-table-1-count >= 1 AND <= 10
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
      D              'NG ON "root-table-1-count" is out of range.'
      D    END-IF
      D    DISPLAY 'two-levels'
      D    DISPLAY '  root-table-2-count <' root-table-2-count '>'
      D    DISPLAY '  table-level1-1-count <' table-level1-1-count '>'
      D    IF root-table-2-count >= 1 AND <= 100
      D      PERFORM VARYING Idx-d4df4249-1 FROM 1 BY 1 UNTIL
      D      Idx-d4df4249-1 > root-table-2-count
      D        DISPLAY '  root-table-2 (' Idx-d4df4249-1 ')'
      D        IF table-level1-1-count >= 1 AND <= 20
      D          PERFORM VARYING Idx-d4df4249-2 FROM 1 BY 1 UNTIL
      D          Idx-d4df4249-2 > table-level1-1-count
      D            DISPLAY '    table-level1-1 (' Idx-d4df4249-1 ' '
      D            Idx-d4df4249-2 ')'
      D            DISPLAY '      var3 (' Idx-d4df4249-1 ' '
      D            Idx-d4df4249-2 ') <' var3 (Idx-d4df4249-1
      D            Idx-d4df4249-2) '>'
      D            DISPLAY '      var4 (' Idx-d4df4249-1 ' '
      D            Idx-d4df4249-2 ') <' var4 (Idx-d4df4249-1
      D            Idx-d4df4249-2) '>'
      D          END-PERFORM
      D        ELSE
      D          DISPLAY 'Cannot DISPLAY "table-level1-1" because its D'
      D                  'EPENDING ON "table-level1-1-count" is out of '
      D                  'range.'
      D        END-IF
      D      END-PERFORM
      D    ELSE
      D      DISPLAY 'Cannot DISPLAY "root-table-2" because its DEPENDI'
      D              'NG ON "root-table-2-count" is out of range.'
      D    END-IF
      D    DISPLAY 'three-levels'
      D    DISPLAY '  root-table-3-count <' root-table-3-count '>'
      D    DISPLAY '  table-level1-2-count <' table-level1-2-count '>'
      D    DISPLAY '  table-level2-1-count <' table-level2-1-count '>'
      D    IF root-table-3-count >= 1 AND <= 1000
      D      PERFORM VARYING Idx-d4df4249-1 FROM 1 BY 1 UNTIL
      D      Idx-d4df4249-1 > root-table-3-count
      D        DISPLAY '  root-table-3 (' Idx-d4df4249-1 ')'
      D        IF table-level1-2-count >= 1 AND <= 200
      D          PERFORM VARYING Idx-d4df4249-2 FROM 1 BY 1 UNTIL
      D          Idx-d4df4249-2 > table-level1-2-count
      D            DISPLAY '    table-level1-2 (' Idx-d4df4249-1 ' '
      D            Idx-d4df4249-2 ')'
      D            IF table-level2-1-count >= 1 AND <= 30
      D              PERFORM VARYING Idx-d4df4249-3 FROM 1 BY 1 UNTIL
      D              Idx-d4df4249-3 > table-level2-1-count
      D                DISPLAY '      table-level2-1 (' Idx-d4df4249-1
      D                ' ' Idx-d4df4249-2 ' ' Idx-d4df4249-3 ')'
      D                DISPLAY '        var5 (' Idx-d4df4249-1 ' '
      D                Idx-d4df4249-2 ' ' Idx-d4df4249-3 ') <' var5
      D                (Idx-d4df4249-1 Idx-d4df4249-2 Idx-d4df4249-3)
      D                '>'
      D                DISPLAY '        var6 (' Idx-d4df4249-1 ' '
      D                Idx-d4df4249-2 ' ' Idx-d4df4249-3 ') <' var6
      D                (Idx-d4df4249-1 Idx-d4df4249-2 Idx-d4df4249-3)
      D                '>'
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