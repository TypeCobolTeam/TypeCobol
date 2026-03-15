# PR3: Fix SQL Parser Hang via Grammar Catch-All Rule

## Problem

When `EnableSqlParsing=true` and the Scanner encounters an unsupported SQL statement (INSERT, UPDATE, DELETE, DECLARE CURSOR, OPEN, FETCH, CLOSE), ANTLR tokenizes SQL keywords individually then tries all 100+ `codeElement` alternatives. The error recovery loops exponentially, causing a parser hang (30s+ timeout instead of <1s).

## Previous Attempts

- **PR1** (feature branch `feature/sql-dml-cursor-statements`): Added full grammar rules for 7 DML statements. Accepted with reservations (parasitic renaming, encoding changes, missing tests).
- **PR2** (branch `fix/sql-parser-hang-unsupported-statements`): Filtered at Scanner level with a hardcoded keyword whitelist. Rejected: changes `InsideSql` semantics, hardcoded list to maintain.

## Solution: Grammar Catch-All Rule

Add an `unsupportedSqlStatement` rule as the **last SQL alternative** in `codeElement`. ANTLR tries supported rules first; the catch-all consumes remaining SQL tokens without triggering error recovery.

### Key Properties

- `InsideSql` keeps its original semantics (always `true` inside `EXEC SQL`)
- No hardcoded keyword list in the Scanner
- ANTLR handles parsing cleanly (no error recovery hack)
- Produces an informative AST node (`UnsupportedSqlStatement`) for diagnostics

## Components

### 1. Grammar (`CobolCodeElements.g4`)

Add after `executeImmediateStatement` in `codeElement` alternatives:

```antlr
| unsupportedSqlStatement
```

New rules:

```antlr
unsupportedSqlStatement: sqlUnrecognizedKeyword sqlToken*;
sqlUnrecognizedKeyword: SQL_INSERT | SQL_UPDATE | SQL_DELETE
                      | SQL_DECLARE | SQL_OPEN | SQL_FETCH | SQL_CLOSE;
sqlToken: ~(END_EXEC | EOF);
```

### 2. Scanner (`Scanner.cs`)

Revert `IsSupportedSqlStatement()` method and the conditional around `InsideSql = true`. Restore original behavior: `InsideSql` is always set to `true` after `EXEC SQL`.

### 3. AST Nodes (new files)

- `UnsupportedSqlStatement.cs` — CodeElement preserving raw token text
- `UnsupportedSql.cs` — AST Node

### 4. Infrastructure (existing files to modify)

- `CodeElementType.cs`: add `UnsupportedSqlStatement`
- `StatementElement.cs`: add `StatementType.UnsupportedSqlStatement`
- `SqlStatementElement.cs`: keep DML keywords in `IsSqlStatementStartingKeyword()` for error recovery boundaries
- `CobolCodeElementBuilder.cs`: add Enter/Exit visitor methods
- `SqlCodeElementBuilder.cs`: create CodeElement from parser context
- `CobolLanguageLevelVisitor.cs`: add visit delegation
- `IProgramClassBuilder.cs` / `ProgramClassBuilder.cs` / dispatchers: add interface methods
- `.cup` files: add production rules

### 5. Tests

- Unit tests in TypeCobol test framework (`.cbl` reference files with expected results)
- Validation on 12 COBOL files from `Triangulate/triangulate/test_fixtures/` (no hang, no memory leak)

## Files from Triangulate for Testing

```
test_fixtures/sample_cobol/BUFOVFLW.cbl
test_fixtures/sample_cobol/CPYUTIL.cpy
test_fixtures/sample_cobol/DEBUGPGM.cbl
test_fixtures/sample_cobol/HELLO.cbl
test_fixtures/sample_cobol/SQLCURS.cbl
test_fixtures/sample_cobol/SQLDML.cbl
test_fixtures/sample_cobol/SQLDML_SIMPLE.cbl
test_fixtures/sample_cobol/SQLPROG.cbl
test_fixtures/sample_cobol/SQLUPD.cbl
test_fixtures/sample_cobol/ZCALLPGM.cbl
test_fixtures/uast/synth_cobol.cbl
test_fixtures/uast/synth_cobol_sql.cbl
```
