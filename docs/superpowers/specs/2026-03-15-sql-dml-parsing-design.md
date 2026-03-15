# SQL DML Parsing for XREF CRUD Extraction

**Date:** 2026-03-15
**Status:** Draft
**Goal:** Parse embedded SQL DML statements (INSERT, UPDATE, DELETE, DECLARE CURSOR) in COBOL to extract table names, column names, and host variable bindings for XREF CRUD matrix generation and column-level data lineage in TriangulateSystemOfSystem.

## Context

TypeCobol's SQL parser currently supports SELECT, COMMIT, ROLLBACK, and several DDL/utility statements. INSERT, UPDATE, DELETE, DECLARE CURSOR, OPEN, FETCH, and CLOSE are caught by `UnsupportedSqlStatement`, which consumes tokens without extracting structured information.

The Triangulate ecosystem needs structured CRUD data with column bindings to feed:
- `xref_crud` table in TriangulateDB (with `column_bindings` JSONB)
- TriangulateSystemOfSystem's DFA engine for column-level data lineage in Neo4j

## Approach

**Phase A (Targeted Extraction):** Parse the 4 critical DML statements + enrich SELECT to extract tables, columns, and host variables.

**Phase C (Catch-all for the rest):** Keep `UnsupportedSqlStatement` for OPEN/FETCH/CLOSE and any SQL syntax not covered by Phase A.

## New CodeElements

### HostVariableBinding (shared structure)

```csharp
// TypeCobol/Compiler/Sql/CodeElements/HostVariableBinding.cs
public class HostVariableBinding
{
    public string ColumnName { get; set; }      // SQL column, nullable
    public string VariableName { get; set; }    // :WS-VAR
    public string IndicatorName { get; set; }   // :WS-IND, nullable
    public HostVariableDirection Direction { get; set; } // IN or OUT
}

public enum HostVariableDirection { IN, OUT }
```

### InsertStatement

```csharp
// TypeCobol/Compiler/Sql/CodeElements/Statements/InsertStatement.cs
public class InsertStatement : SqlStatementElement
{
    public string TableName { get; set; }                    // qualified: schema.table
    public IList<string> Columns { get; set; }               // explicit column list, nullable
    public IList<HostVariableBinding> HostVariables { get; set; }  // VALUES :vars
    public bool HasSubselect { get; set; }                   // INSERT INTO ... SELECT ...
}
```

Parses: `INSERT INTO schema.table (col1, col2) VALUES (:v1, :v2)` and `INSERT INTO table SELECT ...`

### UpdateStatement

```csharp
// TypeCobol/Compiler/Sql/CodeElements/Statements/UpdateStatement.cs
public class UpdateStatement : SqlStatementElement
{
    public string TableName { get; set; }
    public IList<HostVariableBinding> SetBindings { get; set; }    // SET col=:var pairs
    public IList<HostVariableBinding> WhereBindings { get; set; }  // WHERE host vars
}
```

Parses: `UPDATE table SET col1 = :v1, col2 = :v2 WHERE col3 = :v3`

### DeleteStatement

```csharp
// TypeCobol/Compiler/Sql/CodeElements/Statements/DeleteStatement.cs
public class DeleteStatement : SqlStatementElement
{
    public string TableName { get; set; }
    public IList<HostVariableBinding> WhereBindings { get; set; }
}
```

Parses: `DELETE FROM table WHERE col = :v1`

### DeclareCursorStatement

```csharp
// TypeCobol/Compiler/Sql/CodeElements/Statements/DeclareCursorStatement.cs
public class DeclareCursorStatement : SqlStatementElement
{
    public string CursorName { get; set; }
    public bool WithHold { get; set; }
    public bool WithReturn { get; set; }
    public SelectStatement InnerSelect { get; set; }  // reuses existing SelectStatement
    public string StatementName { get; set; }          // for DECLARE c CURSOR FOR stmt_name
}
```

### SelectStatement (enriched)

Add to existing `SelectStatement`:

```csharp
public IList<HostVariableBinding> IntoHostVariables { get; set; }  // SELECT ... INTO :v1, :v2
public IList<HostVariableBinding> WhereHostVariables { get; set; } // WHERE host vars
```

## Grammar Rules (CobolCodeElements.g4)

### Token-gobbling strategy for WHERE and VALUES

For clauses containing arbitrary SQL expressions (WHERE conditions, VALUES expressions, function calls), we use a **token-gobbling** approach: consume all tokens while extracting host variable references (`:identifier` pattern) without parsing expression structure.

```antlr
// New helper rules
tableNameQualified:
    (UserDefinedWord '.')? (UserDefinedWord '.')? UserDefinedWord;

hostVariable:
    ':' UserDefinedWord (':' UserDefinedWord)?;  // :VAR or :VAR:INDICATOR

columnList:
    '(' UserDefinedWord (',' UserDefinedWord)* ')';

// INSERT
insertStatement:
    SQL_INSERT SQL_INTO tableNameQualified
    columnList?
    (SQL_VALUES '(' insertValueList ')' | fullselect);

insertValueList:
    insertValue (',' insertValue)*;

insertValue:
    hostVariable | ~(',' | ')')+;  // host var or any expression tokens

// UPDATE
updateStatement:
    SQL_UPDATE tableNameQualified SQL_SET
    setClause (',' setClause)*
    whereClauseWithHostVars?;

setClause:
    UserDefinedWord '=' (hostVariable | ~(',' | SQL_WHERE | END_EXEC)+);

// DELETE
deleteStatement:
    SQL_DELETE SQL_FROM tableNameQualified
    whereClauseWithHostVars?;

// WHERE - gobbles tokens, extracts host variables
whereClauseWithHostVars:
    SQL_WHERE ~END_EXEC*;  // consumed as raw tokens, host vars extracted in builder

// DECLARE CURSOR
declareCursorStatement:
    SQL_DECLARE UserDefinedWord SQL_CURSOR
    (SQL_WITH (SQL_HOLD | SQL_RETURN))*
    SQL_FOR (fullselect | UserDefinedWord);
```

### Updated catch-all

```antlr
unsupportedSqlStatement:
    (SQL_OPEN | SQL_FETCH | SQL_CLOSE) ~END_EXEC*;
```

Removes SQL_INSERT, SQL_UPDATE, SQL_DELETE, SQL_DECLARE from the catch-all.

### Updated SELECT (enriched)

Add `INTO` clause support to the existing `selectStatement` / `subSelect` rule:

```antlr
intoClause:
    SQL_INTO hostVariable (',' hostVariable)*;

// Modify subSelect to include optional intoClause after select list
```

## AST Nodes

New nodes in `TypeCobol/Compiler/Sql/Nodes/`:

| Node Class | CodeElement | Statement interface |
|------------|-------------|-------------------|
| `Insert` | `InsertStatement` | `Statement` |
| `Update` | `UpdateStatement` | `Statement` |
| `Delete` | `DeleteStatement` | `Statement` |
| `DeclareCursor` | `DeclareCursorStatement` | `Statement` |

Each follows the existing pattern: `class Insert : GenericNode<InsertStatement>, Statement`

## Visitor Infrastructure

### ISqlVisitor additions

```csharp
bool Visit(Insert insert);
bool Visit(Update update);
bool Visit(Delete delete);
bool Visit(DeclareCursor declareCursor);
```

### IASTVisitor additions

```csharp
bool VisitInsert(Insert insert);
bool VisitUpdate(Update update);
bool VisitDelete(Delete delete);
bool VisitDeclareCursor(DeclareCursor declareCursor);
```

Plus corresponding updates to: `AbstractAstVisitor`, `CodeElementDispatcher`, `ProgramListener`, `SqlCodeElementBuilder`.

## Bridge Integration (XRefSerializer)

The XRefSerializer in `Triangulate/triangulate/bridge/TypeCobol.Bridge/` visits the new AST nodes:

### CRUD output per statement type

**InsertStatement:**
```json
{
  "table": "T_DELIVERY",
  "operation": "C",
  "column_bindings": [
    {"column": "DELIVERY_ID", "variable": "WS-DELIVERY-ID", "direction": "IN"},
    {"column": "STATUS", "variable": "WS-STATUS", "direction": "IN"}
  ]
}
```

**UpdateStatement:**
```json
{
  "table": "T_DELIVERY",
  "operation": "U",
  "column_bindings": [
    {"column": "STATUS", "variable": "WS-NEW-STATUS", "direction": "IN"},
    {"column": "DELIVERY_ID", "variable": "WS-DELIVERY-ID", "direction": "IN"}
  ]
}
```

**DeleteStatement:**
```json
{
  "table": "T_DELIVERY",
  "operation": "D",
  "column_bindings": [
    {"column": null, "variable": "WS-DELIVERY-ID", "direction": "IN"}
  ]
}
```

**SelectStatement (enriched):**
```json
{
  "table": "T_DELIVERY",
  "operation": "R",
  "column_bindings": [
    {"column": "DELIVERY_ID", "variable": "WS-DELIVERY-ID", "direction": "OUT"},
    {"column": "STATUS", "variable": "WS-STATUS", "direction": "OUT"},
    {"column": "DELIVERY_ID", "variable": "WS-DELIVERY-ID", "direction": "IN"}
  ]
}
```

**DeclareCursorStatement:** Same as its inner SelectStatement, with cursor name in scope.

## Testing Strategy

1. **Unit tests** in `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/` — new `.cbl` test files for each DML type with expected results
2. **Integration test** — `_test_sql_parse/Program.cs` on TriangulateSI COBOL files (STKWHS01.cbl and others)
3. **Bridge test** — Verify XRefSerializer produces correct JSON CRUD with column_bindings
4. **Regression** — Ensure existing 63 tests still pass

## Implementation Order

1. `HostVariableBinding` class + `HostVariableDirection` enum
2. Grammar rules in `.g4` for INSERT, UPDATE, DELETE, DECLARE CURSOR
3. Enrich SELECT grammar with INTO clause
4. CUP productions linking grammar → CodeElements
5. New CodeElement classes (InsertStatement, UpdateStatement, DeleteStatement, DeclareCursorStatement)
6. Enrich SelectStatement with IntoHostVariables, WhereHostVariables
7. New AST Nodes (Insert, Update, Delete, DeclareCursor)
8. Visitor/Builder/Dispatcher/Listener infrastructure
9. SqlCodeElementBuilder — extract structured data from parse trees
10. Update catch-all to remove INSERT/UPDATE/DELETE/DECLARE
11. Bridge XRefSerializer — consume new CodeElements for CRUD output
12. Unit tests for each DML type
13. Integration test on TriangulateSI files
14. Regression test (63 existing tests)

## Risks and Mitigations

| Risk | Mitigation |
|------|-----------|
| Grammar conflicts with existing rules | New rules are alternatives in `sqlStatement`; test incrementally |
| Complex SQL not covered (subqueries in WHERE, CASE expressions) | Token-gobbling consumes without parsing; host vars still extracted |
| ANTLR backtracking on ambiguous SQL | Keep rules simple; catch-all remains as fallback |
| Breaking existing tests | Run full test suite after each step |

## Out of Scope

- Full SQL expression parsing (CASE, COALESCE, arithmetic, etc.)
- JOIN syntax beyond simple table references in FROM
- MERGE statement
- CREATE/ALTER TABLE (already handled separately)
- Stored procedure CALL (different from COBOL CALL)
