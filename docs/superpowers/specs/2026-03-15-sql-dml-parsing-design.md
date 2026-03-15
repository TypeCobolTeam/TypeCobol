# SQL DML Parsing for XREF CRUD Extraction

**Date:** 2026-03-15
**Status:** Approved
**Goal:** Parse embedded SQL DML statements (INSERT, UPDATE, DELETE, DECLARE CURSOR) in COBOL to extract table names, column names, and host variable bindings for XREF CRUD matrix generation and column-level data lineage in TriangulateSystemOfSystem.

## Context

TypeCobol's SQL parser currently supports SELECT, COMMIT, ROLLBACK, and several DDL/utility statements. INSERT, UPDATE, DELETE, DECLARE CURSOR, OPEN, FETCH, and CLOSE are caught by `UnsupportedSqlStatement`, which consumes tokens without extracting structured information.

The Triangulate ecosystem needs structured CRUD data with column bindings to feed:
- `xref_crud` table in TriangulateDB (with `column_bindings` JSONB)
- TriangulateSystemOfSystem's DFA engine for column-level data lineage in Neo4j

## Approach

**Phase 1 (Targeted Extraction):** Parse the 4 critical DML statements + enrich SELECT to extract tables, columns, and host variables.

**Phase 2 (Catch-all for the rest):** Keep `UnsupportedSqlStatement` for OPEN/FETCH/CLOSE and any SQL syntax not covered by Phase 1.

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

Note: This is distinct from the existing `SqlVariable`/`hostVariable` grammar model. `HostVariableBinding` adds column-binding context (which column the variable maps to, and data flow direction) needed for XREF CRUD. The existing `hostVariable` grammar rule is reused for parsing.

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

### SqlDeleteStatement

```csharp
// TypeCobol/Compiler/Sql/CodeElements/Statements/SqlDeleteStatement.cs
public class SqlDeleteStatement : SqlStatementElement
{
    public string TableName { get; set; }
    public IList<HostVariableBinding> WhereBindings { get; set; }
}
```

Named `SqlDeleteStatement` to avoid collision with existing COBOL `DeleteStatement` (indexed file record deletion in `TypeCobol/Compiler/CodeElements/Statement/DeleteStatement.cs`). AST node named `SqlDelete`.

Parses: `DELETE FROM table WHERE col = :v1` and `DELETE FROM table WHERE CURRENT OF cursor_name`

### DeclareCursorStatement

```csharp
// TypeCobol/Compiler/Sql/CodeElements/Statements/DeclareCursorStatement.cs
public class DeclareCursorStatement : SqlStatementElement
{
    public string CursorName { get; set; }
    public bool WithHold { get; set; }
    public bool WithReturn { get; set; }
    public FullSelect InnerSelect { get; set; }  // reuses existing FullSelect model (not a CodeElement)
    public string StatementName { get; set; }     // for DECLARE c CURSOR FOR stmt_name
}
```

Note: `InnerSelect` holds a `FullSelect` model object, not a `SelectStatement` CodeElement. A CodeElement should not contain another CodeElement.

### SelectStatement (enriched)

Add to existing `SelectStatement`:

```csharp
public IList<HostVariableBinding> IntoHostVariables { get; set; }  // SELECT ... INTO :v1, :v2
public IList<HostVariableBinding> WhereHostVariables { get; set; } // WHERE host vars
```

## Grammar Rules (CobolCodeElements.g4)

### Design principles

- **Reuse existing rules:** `tableOrViewOrCorrelationName` (line 8434), `hostVariable` (line 8452), `fullselect` for table names, host variables, and subqueries.
- **Use correct token types:** `LeftParenthesisSeparator`, `RightParenthesisSeparator`, `SQL_CommaSeparator`, `ColonSeparator` (not literal characters).
- **Token-gobbling for expressions:** WHERE clauses and VALUES expressions consume tokens while creating sub-contexts for host variables.
- `IsSqlStatementStartingKeyword()` already includes SQL_INSERT, SQL_UPDATE, SQL_DELETE, SQL_DECLARE (done in prior commit).

### New rules

```antlr
// INSERT
insertStatement:
    SQL_INSERT SQL_INTO tableOrViewOrCorrelationName
    insertColumnList?
    (SQL_VALUES LeftParenthesisSeparator insertValueList RightParenthesisSeparator | fullselect);

insertColumnList:
    LeftParenthesisSeparator UserDefinedWord (SQL_CommaSeparator UserDefinedWord)* RightParenthesisSeparator;

insertValueList:
    insertValue (SQL_CommaSeparator insertValue)*;

insertValue:
    hostVariable | (~(SQL_CommaSeparator | RightParenthesisSeparator | ColonSeparator))+;

// UPDATE
updateStatement:
    SQL_UPDATE tableOrViewOrCorrelationName SQL_SET
    updateSetClause (SQL_CommaSeparator updateSetClause)*
    whereClauseWithHostVars?;

updateSetClause:
    UserDefinedWord EqualOperator (hostVariable | (~(SQL_CommaSeparator | SQL_WHERE | END_EXEC | ColonSeparator))+);

// DELETE (named sqlDeleteStatement to avoid collision with COBOL DELETE)
sqlDeleteStatement:
    SQL_DELETE SQL_FROM tableOrViewOrCorrelationName
    whereClauseWithHostVars?;

// WHERE - gobbles tokens, creates sub-contexts for host variables
whereClauseWithHostVars:
    SQL_WHERE (hostVariable | ~END_EXEC)*;

// DECLARE CURSOR
declareCursorStatement:
    SQL_DECLARE UserDefinedWord SQL_CURSOR
    (SQL_WITH (SQL_HOLD | SQL_RETURN))*
    SQL_FOR (fullselect | UserDefinedWord);

// INTO clause for SELECT enrichment
intoClause:
    SQL_INTO hostVariable (SQL_CommaSeparator hostVariable)*;
```

### Host variable extraction in WHERE

The `whereClauseWithHostVars` rule uses `(hostVariable | ~END_EXEC)*` to create sub-contexts for host variables within the gobbled tokens. The `SqlCodeElementBuilder` can then iterate over child contexts to find `hostVariable` instances without manual token-stream walking.

### Updated catch-all

```antlr
unsupportedSqlStatement:
    (SQL_OPEN | SQL_FETCH | SQL_CLOSE) ~END_EXEC*;
```

Removes SQL_INSERT, SQL_UPDATE, SQL_DELETE, SQL_DECLARE from the catch-all.

### Updated SELECT (enriched)

Add optional `intoClause` to the existing `subSelect` rule, after the select list and before the FROM clause.

## Integration Points

### 1. ANTLR Listener (CobolCodeElementBuilder.cs)

New `Enter*`/`Exit*` methods for each new grammar rule. The `ExitInsertStatement`, `ExitUpdateStatement`, etc. methods create the corresponding CodeElements by extracting data from the parse tree contexts.

### 2. CUP Parser (TypeCobolProgram.cup)

- Add new terminal declarations: `InsertStatement`, `UpdateStatement`, `SqlDeleteStatement`, `DeclareCursorStatement`
- Add these as new alternatives in the `sqlStatement` production rule
- Each alternative calls the corresponding AST node builder

### 3. CodeElementType and StatementType enums

Add new entries:
- `CodeElementType.InsertStatement`
- `CodeElementType.UpdateStatement`
- `CodeElementType.SqlDeleteStatement`
- `CodeElementType.DeclareCursorStatement`
- Corresponding `StatementType` entries

## AST Nodes

New nodes in `TypeCobol/Compiler/Sql/Nodes/`:

| Node Class | CodeElement | Statement interface |
|------------|-------------|-------------------|
| `Insert` | `InsertStatement` | `Statement` |
| `Update` | `UpdateStatement` | `Statement` |
| `SqlDelete` | `SqlDeleteStatement` | `Statement` |
| `DeclareCursor` | `DeclareCursorStatement` | `Statement` |

Each follows the existing pattern: `class Insert : GenericNode<InsertStatement>, Statement`

## Visitor Infrastructure

### ISqlVisitor additions

```csharp
bool Visit(Insert insert);
bool Visit(Update update);
bool Visit(SqlDelete sqlDelete);
bool Visit(DeclareCursor declareCursor);
```

### IASTVisitor additions

```csharp
bool VisitInsert(Insert insert);
bool VisitUpdate(Update update);
bool VisitSqlDelete(SqlDelete sqlDelete);
bool VisitDeclareCursor(DeclareCursor declareCursor);
```

Plus corresponding updates to: `AbstractAstVisitor`, `CodeElementDispatcher`, `ProgramListener`, `SqlCodeElementBuilder`.

## Bridge Integration (XRefSerializer)

The XRefSerializer in `Triangulate/triangulate/bridge/TypeCobol.Bridge/` visits the new AST nodes and produces CRUD entries:

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

**SqlDeleteStatement:**
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

**DeclareCursorStatement:** Same as its inner FullSelect, with cursor name in scope.

## Testing Strategy

1. **Unit tests** in `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/` -- new `.cbl` test files for each DML type with expected results
2. **Integration test** -- `_test_sql_parse/Program.cs` on TriangulateSI COBOL files (STKWHS01.cbl and others)
3. **Bridge test** -- Verify XRefSerializer produces correct JSON CRUD with column_bindings
4. **Regression** -- Ensure all existing tests still pass (`dotnet test` on TypeCobol.Test)

## Implementation Order

1. `HostVariableBinding` class + `HostVariableDirection` enum
2. `CodeElementType` and `StatementType` enum entries for new statements
3. Grammar rules in `.g4` for INSERT, UPDATE, sqlDeleteStatement, DECLARE CURSOR
4. Enrich SELECT grammar with `intoClause`
5. New CodeElement classes (InsertStatement, UpdateStatement, SqlDeleteStatement, DeclareCursorStatement)
6. Enrich SelectStatement with IntoHostVariables, WhereHostVariables
7. ANTLR listener methods in `CobolCodeElementBuilder.cs` -- create CodeElements from parse trees
8. `SqlCodeElementBuilder` -- extract structured data (tables, columns, host vars) from parse tree contexts
9. CUP terminal declarations and `sqlStatement` alternatives in `TypeCobolProgram.cup`
10. New AST Nodes (Insert, Update, SqlDelete, DeclareCursor)
11. Visitor/Builder/Dispatcher/Listener infrastructure (ISqlVisitor, IASTVisitor, AbstractAstVisitor, CodeElementDispatcher, ProgramListener)
12. Update catch-all to remove INSERT/UPDATE/DELETE/DECLARE
13. Bridge XRefSerializer -- consume new CodeElements for CRUD output
14. Unit tests for each DML type
15. Integration test on TriangulateSI files
16. Regression test (full test suite)

## Edge Cases

- `INSERT INTO table DEFAULT VALUES` -- no column list, no VALUES expressions. TableName extracted, no column bindings.
- `UPDATE table SET (col1, col2) = (val1, val2)` -- row-value expression syntax. Token-gobbling handles gracefully, columns extracted from left side.
- `DELETE FROM table WHERE CURRENT OF cursor_name` -- positioned DELETE referencing a cursor. TableName extracted, no host variable bindings.
- `DECLARE cursor SCROLL CURSOR` -- scrollable cursors. SCROLL keyword consumed between DECLARE and CURSOR.
- `INSERT INTO table (col1) VALUES (DEFAULT)` -- DEFAULT keyword in values. No host variable binding for that position.

## Risks and Mitigations

| Risk | Mitigation |
|------|-----------|
| Grammar conflicts with existing rules | New rules are alternatives in `sqlStatement`; test incrementally |
| Complex SQL not covered (subqueries in WHERE, CASE) | Token-gobbling with `(hostVariable \| ~END_EXEC)*` consumes without parsing; host vars still extracted |
| ANTLR ambiguity with ColonSeparator in negated sets | Explicitly exclude `ColonSeparator` from negated token sets so `hostVariable` alternative is unambiguous |
| `DeleteStatement` naming collision with COBOL DELETE | Named `SqlDeleteStatement` / `SqlDelete` to disambiguate |
| Breaking existing tests | Run full test suite after each step |

## Out of Scope

- Full SQL expression parsing (CASE, COALESCE, arithmetic, etc.)
- JOIN syntax beyond simple table references in FROM
- MERGE statement
- CREATE/ALTER TABLE (already handled separately)
- Stored procedure CALL (different from COBOL CALL)

## Key Files

| File | Role |
|------|------|
| `TypeCobol/AntlrGrammar/CobolCodeElements.g4` | ANTLR grammar (SQL rules at ~line 8406) |
| `TypeCobol/Compiler/Sql/CodeElements/SqlStatementElement.cs` | Base class for SQL CodeElements |
| `TypeCobol/Compiler/Sql/CodeElements/SqlCodeElementBuilder.cs` | Builder creating CodeElements from parse trees |
| `TypeCobol/Compiler/Sql/CodeElements/Statements/*.cs` | Individual SQL statement classes |
| `TypeCobol/Compiler/Sql/Nodes/*.cs` | AST node wrappers |
| `TypeCobol/Compiler/Sql/VisitorPattern.cs` | ISqlVisitor interface |
| `TypeCobol/Compiler/CodeElements/CodeElementType.cs` | CodeElementType enum |
| `TypeCobol/Compiler/CodeElements/StatementElement.cs` | StatementType enum |
| `TypeCobol/Compiler/Parser/CodeElementBuilder/CobolCodeElementBuilder.cs` | ANTLR listener wiring |
| `TypeCobol/Compiler/CupParser/TypeCobolProgram.cup` | CUP parser (terminals + sqlStatement rule) |
| `TypeCobol/Compiler/CodeElements/CobolLanguageLevelVisitor.cs` | IASTVisitor interface |
