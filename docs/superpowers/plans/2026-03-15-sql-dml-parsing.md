# SQL DML Parsing Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Parse INSERT, UPDATE, DELETE, DECLARE CURSOR in COBOL embedded SQL to extract table names, columns, and host variable bindings for XREF CRUD matrices.

**Architecture:** Extend TypeCobol's ANTLR SQL grammar with 4 new DML rules, create corresponding CodeElements/Nodes, wire them through the CUP parser and visitor infrastructure, then update the catch-all to only cover OPEN/FETCH/CLOSE.

**Tech Stack:** C# / .NET 8, ANTLR4, CUP parser, TypeCobol compiler infrastructure

**Spec:** `docs/superpowers/specs/2026-03-15-sql-dml-parsing-design.md`

**Out of scope for this PR:** Bridge XRefSerializer update (will be done in a follow-up PR in the Triangulate repo once TypeCobol changes are merged).

---

## Chunk 1: Foundation — Enums + Nodes + Visitors (build-safe order)

Tasks are ordered so the build compiles at every commit. Nodes and visitor stubs are created BEFORE CodeElements that reference them.

### Task 1: Create HostVariableBinding shared structure

**Files:**
- Create: `TypeCobol/Compiler/Sql/CodeElements/HostVariableBinding.cs`

- [ ] **Step 1: Create HostVariableBinding.cs**

```csharp
using System.Collections.Generic;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    public enum HostVariableDirection
    {
        IN,
        OUT
    }

    public class HostVariableBinding
    {
        public string ColumnName { get; set; }
        public string VariableName { get; set; }
        public string IndicatorName { get; set; }
        public HostVariableDirection Direction { get; set; }

        public HostVariableBinding(string variableName, HostVariableDirection direction, string columnName = null, string indicatorName = null)
        {
            VariableName = variableName;
            Direction = direction;
            ColumnName = columnName;
            IndicatorName = indicatorName;
        }
    }
}
```

- [ ] **Step 2: Build to verify**

Run: `dotnet build TypeCobol/TypeCobol.csproj -c Debug --verbosity quiet 2>&1 | tail -3`
Expected: 0 errors

- [ ] **Step 3: Commit**

```bash
git add TypeCobol/Compiler/Sql/CodeElements/HostVariableBinding.cs
git commit -m "feat: add HostVariableBinding class for SQL column-variable mapping"
```

### Task 2: Add CodeElementType and StatementType enum entries

**Files:**
- Modify: `TypeCobol/Compiler/CodeElements/CodeElementType.cs:179` (before UnsupportedSqlStatement)
- Modify: `TypeCobol/Compiler/CodeElements/StatementElement.cs:147` (before UnsupportedSqlStatement)

- [ ] **Step 1: Add enum entries to CodeElementType.cs**

After `ExecuteImmediateStatement,` (line 178), before `UnsupportedSqlStatement,` (line 179), add:

```csharp
        InsertStatement,
        UpdateStatement,
        SqlDeleteStatement,
        DeclareCursorStatement,
```

- [ ] **Step 2: Add enum entries to StatementElement.cs**

After `ExecuteImmediateStatement,` (line 146), before `UnsupportedSqlStatement` (line 147), add:

```csharp
        InsertStatement,
        UpdateStatement,
        SqlDeleteStatement,
        DeclareCursorStatement,
```

- [ ] **Step 3: Build and commit**

```bash
dotnet build TypeCobol/TypeCobol.csproj -c Debug --verbosity quiet 2>&1 | tail -3
git add TypeCobol/Compiler/CodeElements/CodeElementType.cs TypeCobol/Compiler/CodeElements/StatementElement.cs
git commit -m "feat: add CodeElementType and StatementType entries for INSERT/UPDATE/DELETE/DECLARE CURSOR"
```

### Task 3: Create AST Nodes (Insert, Update, SqlDelete, DeclareCursor)

Create nodes BEFORE CodeElements so the IASTVisitor stubs can reference them.

**Files:**
- Create: `TypeCobol/Compiler/Sql/Nodes/Insert.cs`
- Create: `TypeCobol/Compiler/Sql/Nodes/Update.cs`
- Create: `TypeCobol/Compiler/Sql/Nodes/SqlDelete.cs`
- Create: `TypeCobol/Compiler/Sql/Nodes/DeclareCursor.cs`

Follow existing pattern from `TypeCobol/Compiler/Sql/Nodes/Select.cs`.

- [ ] **Step 1: Create Insert.cs**

```csharp
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    public class Insert : GenericNode<InsertStatement>, Statement
    {
        public Insert(InsertStatement statement) : base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}
```

- [ ] **Step 2: Create Update.cs**

```csharp
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    public class Update : GenericNode<UpdateStatement>, Statement
    {
        public Update(UpdateStatement statement) : base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}
```

- [ ] **Step 3: Create SqlDelete.cs**

```csharp
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    public class SqlDelete : GenericNode<SqlDeleteStatement>, Statement
    {
        public SqlDelete(SqlDeleteStatement statement) : base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}
```

- [ ] **Step 4: Create DeclareCursor.cs**

```csharp
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    public class DeclareCursor : GenericNode<DeclareCursorStatement>, Statement
    {
        public DeclareCursor(DeclareCursorStatement statement) : base(statement) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}
```

Note: These will not compile yet because the CodeElement classes (InsertStatement etc.) don't exist. That's fine — they will be created in Task 5 and everything compiles together at Task 6.

- [ ] **Step 5: Commit (WIP)**

```bash
git add TypeCobol/Compiler/Sql/Nodes/Insert.cs TypeCobol/Compiler/Sql/Nodes/Update.cs TypeCobol/Compiler/Sql/Nodes/SqlDelete.cs TypeCobol/Compiler/Sql/Nodes/DeclareCursor.cs
git commit -m "feat(wip): add AST nodes Insert, Update, SqlDelete, DeclareCursor"
```

### Task 4: Add Visit methods to IASTVisitor and AbstractAstVisitor

**Files:**
- Modify: `TypeCobol/Compiler/CodeElements/CobolLanguageLevelVisitor.cs`
  - IASTVisitor interface: after line 416 (`bool Visit([NotNull] UnsupportedSql unsupportedSql);`)
  - AbstractAstVisitor class: after line ~1676 (Visit UnsupportedSql implementation)

- [ ] **Step 1: Add usings at top of file (if not already present)**

```csharp
using TypeCobol.Compiler.Sql.CodeElements.Statements;
using TypeCobol.Compiler.Sql.Nodes;
```

- [ ] **Step 2: Add to IASTVisitor interface**

After `bool Visit([NotNull] UnsupportedSql unsupportedSql);` (line 416), add:

```csharp
        bool Visit([NotNull] InsertStatement insertStatement);
        bool Visit([NotNull] Insert insert);
        bool Visit([NotNull] UpdateStatement updateStatement);
        bool Visit([NotNull] Update update);
        bool Visit([NotNull] SqlDeleteStatement sqlDeleteStatement);
        bool Visit([NotNull] SqlDelete sqlDelete);
        bool Visit([NotNull] DeclareCursorStatement declareCursorStatement);
        bool Visit([NotNull] DeclareCursor declareCursor);
```

- [ ] **Step 3: Add default implementations to AbstractAstVisitor**

After the `Visit(UnsupportedSql)` method (~line 1676), add:

```csharp
        public virtual bool Visit([NotNull] InsertStatement insertStatement)
        {
            return true;
        }
        public virtual bool Visit([NotNull] Insert insert)
        {
            return true;
        }
        public virtual bool Visit([NotNull] UpdateStatement updateStatement)
        {
            return true;
        }
        public virtual bool Visit([NotNull] Update update)
        {
            return true;
        }
        public virtual bool Visit([NotNull] SqlDeleteStatement sqlDeleteStatement)
        {
            return true;
        }
        public virtual bool Visit([NotNull] SqlDelete sqlDelete)
        {
            return true;
        }
        public virtual bool Visit([NotNull] DeclareCursorStatement declareCursorStatement)
        {
            return true;
        }
        public virtual bool Visit([NotNull] DeclareCursor declareCursor)
        {
            return true;
        }
```

- [ ] **Step 4: Commit (WIP)**

```bash
git add TypeCobol/Compiler/CodeElements/CobolLanguageLevelVisitor.cs
git commit -m "feat(wip): add IASTVisitor methods for INSERT/UPDATE/DELETE/DECLARE CURSOR"
```

### Task 5: Create CodeElement classes (InsertStatement, UpdateStatement, SqlDeleteStatement, DeclareCursorStatement)

**Files:**
- Create: `TypeCobol/Compiler/Sql/CodeElements/Statements/InsertStatement.cs`
- Create: `TypeCobol/Compiler/Sql/CodeElements/Statements/UpdateStatement.cs`
- Create: `TypeCobol/Compiler/Sql/CodeElements/Statements/SqlDeleteStatement.cs`
- Create: `TypeCobol/Compiler/Sql/CodeElements/Statements/DeclareCursorStatement.cs`

- [ ] **Step 1: Create InsertStatement.cs**

```csharp
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class InsertStatement : SqlStatementElement
    {
        public string TableName { get; }
        public IList<string> Columns { get; }
        public IList<HostVariableBinding> HostVariables { get; }
        public bool HasSubselect { get; }

        public InsertStatement(string tableName, IList<string> columns, IList<HostVariableBinding> hostVariables, bool hasSubselect)
            : base(CodeElementType.InsertStatement, StatementType.InsertStatement)
        {
            TableName = tableName;
            Columns = columns;
            HostVariables = hostVariables ?? new List<HostVariableBinding>();
            HasSubselect = hasSubselect;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}
```

- [ ] **Step 2: Create UpdateStatement.cs**

```csharp
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class UpdateStatement : SqlStatementElement
    {
        public string TableName { get; }
        public IList<HostVariableBinding> SetBindings { get; }
        public IList<HostVariableBinding> WhereBindings { get; }

        public UpdateStatement(string tableName, IList<HostVariableBinding> setBindings, IList<HostVariableBinding> whereBindings)
            : base(CodeElementType.UpdateStatement, StatementType.UpdateStatement)
        {
            TableName = tableName;
            SetBindings = setBindings ?? new List<HostVariableBinding>();
            WhereBindings = whereBindings ?? new List<HostVariableBinding>();
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}
```

- [ ] **Step 3: Create SqlDeleteStatement.cs**

Named `SqlDeleteStatement` to avoid collision with existing COBOL `DeleteStatement` at `TypeCobol/Compiler/CodeElements/Statement/DeleteStatement.cs`.

```csharp
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class SqlDeleteStatement : SqlStatementElement
    {
        public string TableName { get; }
        public IList<HostVariableBinding> WhereBindings { get; }

        public SqlDeleteStatement(string tableName, IList<HostVariableBinding> whereBindings)
            : base(CodeElementType.SqlDeleteStatement, StatementType.SqlDeleteStatement)
        {
            TableName = tableName;
            WhereBindings = whereBindings ?? new List<HostVariableBinding>();
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}
```

- [ ] **Step 4: Create DeclareCursorStatement.cs**

```csharp
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class DeclareCursorStatement : SqlStatementElement
    {
        public string CursorName { get; }
        public bool WithHold { get; }
        public bool WithReturn { get; }
        public FullSelect InnerSelect { get; }
        public string StatementName { get; }

        public DeclareCursorStatement(string cursorName, FullSelect innerSelect, string statementName = null, bool withHold = false, bool withReturn = false)
            : base(CodeElementType.DeclareCursorStatement, StatementType.DeclareCursorStatement)
        {
            CursorName = cursorName;
            InnerSelect = innerSelect;
            StatementName = statementName;
            WithHold = withHold;
            WithReturn = withReturn;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && (InnerSelect == null || astVisitor.SqlVisitor == null || astVisitor.SqlVisitor.ContinueVisit(InnerSelect));
        }
    }
}
```

- [ ] **Step 5: Build to verify ALL WIP commits compile together**

Run: `dotnet build TypeCobol/TypeCobol.csproj -c Debug --verbosity quiet 2>&1 | tail -5`
Expected: 0 errors — Nodes reference CodeElements, CodeElements reference IASTVisitor, IASTVisitor references Nodes. All exist now.

- [ ] **Step 6: Commit**

```bash
git add TypeCobol/Compiler/Sql/CodeElements/Statements/InsertStatement.cs TypeCobol/Compiler/Sql/CodeElements/Statements/UpdateStatement.cs TypeCobol/Compiler/Sql/CodeElements/Statements/SqlDeleteStatement.cs TypeCobol/Compiler/Sql/CodeElements/Statements/DeclareCursorStatement.cs
git commit -m "feat: add CodeElements for INSERT, UPDATE, SqlDELETE, DECLARE CURSOR"
```

### Task 6: Enrich SelectStatement with INTO and WHERE host variables

**Files:**
- Modify: `TypeCobol/Compiler/Sql/CodeElements/Statements/SelectStatement.cs`

- [ ] **Step 1: Rewrite SelectStatement.cs**

Replace full content of `TypeCobol/Compiler/Sql/CodeElements/Statements/SelectStatement.cs`:

```csharp
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class SelectStatement : SqlStatementElement
    {
        public FullSelect FullSelect { get; }
        public IList<HostVariableBinding> IntoHostVariables { get; }
        public IList<HostVariableBinding> WhereHostVariables { get; }

        public SelectStatement(FullSelect fullSelect, IList<HostVariableBinding> intoHostVariables = null, IList<HostVariableBinding> whereHostVariables = null)
            : base(CodeElementType.SelectStatement, StatementType.SelectStatement)
        {
            FullSelect = fullSelect;
            IntoHostVariables = intoHostVariables ?? new List<HostVariableBinding>();
            WhereHostVariables = whereHostVariables ?? new List<HostVariableBinding>();
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(FullSelect);
        }
    }
}
```

- [ ] **Step 2: Build and commit**

```bash
dotnet build TypeCobol/TypeCobol.csproj -c Debug --verbosity quiet 2>&1 | tail -3
git add TypeCobol/Compiler/Sql/CodeElements/Statements/SelectStatement.cs
git commit -m "feat: enrich SelectStatement with IntoHostVariables and WhereHostVariables"
```

## Chunk 2: Infrastructure Wiring — CUP + Builders + Dispatcher + Listener

### Task 7: Wire CUP terminals and sqlStatement alternatives

**Files:**
- Modify: `TypeCobol/Compiler/CupParser/TypeCobolProgram.cup`

- [ ] **Step 1: Add terminal declarations**

After line 250 (`terminal ... ExecuteImmediateStatement ExecuteImmediateStatement;`), before line 251 (`terminal ... UnsupportedSqlStatement UnsupportedSqlStatement;`), add:

```
terminal TypeCobol.Compiler.Sql.CodeElements.Statements.InsertStatement InsertStatement;
terminal TypeCobol.Compiler.Sql.CodeElements.Statements.UpdateStatement UpdateStatement;
terminal TypeCobol.Compiler.Sql.CodeElements.Statements.SqlDeleteStatement SqlDeleteStatement;
terminal TypeCobol.Compiler.Sql.CodeElements.Statements.DeclareCursorStatement DeclareCursorStatement;
```

- [ ] **Step 2: Add sqlStatement alternatives**

After line 834 (`{: my_parser.Builder.OnExecuteImmediateStatement(stmt); :}`), before line 835 (`| UnsupportedSqlStatement:stmt`), add:

```
|	InsertStatement:stmt
{: my_parser.Builder.OnInsertStatement(stmt); :}
|	UpdateStatement:stmt
{: my_parser.Builder.OnUpdateStatement(stmt); :}
|	SqlDeleteStatement:stmt
{: my_parser.Builder.OnSqlDeleteStatement(stmt); :}
|	DeclareCursorStatement:stmt
{: my_parser.Builder.OnDeclareCursorStatement(stmt); :}
```

- [ ] **Step 3: Commit (WIP — Builder On* methods don't exist yet)**

```bash
git add TypeCobol/Compiler/CupParser/TypeCobolProgram.cup
git commit -m "feat(wip): add CUP terminals and sqlStatement alternatives for DML"
```

### Task 8: Wire IProgramClassBuilder + ProgramClassBuilder + Dispatcher + Listener

**Files:**
- Modify: `TypeCobol/Compiler/CupParser/NodeBuilder/IProgramClassBuilder.cs` (~line 892)
- Modify: `TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilder.cs` (~line 1806)
- Modify: `TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeDispatcher.cs` (~line 912)
- Modify: `TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeListener.cs` (~line 900)

- [ ] **Step 1: Add to IProgramClassBuilder.cs**

After `void OnUnsupportedSqlStatement(...)` (line 892), add:

```csharp
        void OnInsertStatement([NotNull] InsertStatement insert);
        void OnUpdateStatement([NotNull] UpdateStatement update);
        void OnSqlDeleteStatement([NotNull] SqlDeleteStatement sqlDelete);
        void OnDeclareCursorStatement([NotNull] DeclareCursorStatement declareCursor);
```

Add using: `using TypeCobol.Compiler.Sql.CodeElements.Statements;` (if not already present).

- [ ] **Step 2: Add to ProgramClassBuilder.cs**

After `OnUnsupportedSqlStatement` method (line ~1806), add:

```csharp
        public void OnInsertStatement([NotNull] InsertStatement insert)
        {
            Enter(new Insert(insert), insert);
            Exit();
            Dispatcher.OnInsertStatement(insert);
        }
        public void OnUpdateStatement([NotNull] UpdateStatement update)
        {
            Enter(new Update(update), update);
            Exit();
            Dispatcher.OnUpdateStatement(update);
        }
        public void OnSqlDeleteStatement([NotNull] SqlDeleteStatement sqlDelete)
        {
            Enter(new SqlDelete(sqlDelete), sqlDelete);
            Exit();
            Dispatcher.OnSqlDeleteStatement(sqlDelete);
        }
        public void OnDeclareCursorStatement([NotNull] DeclareCursorStatement declareCursor)
        {
            Enter(new DeclareCursor(declareCursor), declareCursor);
            Exit();
            Dispatcher.OnDeclareCursorStatement(declareCursor);
        }
```

Add usings:
```csharp
using TypeCobol.Compiler.Sql.CodeElements.Statements;
using TypeCobol.Compiler.Sql.Nodes;
```

- [ ] **Step 3: Add to ProgramClassBuilderNodeDispatcher.cs**

After `OnUnsupportedSqlStatement` method (~line 912), add:

```csharp
        public void OnInsertStatement([NotNull] InsertStatement insert)
        {
            foreach (var listener in _listeners) listener.OnInsertStatement(insert);
        }
        public void OnUpdateStatement([NotNull] UpdateStatement update)
        {
            foreach (var listener in _listeners) listener.OnUpdateStatement(update);
        }
        public void OnSqlDeleteStatement([NotNull] SqlDeleteStatement sqlDelete)
        {
            foreach (var listener in _listeners) listener.OnSqlDeleteStatement(sqlDelete);
        }
        public void OnDeclareCursorStatement([NotNull] DeclareCursorStatement declareCursor)
        {
            foreach (var listener in _listeners) listener.OnDeclareCursorStatement(declareCursor);
        }
```

- [ ] **Step 4: Add empty stubs to ProgramClassBuilderNodeListener.cs**

**CRITICAL:** This file implements `IProgramClassBuilderNodeListener` which extends `IProgramClassBuilder`. Without these stubs, the build will fail.

After `OnUnsupportedSqlStatement` method (~line 900), add:

```csharp
        public void OnInsertStatement([NotNull] InsertStatement insert) { }
        public void OnUpdateStatement([NotNull] UpdateStatement update) { }
        public void OnSqlDeleteStatement([NotNull] SqlDeleteStatement sqlDelete) { }
        public void OnDeclareCursorStatement([NotNull] DeclareCursorStatement declareCursor) { }
```

Add using: `using TypeCobol.Compiler.Sql.CodeElements.Statements;` (if not already present).

- [ ] **Step 5: Build to verify**

Run: `dotnet build TypeCobol/TypeCobol.csproj -c Debug --verbosity quiet 2>&1 | tail -5`
Expected: 0 errors

- [ ] **Step 6: Commit**

```bash
git add TypeCobol/Compiler/CupParser/NodeBuilder/IProgramClassBuilder.cs TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilder.cs TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeDispatcher.cs TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeListener.cs
git commit -m "feat: wire IProgramClassBuilder, ProgramClassBuilder, Dispatcher, Listener for DML"
```

## Chunk 3: ANTLR Grammar + Builder + ANTLR Listener

### Task 9: Add ANTLR grammar rules for INSERT, UPDATE, DELETE, DECLARE CURSOR

**Files:**
- Modify: `TypeCobol/AntlrGrammar/CobolCodeElements.g4`

- [ ] **Step 1: Add new grammar rules**

Insert before line 8534 (`// Catch-all rule for SQL statements...`):

```antlr
// INSERT statement
// See Documentation [https://www.ibm.com/docs/en/db2-for-zos/12?topic=statements-insert]
insertStatement:
    SQL_INSERT SQL_INTO tableOrViewOrCorrelationName
    insertColumnList?
    (SQL_VALUES LeftParenthesisSeparator insertValueList RightParenthesisSeparator | fullselect);

insertColumnList:
    LeftParenthesisSeparator column_name (SQL_CommaSeparator column_name)* RightParenthesisSeparator;

insertValueList:
    insertValue (SQL_CommaSeparator insertValue)*;

insertValue:
    hostVariable | (~(SQL_CommaSeparator | RightParenthesisSeparator | ColonSeparator))+;

// UPDATE statement
// See Documentation [https://www.ibm.com/docs/en/db2-for-zos/12?topic=statements-update]
updateStatement:
    SQL_UPDATE tableOrViewOrCorrelationName SQL_SET
    updateSetClause (SQL_CommaSeparator updateSetClause)*
    whereClauseWithHostVars?;

updateSetClause:
    column_name EqualOperator (hostVariable | (~(SQL_CommaSeparator | SQL_WHERE | END_EXEC | ColonSeparator))+);

// DELETE statement (named sqlDeleteStatement to avoid COBOL DELETE collision)
// See Documentation [https://www.ibm.com/docs/en/db2-for-zos/12?topic=statements-delete]
sqlDeleteStatement:
    SQL_DELETE SQL_FROM tableOrViewOrCorrelationName
    whereClauseWithHostVars?;

// WHERE clause - gobbles tokens, creates sub-contexts for host variables
// ColonSeparator is excluded from the negated set so hostVariable gets priority
whereClauseWithHostVars:
    SQL_WHERE (hostVariable | ~(END_EXEC | ColonSeparator))*;

// DECLARE CURSOR statement
// See Documentation [https://www.ibm.com/docs/en/db2-for-zos/12?topic=statements-declare-cursor]
declareCursorStatement:
    SQL_DECLARE cursorName=UserDefinedWord SQL_CURSOR
    (SQL_WITH SQL_HOLD)? (SQL_WITH SQL_RETURN)?
    SQL_FOR (fullselect | statementName=UserDefinedWord);

// INTO clause for SELECT enrichment
intoClause: SQL_INTO hostVariable (SQL_CommaSeparator hostVariable)*;
```

- [ ] **Step 2: Modify subselect to include intoClause and whereClauseWithHostVars**

Change line 8425 from:
```antlr
subselect: sql_selectClause from_clause;
```
to:
```antlr
subselect: sql_selectClause intoClause? from_clause whereClauseWithHostVars?;
```

- [ ] **Step 3: Update catch-all rule (lines 8537-8539)**

Replace:
```antlr
unsupportedSqlStatement:
    ( SQL_INSERT | SQL_UPDATE | SQL_DELETE
    | SQL_DECLARE | SQL_OPEN | SQL_FETCH | SQL_CLOSE ) ~END_EXEC*;
```
With:
```antlr
unsupportedSqlStatement:
    ( SQL_OPEN | SQL_FETCH | SQL_CLOSE ) ~END_EXEC*;
```

- [ ] **Step 4: Add new rules as alternatives in codeElement production**

Find the section where SQL alternatives are listed (~lines 217-231). Add before `unsupportedSqlStatement`:

```antlr
    | insertStatement
    | updateStatement
    | sqlDeleteStatement
    | declareCursorStatement
```

These MUST appear BEFORE `unsupportedSqlStatement` since ANTLR tries alternatives in order.

- [ ] **Step 5: Commit**

```bash
git add TypeCobol/AntlrGrammar/CobolCodeElements.g4
git commit -m "feat: add ANTLR grammar rules for INSERT/UPDATE/DELETE/DECLARE CURSOR/SELECT INTO"
```

### Task 10: Add SqlCodeElementBuilder Create methods

**Files:**
- Modify: `TypeCobol/Compiler/Sql/CodeElements/SqlCodeElementBuilder.cs`

- [ ] **Step 1: Add Create methods and helpers**

After `CreateUnsupportedSqlStatement` (line 946), add:

```csharp
        public InsertStatement CreateInsertStatement(CodeElementsParser.InsertStatementContext context)
        {
            string tableName = ExtractQualifiedTableName(context.tableOrViewOrCorrelationName());

            List<string> columns = null;
            if (context.insertColumnList() != null)
            {
                columns = new List<string>();
                foreach (var col in context.insertColumnList().column_name())
                {
                    columns.Add(col.GetText());
                }
            }

            var hostVariables = new List<HostVariableBinding>();
            bool hasSubselect = false;

            if (context.insertValueList() != null)
            {
                int colIndex = 0;
                foreach (var value in context.insertValueList().insertValue())
                {
                    if (value.hostVariable() != null)
                    {
                        string colName = columns != null && colIndex < columns.Count ? columns[colIndex] : null;
                        var binding = CreateHostVariableBinding(value.hostVariable(), HostVariableDirection.IN, colName);
                        hostVariables.Add(binding);
                    }
                    colIndex++;
                }
            }
            else if (context.fullselect() != null)
            {
                hasSubselect = true;
            }

            return new InsertStatement(tableName, columns, hostVariables, hasSubselect);
        }

        public UpdateStatement CreateUpdateStatement(CodeElementsParser.UpdateStatementContext context)
        {
            string tableName = ExtractQualifiedTableName(context.tableOrViewOrCorrelationName());

            var setBindings = new List<HostVariableBinding>();
            foreach (var setClause in context.updateSetClause())
            {
                string colName = setClause.column_name()?.GetText();
                if (setClause.hostVariable() != null)
                {
                    var binding = CreateHostVariableBinding(setClause.hostVariable(), HostVariableDirection.IN, colName);
                    setBindings.Add(binding);
                }
            }

            var whereBindings = ExtractWhereHostVariables(context.whereClauseWithHostVars());
            return new UpdateStatement(tableName, setBindings, whereBindings);
        }

        public SqlDeleteStatement CreateSqlDeleteStatement(CodeElementsParser.SqlDeleteStatementContext context)
        {
            string tableName = ExtractQualifiedTableName(context.tableOrViewOrCorrelationName());
            var whereBindings = ExtractWhereHostVariables(context.whereClauseWithHostVars());
            return new SqlDeleteStatement(tableName, whereBindings);
        }

        public DeclareCursorStatement CreateDeclareCursorStatement(CodeElementsParser.DeclareCursorStatementContext context)
        {
            string cursorName = context.cursorName?.Text;
            bool withHold = context.SQL_HOLD() != null && context.SQL_HOLD().Length > 0;
            bool withReturn = context.SQL_RETURN() != null && context.SQL_RETURN().Length > 0;

            FullSelect innerSelect = null;
            string statementName = null;

            if (context.fullselect() != null)
            {
                innerSelect = CreateFullSelect(context.fullselect());
            }
            else if (context.statementName != null)
            {
                statementName = context.statementName.Text;
            }

            return new DeclareCursorStatement(cursorName, innerSelect, statementName, withHold, withReturn);
        }

        private string ExtractQualifiedTableName(CodeElementsParser.TableOrViewOrCorrelationNameContext context)
        {
            if (context == null) return "UNKNOWN";
            string name = context.Name?.Text ?? "UNKNOWN";
            if (context.SchemaName != null)
                name = context.SchemaName.Text + "." + name;
            if (context.DBMS != null)
                name = context.DBMS.Text + "." + name;
            return name;
        }

        private HostVariableBinding CreateHostVariableBinding(CodeElementsParser.HostVariableContext context, HostVariableDirection direction, string columnName = null)
        {
            string varName = context.mainVariable?.Text;
            string indName = context.indicatorVariable?.Text;
            return new HostVariableBinding(varName, direction, columnName, indName);
        }

        private List<HostVariableBinding> ExtractWhereHostVariables(CodeElementsParser.WhereClauseWithHostVarsContext context)
        {
            var bindings = new List<HostVariableBinding>();
            if (context == null) return bindings;
            foreach (var hvCtx in context.hostVariable())
            {
                var binding = CreateHostVariableBinding(hvCtx, HostVariableDirection.IN);
                bindings.Add(binding);
            }
            return bindings;
        }
```

- [ ] **Step 2: Update CreateSelectStatement to extract INTO and WHERE host variables**

Replace the existing `CreateSelectStatement` method (lines 87-96):

```csharp
        public SelectStatement CreateSelectStatement(CodeElementsParser.SelectStatementContext context)
        {
            FullSelect fullSelect = null;
            if (context.fullselect() != null)
            {
                fullSelect = CreateFullSelect(context.fullselect());
            }

            var intoVars = new List<HostVariableBinding>();
            var whereVars = new List<HostVariableBinding>();

            var subSelectCtx = context.fullselect()?.subselect();
            if (subSelectCtx != null)
            {
                if (subSelectCtx.intoClause() != null)
                {
                    var selections = subSelectCtx.sql_selectClause()?.selections()?.selection();
                    int colIndex = 0;
                    foreach (var hvCtx in subSelectCtx.intoClause().hostVariable())
                    {
                        string colName = null;
                        if (selections != null && colIndex < selections.Length)
                        {
                            colName = selections[colIndex].GetText();
                        }
                        var binding = CreateHostVariableBinding(hvCtx, HostVariableDirection.OUT, colName);
                        intoVars.Add(binding);
                        colIndex++;
                    }
                }
                whereVars = ExtractWhereHostVariables(subSelectCtx.whereClauseWithHostVars());
            }

            return new SelectStatement(fullSelect, intoVars, whereVars);
        }
```

- [ ] **Step 3: Add using for HostVariableBinding**

Add at top of file: `using TypeCobol.Compiler.Sql.CodeElements;` (the namespace where HostVariableBinding lives — may not be needed if already in that namespace). Check the file's namespace is `TypeCobol.Compiler.Sql.CodeElements` — if so, no additional using needed.

- [ ] **Step 4: Build to verify**

Run: `dotnet build TypeCobol/TypeCobol.csproj -c Debug --verbosity quiet 2>&1 | tail -5`
Expected: 0 errors

- [ ] **Step 5: Commit**

```bash
git add TypeCobol/Compiler/Sql/CodeElements/SqlCodeElementBuilder.cs
git commit -m "feat: add SqlCodeElementBuilder Create methods for all DML + enrich SELECT"
```

### Task 11: Wire ANTLR listener in CobolCodeElementBuilder

**Files:**
- Modify: `TypeCobol/Compiler/Parser/CodeElementBuilder/CobolCodeElementBuilder.cs` (after EnterUnsupportedSqlStatement, ~line 2467)

- [ ] **Step 1: Add Enter methods**

After `EnterUnsupportedSqlStatement` method (~line 2471), add:

```csharp
        public override void EnterInsertStatement([NotNull] CodeElementsParser.InsertStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateInsertStatement(context);
        }

        public override void EnterUpdateStatement([NotNull] CodeElementsParser.UpdateStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateUpdateStatement(context);
        }

        public override void EnterSqlDeleteStatement([NotNull] CodeElementsParser.SqlDeleteStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateSqlDeleteStatement(context);
        }

        public override void EnterDeclareCursorStatement([NotNull] CodeElementsParser.DeclareCursorStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateDeclareCursorStatement(context);
        }
```

- [ ] **Step 2: Build the full solution**

Run: `dotnet build TypeCobol.sln -c Debug --verbosity quiet 2>&1 | tail -5`
Expected: 0 errors

- [ ] **Step 3: Commit**

```bash
git add TypeCobol/Compiler/Parser/CodeElementBuilder/CobolCodeElementBuilder.cs
git commit -m "feat: wire ANTLR listener for INSERT/UPDATE/DELETE/DECLARE CURSOR"
```

### Task 12: Update UnsupportedSqlStatement comment

**Files:**
- Modify: `TypeCobol/Compiler/Sql/CodeElements/Statements/UnsupportedSqlStatement.cs`

- [ ] **Step 1: Update the comment (line 7)**

Change:
```csharp
    /// (e.g., INSERT, UPDATE, DELETE, DECLARE CURSOR, OPEN, FETCH, CLOSE).
```
to:
```csharp
    /// (e.g., OPEN, FETCH, CLOSE cursor operations).
```

- [ ] **Step 2: Commit**

```bash
git add TypeCobol/Compiler/Sql/CodeElements/Statements/UnsupportedSqlStatement.cs
git commit -m "chore: update UnsupportedSqlStatement comment (INSERT/UPDATE/DELETE now supported)"
```

## Chunk 4: Tests + Validation

### Task 13: Create test COBOL files

**Files:**
- Create: `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWithInsert.rdz.cbl`
- Create: `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWithUpdate.rdz.cbl`
- Create: `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWithDelete.rdz.cbl`
- Create: `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWithDeclareCursor.rdz.cbl`
- Create: `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWithSelectInto.rdz.cbl`

Note: `.rdz.cbl` files use RDZ reference format (6-char sequence, col 7 indicator, cols 8-72 source, 73-80 identification). Check existing test files for exact formatting.

- [ ] **Step 1: Read an existing test file for format reference**

Read: `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWithCommit.rdz.cbl` to understand exact formatting.

- [ ] **Step 2: Create ExecSqlWithInsert.rdz.cbl**

Follow the exact format from step 1. Contents must include:
- `EXEC SQL INSERT INTO EMPLOYEES (EMP_ID, EMP_NAME, SALARY) VALUES (:WS-ID, :WS-NAME, :WS-SALARY) END-EXEC`
- `EXEC SQL INSERT INTO SCHEMA1.AUDIT_LOG VALUES (:WS-ID, :WS-NAME) END-EXEC`

- [ ] **Step 3: Create ExecSqlWithUpdate.rdz.cbl**

Contents must include:
- `EXEC SQL UPDATE EMPLOYEES SET EMP_NAME = :WS-NAME, SALARY = :WS-SALARY WHERE EMP_ID = :WS-ID END-EXEC`

- [ ] **Step 4: Create ExecSqlWithDelete.rdz.cbl**

Contents must include:
- `EXEC SQL DELETE FROM EMPLOYEES WHERE EMP_ID = :WS-ID END-EXEC`

- [ ] **Step 5: Create ExecSqlWithDeclareCursor.rdz.cbl**

Contents must include:
- `EXEC SQL DECLARE EMP_CURSOR CURSOR FOR SELECT EMP_ID, EMP_NAME FROM EMPLOYEES END-EXEC`
- `EXEC SQL DECLARE DEPT_CURSOR CURSOR WITH HOLD FOR SELECT DEPT_ID, DEPT_NAME FROM DEPARTMENTS END-EXEC`

- [ ] **Step 6: Create ExecSqlWithSelectInto.rdz.cbl**

Contents must include:
- `EXEC SQL SELECT EMP_NAME, SALARY INTO :WS-NAME, :WS-SALARY FROM EMPLOYEES WHERE EMP_ID = :WS-ID END-EXEC`

- [ ] **Step 7: Commit**

```bash
git add TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWith*.rdz.cbl
git commit -m "test: add COBOL test files for INSERT/UPDATE/DELETE/DECLARE CURSOR/SELECT INTO"
```

### Task 14: Add test methods

**Files:**
- Modify: Test class that handles ExecSql tests (find with `grep -rn "ExecSqlWithUnsupportedStatement\|ExecSqlWith" TypeCobol.Test/ --include="*.cs"`)

- [ ] **Step 1: Find the test class and understand the test pattern**

Run: `grep -rn "ExecSqlWithUnsupportedStatement" TypeCobol.Test/ --include="*.cs" | head -5`

- [ ] **Step 2: Add test methods following the existing pattern**

Each test should parse the COBOL file with `EnableSqlParsing = true` and verify:
- 0 parsing errors
- Correct CodeElement types produced

- [ ] **Step 3: Run tests**

Run: `dotnet test TypeCobol.Test/TypeCobol.Test.csproj --filter "ExecSql" -v normal 2>&1 | tail -20`
Expected: All tests pass

- [ ] **Step 4: Commit**

```bash
git add TypeCobol.Test/
git commit -m "test: add unit tests for INSERT/UPDATE/DELETE/DECLARE CURSOR/SELECT INTO parsing"
```

### Task 15: Run regression tests and integration test

- [ ] **Step 1: Run full test suite**

Run: `dotnet test TypeCobol.Test/TypeCobol.Test.csproj -v normal 2>&1 | tail -30`
Expected: All tests pass

- [ ] **Step 2: Update expected results if needed**

Some existing test result files (`.txt`) may need updating for the new CodeElement type names. Update them and re-run.

- [ ] **Step 3: Run integration test on TriangulateSI COBOL file**

Run: `cd _test_sql_parse && dotnet run -c Debug 2>&1 | grep -v "^EXEC : warning"`

Expected: SQL statements now identified as InsertStatement, UpdateStatement, SqlDeleteStatement, SelectStatement (with INTO), DeclareCursorStatement. Error count should drop significantly from 65.

- [ ] **Step 4: Commit any test result updates**

```bash
git add -A TypeCobol.Test/
git commit -m "test: update expected results for new SQL DML CodeElements"
```

### Task 16: Final verification

- [ ] **Step 1: Clean build**

Run: `dotnet build TypeCobol.sln -c Debug --verbosity quiet 2>&1 | tail -5`
Expected: 0 errors

- [ ] **Step 2: Full test suite**

Run: `dotnet test TypeCobol.Test/TypeCobol.Test.csproj -v normal 2>&1 | tail -10`
Expected: All tests pass

- [ ] **Step 3: Final commit**

```bash
git commit --allow-empty -m "feat: complete SQL DML parsing (INSERT/UPDATE/DELETE/DECLARE CURSOR/SELECT INTO)

Parse embedded SQL statements to extract table names, column names, and
host variable bindings for XREF CRUD matrix generation.

- INSERT: table, columns, VALUES host variables (direction=IN)
- UPDATE: table, SET column=:var pairs (direction=IN), WHERE host vars
- DELETE: table, WHERE host variables
- DECLARE CURSOR: cursor name, inner SELECT with tables/columns
- SELECT INTO: enriched with INTO host variables (direction=OUT)
- Catch-all narrowed to OPEN/FETCH/CLOSE only"
```
