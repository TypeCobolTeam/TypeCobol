# SQL Catch-All Grammar Rule Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix parser hang on unsupported SQL statements by adding a catch-all grammar rule instead of Scanner-level filtering.

**Architecture:** Add `unsupportedSqlStatement` as the last SQL alternative in the ANTLR grammar. It consumes tokens for SQL statements without dedicated grammar rules (INSERT, UPDATE, DELETE, DECLARE CURSOR, OPEN, FETCH, CLOSE). Revert the Scanner.cs whitelist approach. Produce an `UnsupportedSqlStatement` AST node for diagnostics.

**Tech Stack:** C# / ANTLR4 / CUP parser / .NET

---

## File Structure

| Action | File | Responsibility |
|--------|------|----------------|
| Create | `TypeCobol/Compiler/Sql/CodeElements/Statements/UnsupportedSqlStatement.cs` | CodeElement for unsupported SQL |
| Create | `TypeCobol/Compiler/Sql/Nodes/UnsupportedSql.cs` | AST Node |
| Create | `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWithUnsupportedStatement.rdz.cbl` | Test COBOL file |
| Modify | `TypeCobol/AntlrGrammar/CobolCodeElements.g4` | Add grammar rule |
| Modify | `TypeCobol/Compiler/Scanner/Scanner.cs` | Revert whitelist |
| Modify | `TypeCobol/Compiler/CodeElements/CodeElementType.cs` | Add enum |
| Modify | `TypeCobol/Compiler/CodeElements/StatementElement.cs` | Add enum |
| Modify | `TypeCobol/Compiler/Sql/CodeElements/SqlCodeElementBuilder.cs` | Factory method |
| Modify | `TypeCobol/Compiler/Parser/CodeElementBuilder/CobolCodeElementBuilder.cs` | Enter visitor |
| Modify | `TypeCobol/Compiler/CodeElements/CobolLanguageLevelVisitor.cs` | Visit methods |
| Modify | `TypeCobol/Compiler/CupParser/NodeBuilder/IProgramClassBuilder.cs` | Interface |
| Modify | `TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilder.cs` | Implementation |
| Modify | `TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeDispatcher.cs` | Dispatcher |
| Modify | `TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeListener.cs` | Listener |
| Modify | `TypeCobol/Compiler/CupParser/EmptyTypeCobolProgram.cup` | CUP terminal + production |
| Modify | `TypeCobol/Compiler/CupParser/TypeCobolProgram.cup` | CUP terminal + production |
| Keep | `TypeCobol/Compiler/Sql/CodeElements/SqlStatementElement.cs` | Already has DML tokens in IsSqlStatementStartingKeyword |

---

## Chunk 1: New Branch + Revert Scanner

### Task 1: Create new branch from develop

- [ ] **Step 1: Create branch**

```bash
cd C:/dev/TypeCobol
git checkout develop
git checkout -b fix/sql-catch-all-grammar
```

- [ ] **Step 2: Verify clean state**

```bash
git status
git log --oneline -3
```

Expected: on `fix/sql-catch-all-grammar`, clean working tree

### Task 2: Add SqlStatementElement.cs DML tokens (from PR1)

The `IsSqlStatementStartingKeyword` method on develop does NOT include the DML tokens. We need to add them for error recovery boundaries.

**Files:**
- Modify: `TypeCobol/Compiler/Sql/CodeElements/SqlStatementElement.cs:20-50`

- [ ] **Step 1: Add DML token types**

After line 36 (`case TokenType.SQL_WHENEVER:`), add:

```csharp
                case TokenType.SQL_INSERT:
                case TokenType.SQL_UPDATE:
                case TokenType.SQL_DELETE:
                case TokenType.SQL_DECLARE:
                case TokenType.SQL_OPEN:
                case TokenType.SQL_FETCH:
                case TokenType.SQL_CLOSE:
```

- [ ] **Step 2: Commit**

```bash
git add TypeCobol/Compiler/Sql/CodeElements/SqlStatementElement.cs
git commit -m "Add DML token types to SQL statement starting keywords for error recovery"
```

---

## Chunk 2: Enum Types + AST Classes

### Task 3: Add CodeElementType enum value

**Files:**
- Modify: `TypeCobol/Compiler/CodeElements/CodeElementType.cs:178`

- [ ] **Step 1: Add enum value**

After `ExecuteImmediateStatement` (line 178), add:

```csharp
        UnsupportedSqlStatement,
```

- [ ] **Step 2: Verify build**

```bash
cd C:/dev/TypeCobol
dotnet build TypeCobol/TypeCobol.csproj --no-restore 2>&1 | tail -5
```

### Task 4: Add StatementType enum value

**Files:**
- Modify: `TypeCobol/Compiler/CodeElements/StatementElement.cs:146`

- [ ] **Step 1: Add enum value**

After `ExecuteImmediateStatement` (line 146), add:

```csharp
        UnsupportedSqlStatement,
```

### Task 5: Create UnsupportedSqlStatement CodeElement

**Files:**
- Create: `TypeCobol/Compiler/Sql/CodeElements/Statements/UnsupportedSqlStatement.cs`

- [ ] **Step 1: Create the file**

Follow the simple pattern from `CommitStatement.cs`:

```csharp
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// Represents an SQL statement that is not supported by the grammar
    /// (e.g., INSERT, UPDATE, DELETE, DECLARE CURSOR, OPEN, FETCH, CLOSE).
    /// The raw SQL text is preserved for diagnostic purposes.
    /// </summary>
    public class UnsupportedSqlStatement : SqlStatementElement
    {
        /// <summary>
        /// The raw SQL keyword that started this unsupported statement.
        /// </summary>
        public string SqlKeyword { get; }

        public UnsupportedSqlStatement(string sqlKeyword)
            : base(CodeElementType.UnsupportedSqlStatement, StatementType.UnsupportedSqlStatement)
        {
            SqlKeyword = sqlKeyword;
        }
    }
}
```

### Task 6: Create UnsupportedSql Node

**Files:**
- Create: `TypeCobol/Compiler/Sql/Nodes/UnsupportedSql.cs`

- [ ] **Step 1: Create the file**

Follow pattern from `Commit.cs`:

```csharp
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.Nodes
{
    public class UnsupportedSql : GenericNode<UnsupportedSqlStatement>, Statement
    {
        public UnsupportedSql(UnsupportedSqlStatement statement) : base(statement) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
}
```

- [ ] **Step 2: Commit**

```bash
git add TypeCobol/Compiler/CodeElements/CodeElementType.cs \
       TypeCobol/Compiler/CodeElements/StatementElement.cs \
       TypeCobol/Compiler/Sql/CodeElements/Statements/UnsupportedSqlStatement.cs \
       TypeCobol/Compiler/Sql/Nodes/UnsupportedSql.cs
git commit -m "Add UnsupportedSqlStatement CodeElement and AST node"
```

---

## Chunk 3: Visitor + Builder Infrastructure

### Task 7: Add Visit methods in CobolLanguageLevelVisitor

**Files:**
- Modify: `TypeCobol/Compiler/CodeElements/CobolLanguageLevelVisitor.cs:1663`

- [ ] **Step 1: Add Visit methods**

After the `Visit(ExecuteImmediate)` method (around line 1665), add:

```csharp
        public virtual bool Visit(UnsupportedSqlStatement unsupportedSqlStatement)
        {
            return true;
        }
        public virtual bool Visit(UnsupportedSql unsupportedSql)
        {
            return true;
        }
```

- [ ] **Step 2: Add using directives if needed**

Check if the file already imports `TypeCobol.Compiler.Sql.CodeElements.Statements` and `TypeCobol.Compiler.Sql.Nodes`. Add missing usings at the top of the file.

### Task 8: Add interface method in IProgramClassBuilder

**Files:**
- Modify: `TypeCobol/Compiler/CupParser/NodeBuilder/IProgramClassBuilder.cs:887`

- [ ] **Step 1: Add method signature**

Before `#endregion` (line 888), add:

```csharp
        /// <summary>
        /// Enter an UnsupportedSql Statement Node
        /// </summary>
        /// <param name="unsupportedSql">The corresponding UnsupportedSql Statement Code Element</param>
        void OnUnsupportedSqlStatement([NotNull] UnsupportedSqlStatement unsupportedSql);
```

- [ ] **Step 2: Add using directive**

Add at top: `using TypeCobol.Compiler.Sql.CodeElements.Statements;` (if not already present)

### Task 9: Add ProgramClassBuilder implementation

**Files:**
- Modify: `TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilder.cs:1800`

- [ ] **Step 1: Add method**

After `OnExecuteImmediateStatement` (line 1800), before the closing `}`:

```csharp
        public void OnUnsupportedSqlStatement([NotNull] UnsupportedSqlStatement unsupportedSql)
        {
            Enter(new UnsupportedSql(unsupportedSql), unsupportedSql);
            Exit();
            Dispatcher.OnUnsupportedSqlStatement(unsupportedSql);
        }
```

- [ ] **Step 2: Add using directives**

Add: `using TypeCobol.Compiler.Sql.Nodes;` and `using TypeCobol.Compiler.Sql.CodeElements.Statements;` if not already present.

### Task 10: Add Dispatcher method

**Files:**
- Modify: `TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeDispatcher.cs:908`

- [ ] **Step 1: Add method**

After `OnExecuteImmediateStatement` (line 908), before `}`:

```csharp
        public void OnUnsupportedSqlStatement([NotNull] UnsupportedSqlStatement unsupportedSql)
        {
            foreach (var listener in _listeners) listener.OnUnsupportedSqlStatement(unsupportedSql);
        }
```

### Task 11: Add Listener method

**Files:**
- Modify: `TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeListener.cs:896`

- [ ] **Step 1: Add method**

After `OnExecuteImmediateStatement` (line 896), before `}`:

```csharp
        public void OnUnsupportedSqlStatement([NotNull] UnsupportedSqlStatement unsupportedSql)
        {

        }
```

- [ ] **Step 2: Commit**

```bash
git add TypeCobol/Compiler/CodeElements/CobolLanguageLevelVisitor.cs \
       TypeCobol/Compiler/CupParser/NodeBuilder/IProgramClassBuilder.cs \
       TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilder.cs \
       TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeDispatcher.cs \
       TypeCobol/Compiler/CupParser/NodeBuilder/ProgramClassBuilderNodeListener.cs
git commit -m "Add UnsupportedSqlStatement to visitor, builder, dispatcher, listener infrastructure"
```

---

## Chunk 4: Grammar + CUP + Builder

### Task 12: Add grammar rule in CobolCodeElements.g4

**Files:**
- Modify: `TypeCobol/AntlrGrammar/CobolCodeElements.g4:230-231` and `8531-8534`

- [ ] **Step 1: Add alternative in codeElement production**

After `| executeImmediateStatement` (line 230), add:

```antlr
	| unsupportedSqlStatement
```

- [ ] **Step 2: Add grammar rule**

Before the `// End of DB2 coprocessor` comment (line 8532), add:

```antlr
// Catch-all rule for SQL statements without dedicated grammar rules.
// Must be listed LAST among SQL alternatives in codeElement so that
// supported statements are tried first.
unsupportedSqlStatement:
    ( SQL_INSERT | SQL_UPDATE | SQL_DELETE
    | SQL_DECLARE | SQL_OPEN | SQL_FETCH | SQL_CLOSE ) ~END_EXEC*;
```

Note: `~END_EXEC*` matches zero or more tokens that are NOT `END_EXEC`. This consumes the rest of the SQL statement cleanly without triggering error recovery.

### Task 13: Add SqlCodeElementBuilder factory method

**Files:**
- Modify: `TypeCobol/Compiler/Sql/CodeElements/SqlCodeElementBuilder.cs`

- [ ] **Step 1: Add CreateUnsupportedSqlStatement method**

After the last Create method (around line 929), add:

```csharp
        public UnsupportedSqlStatement CreateUnsupportedSqlStatement(CodeElementsParser.UnsupportedSqlStatementContext context)
        {
            // Extract the first token text as the SQL keyword
            var firstToken = context.Start;
            string keyword = firstToken?.Text ?? "UNKNOWN";
            return new UnsupportedSqlStatement(keyword);
        }
```

### Task 14: Add Enter method in CobolCodeElementBuilder

**Files:**
- Modify: `TypeCobol/Compiler/Parser/CodeElementBuilder/CobolCodeElementBuilder.cs:2465`

- [ ] **Step 1: Add Enter method**

Before the closing `}` of the class (line 2466), add:

```csharp
        public override void EnterUnsupportedSqlStatement([NotNull] CodeElementsParser.UnsupportedSqlStatementContext context)
        {
            Context = context;
            CodeElement = _sqlCodeElementBuilder.CreateUnsupportedSqlStatement(context);
        }
```

### Task 15: Add CUP terminal and production (EmptyTypeCobolProgram.cup)

**Files:**
- Modify: `TypeCobol/Compiler/CupParser/EmptyTypeCobolProgram.cup:155` and `737`

- [ ] **Step 1: Add terminal declaration**

After the `ExecuteImmediateStatement` terminal (line 155), add:

```
terminal TypeCobol.Compiler.Sql.CodeElements.Statements.UnsupportedSqlStatement UnsupportedSqlStatement;
```

- [ ] **Step 2: Add production**

In the `sqlStatement` production, after `ExecuteImmediateStatement:stmt` (line 736-737), add before `;`:

```
|	UnsupportedSqlStatement:stmt
{: :}
```

### Task 16: Add CUP terminal and production (TypeCobolProgram.cup)

**Files:**
- Modify: `TypeCobol/Compiler/CupParser/TypeCobolProgram.cup:250` and `833`

- [ ] **Step 1: Add terminal declaration**

After the `ExecuteImmediateStatement` terminal (line 250), add:

```
terminal TypeCobol.Compiler.Sql.CodeElements.Statements.UnsupportedSqlStatement UnsupportedSqlStatement;
```

- [ ] **Step 2: Add production**

In the `sqlStatement` production, after `ExecuteImmediateStatement:stmt` (line 832-833), add before `;`:

```
|	UnsupportedSqlStatement:stmt
{: my_parser.Builder.OnUnsupportedSqlStatement(stmt); :}
```

- [ ] **Step 3: Commit**

```bash
git add TypeCobol/AntlrGrammar/CobolCodeElements.g4 \
       TypeCobol/Compiler/Sql/CodeElements/SqlCodeElementBuilder.cs \
       TypeCobol/Compiler/Parser/CodeElementBuilder/CobolCodeElementBuilder.cs \
       TypeCobol/Compiler/CupParser/EmptyTypeCobolProgram.cup \
       TypeCobol/Compiler/CupParser/TypeCobolProgram.cup
git commit -m "Add unsupportedSqlStatement grammar rule and CUP productions"
```

---

## Chunk 5: Revert Scanner + Build

### Task 17: Revert Scanner.cs to develop version

**Files:**
- Modify: `TypeCobol/Compiler/Scanner/Scanner.cs:2002-2090`

- [ ] **Step 1: Restore original AfterExecSql block**

Replace lines 2002-2013 with the original:

```csharp
            if (tokensLine.ScanState.AfterExecSql)
            {
                // Expect SQL code
                tokensLine.ScanState.InsideSql = true;
            }
```

- [ ] **Step 2: Remove IsSupportedSqlStatement method**

Delete the entire `IsSupportedSqlStatement` method (lines 2033-2090).

- [ ] **Step 3: Commit**

```bash
git add TypeCobol/Compiler/Scanner/Scanner.cs
git commit -m "Revert Scanner whitelist: InsideSql now always true after EXEC SQL"
```

### Task 18: Build the project

- [ ] **Step 1: Full build**

```bash
cd C:/dev/TypeCobol
dotnet build TypeCobol.sln 2>&1 | tail -20
```

Expected: Build succeeded with 0 errors.

- [ ] **Step 2: Fix any compilation errors**

If the ANTLR-generated parser needs regeneration, check if there is a build step or pre-build event that handles this. The grammar change requires ANTLR to regenerate `CodeElementsParser.cs` and related files.

- [ ] **Step 3: Commit fixes if any**

---

## Chunk 6: Tests

### Task 19: Create test COBOL file

**Files:**
- Create: `TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWithUnsupportedStatement.rdz.cbl`

- [ ] **Step 1: Create test file**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTUNSQL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VAR1        PIC X(10).
       01  WS-VAR2        PIC 9(5).
       01  WS-CURSOR-NAME PIC X(20).
       PROCEDURE DIVISION.
      * Supported SQL - should parse normally
           EXEC SQL
              COMMIT
           END-EXEC
      * Unsupported: INSERT
           EXEC SQL
              INSERT INTO TABLE1 (COL1, COL2)
              VALUES (:WS-VAR1, :WS-VAR2)
           END-EXEC
      * Unsupported: UPDATE
           EXEC SQL
              UPDATE TABLE1
              SET COL1 = :WS-VAR1
              WHERE COL2 = :WS-VAR2
           END-EXEC
      * Unsupported: DELETE
           EXEC SQL
              DELETE FROM TABLE1
              WHERE COL1 = :WS-VAR1
           END-EXEC
      * Unsupported: DECLARE CURSOR
           EXEC SQL
              DECLARE C1 CURSOR FOR
              SELECT COL1, COL2 FROM TABLE1
           END-EXEC
      * Unsupported: OPEN cursor
           EXEC SQL
              OPEN C1
           END-EXEC
      * Unsupported: FETCH
           EXEC SQL
              FETCH C1 INTO :WS-VAR1, :WS-VAR2
           END-EXEC
      * Unsupported: CLOSE cursor
           EXEC SQL
              CLOSE C1
           END-EXEC
      * Supported SQL after unsupported - should still parse
           EXEC SQL
              ROLLBACK
           END-EXEC
           GOBACK.
       END PROGRAM TSTUNSQL.
```

### Task 20: Run existing SQL tests

- [ ] **Step 1: Run tests**

```bash
cd C:/dev/TypeCobol
dotnet test TypeCobol.Test/TypeCobol.Test.csproj --filter "ExecSql" -v normal 2>&1 | tail -30
```

Expected: All existing SQL tests pass (no regression).

- [ ] **Step 2: Commit test file**

```bash
git add TypeCobol.Test/Parser/Programs/Cobol85/ExecSql/ExecSqlWithUnsupportedStatement.rdz.cbl
git commit -m "Add test file for unsupported SQL statements (INSERT, UPDATE, DELETE, cursors)"
```

### Task 21: Validate with Triangulate COBOL files

- [ ] **Step 1: Build the TypeCobol bridge**

```bash
cd C:/dev/Triangulate/triangulate
dotnet build bridge/TypeCobol.Bridge/TypeCobol.Bridge.csproj -c Release -r win-x64
cp bridge/TypeCobol.Bridge/bin/Release/*/win-x64/*.{exe,dll} bin/
```

- [ ] **Step 2: Run COBOL analysis on all test fixtures**

```bash
cd C:/dev/Triangulate/triangulate
python -X utf8 main.py sast test_fixtures/sample_cobol/ -l cobol -f both --timeout 60
```

Expected: All 10 `.cbl` files analyzed without hang or timeout.

- [ ] **Step 3: Run on UAST test fixtures**

```bash
python -X utf8 main.py sast test_fixtures/uast/synth_cobol.cbl -l cobol -f both --timeout 60
python -X utf8 main.py sast test_fixtures/uast/synth_cobol_sql.cbl -l cobol -f both --timeout 60
```

Expected: No hang, no memory leak, results produced.
