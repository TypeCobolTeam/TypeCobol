# Compilation process

The current document aims at describing each step of the TypeCobol compilation pipeline.
All steps are as follows:
- _Text Change Notification Handling_
- _Text Update_
- _Tokens Update_
- _Tokens Preprocessing_
- _Code Element Update_
- _AST building_
- _Cross check_
- _Code analysis_

The first five steps: _Text Change Notification Handling_, _Text Update_, _Tokens Update_, _Tokens Preprocessing_ and _Code Element Update_ are **incremental**. They are line-based, meaning that they operate at line level and use lines to define sections of code that are modified by a given text change.

The remaining three steps: _AST building_, _Cross check_ and _Code analysis_ are **non-incremental**. They are node-based and operate on the Abstract Syntax Tree. The AST is fully rebuilt and fully checked during these steps.

## Step 0: Text Change Notification Handling

**This step takes place in LanguageServer, not in parser directly.**

**Following steps all take place in parser.**

|Class|Method|Input|Output|
|-|-|-|-|
|`TypeCobolServer`|`OnDidChangeTextDocument`|`DidChangeTextDocumentParams` instance|`RangeUpdate` array|
|`Workspace`|`UpdateSourceFile`|`RangeUpdate` array|`void`|

This step is merely a translation step from LSP objects to parser objects. Each `TextDocumentContentChangeEvent` contained in the notification is turned into a `RangeUpdate` instance. However if one of the change does correspond to a document clear action, the incremental compilation is totally skipped and the document is reopened, triggering a full parsing.

The `RangeUpdate` array is then passed to the `Workspace.UpdateSourceFile` method which is responsible for driving the compilation process:
1. Forward the `RangeUpdate`s to the `CompilationDocument.UpdateTextLines` method
2. Start incremental parsing, meaning up-to and including _Code Element Update_
3. Schedule remaining non-incremental steps on a timer. After **750ms**, and if no other text changes have been received, the non-incremental parsing steps will be performed.

This step does not produce any diagnostic.

## Step 1: Text Update

|Class|Method|Input|Output|
|-|-|-|-|
|`CompilationDocument`|`UpdateTextLines`|`RangeUpdate` array|Updated `compilationDocumentLines`, list of `DocumentChange<ICobolTextLine>`|

The goal of this step is to apply the text changes (received as an array of `RangeUpdate`) on the `compilationDocumentLines` of the current `CompilationDocument`. Range updates are translated into `TextChange` instances. While a `RangeUpdate` describes a local modification on a portion of text, a `TextChange` describes a modification on a given line. The line can be added, updated or removed and each `TextChange` contains the whole text of the line after being modified. Consequently, `TextChange` are created using both the `RangeUpdate`s and the existing lines.

### From RangeUpdate to TextChange

Each `RangeUpdate` is processed individually to create a `TextChange`. The range update text is split on line breaks to check whether the update contains only one or several lines. The start and end of the range update define the span of the modification in the original text. By comparing the number of lines modified in the original text with the number of lines in the supplied text, we can determine which lines are inserted, updated or removed.

To build the final text of each line, the beginning of the first modified line (before start of update) is concatenated to the beginning of the range upate text. Similarly, the end of the last modified line (after end of update) is concatenated after the end of range update text.

### Applying TextChanges

The `CompilationDocument.ApplyTextChange` method is responsible for modifying the `compilationDocumentLines` collection according to the `TextChange`s previously built. It also creates the `DocumentChange<ICobolTextLine>` objects. To avoid returning redundant changes, the method aggregates them on the fly.

- When a new line is inserted:
  - Create and insert the new instance of `CodeElementsLine`
  - Shift down every following lines, this means increasing the line number for every following lines and every associated document changes 
  - Create the corresponding `DocumentChange`
- When an existing line is updated:
  - Replace the `CodeElementsLine` at specified index with a new instance
  - Create or update the corresponding `DocumentChange`
- When an existing line is removed:
  - Remove the `CodeElementsLine` at specified index
  - Shift up every following lines and every associated document changes and remove document changes corresponding to the removed line (if any)
  - Create the corresponding `DocumentChange`

The Text Update step does not produce any diagnostic but it ensures that existing diagnostics are synced during the line shifting operations (i.e. line number of existing diagnostics are shifted accordingly).

## Step 2: Tokens Update

|Class|Method|Input|Output|
|-|-|-|-|
|`CompilationDocument`|`UpdateTokensLines`|List of `DocumentChange<ICobolTextLine>`|Updated `compilationDocumentLines`, list of `DocumentChange<ITokensLine>`|
|`CompilationDocument`|`RefreshTokensDocumentSnapshot`|`void`|Updated `TokensDocumentSnapshot`|

During this step, the lines that have been altered are scanned to identify their tokens. The `RefreshTokensDocumentSnapshot`  only updates the `TokensDocument` instance, the `UpdateTokensLines` is doing all the work by calling the `ScannerStep` class.

To enable incremental scanning, the ScannerStep uses the `MultilineScanState` object. A line must be rescanned when either:
- it is part of the modified lines (and consequently has no tokens yet)
- it is part of a modified continuation group
- its `InitialScanState` differs from the final `ScanState` of the previous line
  - it means that previous line has been rescanned and the resulting `ScanState` is different from the one computed during previous compilation cycle. The line must be rescanned again according to the updated ScanState

This step produces scanner diagnostics stored in `_ScannerDiagnostics` field in `TokensLines` class.

## Step 3 Tokens Preprocessing

|Class|Method|Input|Output|
|-|-|-|-|
|`CompilationDocument`|`RefreshProcessedTokensDocumentSnapshot`|List of `DocumentChange<ITokensLine>`|Updated `compilationDocumentLines`, list of `DocumentChange<IProcessedTokensLine>`, updated `ProcessedTokensDocumentSnapshot`|

While being called 'Preprocessor', this step does not actually modifies text like a regular preprocessor would. The goal here is to parse compiler directives, this is done through CUP parser using definitions of 'CobolCompilerDirectives.cup' grammar.

For `COPY` directives, a second phase happens during Preprocessor step: after all directives of the modified lines have been parsed, the lines having `COPY` directives are checked again to perform actual loading of the target documents.

### Reparse section

To ensure clean reparsing, the reparse section is delimited by full compiler directives. This means that compiler directives written on several lines are included in the reparse section in order to be reparsed entirely. The `PreprocessorStep.CheckIfAdjacentLinesNeedRefresh` method is responsible for including lines located before or after modified lines that participate in a multiline compiler directive.

### Results

All results of compiler directive parsing are stored at `ProcessedTokensLine` level in these properties:
|Property|Role|
|-|-|
|`CompilerListingControlDirective`|Stores the `*CBL`, `*CONTROL`, `EJECT`, `SKIP1`, `SKIP2`, `SKIP3` or `TITLE` directive of the line if any has been found.|
|`HasDirectiveTokenContinuationFromPreviousLine`|Self-explanatory|
|`HasDirectiveTokenContinuedOnNextLine`|Self-explanatory|
|`ImportedDocuments`|Stores all documents imported by this line using the `COPY` directive.|
|`ReplaceDirective`|Stores the `REPLACE` directive of this line if any.|
|`tokensWithCompilerDirectives`|General storage for all compiler directives found on this line. Each directive is stored in a single `CompilerDirectiveToken` which is then added into this collection.|
|`PreprocessorDiagnostics`|Diagnostics reported during the parsing of the compiler directives of this line.|
|`NeedsCompilerDirectiveParsing`|Boolean flag indicating whether this line has been preprocessed or not.|

Among compiler directives, `REPLACE` and `COPY` have a special handling. Both `REPLACE` and `COPY` alter the stream of tokens used to create CodeElements during next step, so they are both parsed during Preprocessing step but actually processed during CodeElement step. This processing is done through the use of multiple TokensLinesIterators (see `CopyTokensLinesIterator`, `ReplaceTokensLinesIterator` and `ReplacingTokensLinesIterator`).

## Step 4 Code Element Update

|Class|Method|Input|Output|
|-|-|-|-|
|`CompilationUnit`|`RefreshCodeElementsDocumentSnapshot`|List of `DocumentChange<IProcessedTokensLine>`|Updated `compilationDocumentLines`, list of `DocumentChange<ICodeElementsLine>`, updated `CodeElementsDocumentSnapshot`|

This step turns a stream of tokens into a stream of `CodeElement`s. This is achieved using the ANTLR parser. The parsing is done in three phases:
1. Determine the boundaries of the reparse section
2. Parse CodeElements from tokens located inside the reparse section
3. Convert each CodeElement context object built by ANTLR into a real `CodeElement` instance

The results of this step are stored in `CodeElements` collection at the `CodeElementsLine` level. Each code element holds its own diagnostic collection. The `ParserDiagnostics` of `CodeElementsLine` stores diagnostics produced by this step but that can't be attached directly to a code element. This is used mainly for exceptions happening during parsing and also for diagnostics produced during the processing of `REPLACE`/`COPY` directives.

The `ActiveReplaceDirective` which tracks the `REPLACE` that applies to a given line is updated during this step too. 

### Reparse section

To ensure clean reparsing, the reparse section is delimited by CodeElements, but since the token iterator works at line level, the reparse section must also start at the beginning of a line and end at the end of a line.

The beginning of the reparse section is defined as the previous unmodified CodeElement as long as it starts at the beginning of its CodeElementsLine. The end of the reparse section obeys the same rule, but the algorithm adds one more CodeElement because ANTLR diagnostics may be attached on the _following_ CodeElement. Adding one more CodeElement allows to properly refresh the diagnostics for such cases. Additionally if modified lines are affected by a `REPLACE` directive, the reparse section include all lines up to next `REPLACE` directive.

### ANTLR parsing

The parsing itself is triggered through a single call to `CodeElementsParser.cobolCodeElements()`. This is within this operation that the tokens are actually iterated and the `COPY` / `REPLACE` directives are processed. To give ANTLR the real tokens, the iterator must:
- import tokens from documents included by `COPY`s instructions
  - applying the `REPLACING` clause if any
- alter tokens that match current `REPLACE` directive

### Building CodeElements

ANTLR returns a single `CodeElementsParser.CobolCodeElementsContext`. Context objects are not kept in the final result but are rather converted into real `CodeElement`s objects. This is done through a visitor pattern, the visitor object is `CodeElementBuilder` which turns each context object into its corresponding CodeElement object and also gather tokens of this CE into its `ConsumedTokens` collection.

## Step 5 AST building

|Class|Method|Input|Output|
|-|-|-|-|
|`CompilationUnit`|`ProduceTemporarySemanticDocument`|Up-to-date `compilationDocumentLines`|Updated `TemporaryProgramClassDocumentSnapshot`|

This step turns the complete list of CodeElements into an Abstract Syntax Tree using the CUP parser and the 'TypeCobolProgram.cup' grammar. Unlike ANTLR, the parsing and the visit are done simultaneously, no intermediary objects are created and the CUP parser calls directly `ProgramClassBuilder`. The `ProgramClassBuilder` is responsible for creating the AST by adding `Node` objects into it. It always starts with a root node of type `SourceFile`. Then for each structure recognized by CUP, it creates the corresponding `Node` object and adds it in tree. The builder tracks current node by successive calls to `Enter` or `Exit` methods depending on whether the current node expects children or not.

By convention, structures allowing children are entered when the corresponding `StartXXX` method is called and exited when the `ExitXXX` method is called. Childless structures are visited with a `OnXXX` method call.

For example:
- `ProgramClassBuilder.StartDataDivision` signals the beginning of the `DATA DIVISION` of the program, after this call the current node is the newly created `DataDivision` node allowing to add data definitions children.
- `ProgramClassBuilder.EndDataDivision` signals the end of the DATA DIVISION, thus the current node will be the program node.
- `ProgramClassBuilder.OnGobackStatement` signals the encounter of a `GobackStatement`. The method creates the `Goback` node, enters it as a child of the current node (most probably the `ProcedureDivision` node) and exit immediately.

The parsing errors produced by CUP parser are stored in the `Diagnostics` collection of the `TemporarySemanticDocument`.

## Step 6 Cross check

|Class|Method|Input|Output|
|-|-|-|-|
|`CompilationUnit`|`RefreshProgramClassDocumentSnapshot`|Up-to-date `TemporaryProgramClassDocumentSnapshot`|Updated `ProgramClassDocumentSnapshot`|

The goal of this step is to perform a full visit of the refreshed AST in order to check the semantics of the document. The main goal is to check that all used variables are defined unambiguously but other checks specifically related to some statements are performed as well.

The `CrossCompleteChecker` class is the visitor object that implements those checks. Note that it is configured to skip CodeElements during the visit, as it assumes that CodeElements have already been checked previously. All diagnostics must be created on nodes only during this step because CodeElements persist between compilation cycles whereas nodes are always recreated.

The `BeginNode` method triggers the check for variable definition, while the `VisitXXX` methods implement node-specific checks.

As stated before, the diagnostics are stored inside `Node` instances, they are later collected using a dedicated visitor (see `CompilationUnit.AllDiagnostics` method).

## Step 7 Code analysis

|Class|Method|Input|Output|
|-|-|-|-|
|`CompilationUnit`|`RefreshCodeAnalysisDocumentSnapshot`|Up-to-date `ProgramClassDocumentSnapshot`|Updated `CodeAnalysisDocumentSnapshot`|

This final step runs external analyzers on the latest `ProgramClassDocumentSnapshot`. To allow all types of code analysis, the analyzers are given all previous document snapshots (Tokens, ProcessedTokens, CodeElements, TemporaryProgramClass and ProgramClass).

Each analyzer receives all five document snapshot in sequence through calls to their `Inspect` methods. The results of the analysis (mainly diagnostics) are stored in a new instance of `InspectedProgramClassDocument`.

## About snapshots and result consistency.

The parser does not use multi-threading to parallelize source code processing. However the client of the parser may choose to perform multiple operations on the same source simultaneously. Typically in LanguageServer, a thread may be reading results of an incremental compilation round while the timer thread is already performing the remaining non-incremental compilation steps.

To ensure consistency, the results are exposed as 'snapshots', each `IDocumentSnapshot` contains compilation results for a given step, is identified by a version number and is _consistent_.

Line objects are intrinsicallly shared between snapshots because a single most-derived type is used to represent a line: each `CodeElementsLine` _is_ indeed a `CodeElementsLine` but also a `ProcessedTokensLine` and a `TokensLine`. To avoid altering a line that has already been captured in a snapshot, the parser checks whether a line _can still be updated_ by a given step. A line can be updated by a given step only if the updating step is further than the creating step of this line. If the line cannot be updated, a new instance is created instead.

This logic is implemented by `CobolTextLine.CanStillBeUpdatedBy` and `CompilationDocument.PrepareDocumentLineForUpdate`.

Note that lines are not necessarily created by the _Text Update_ step: during the incremental parsing steps, the reparse section may grow in both directions because of continuations, multiline compiler directives or multiline code elements and consequently lines may need to be updated/created during _Tokens Update_, _Tokens Preprocessing_ or _Code Element Update_ steps.
While not being modified directly by an edit, these lines will have their data actually modified.

### Line classes hierarchy

The line types are defined through a seraies of classes and interfaces.

- `ITextLine`
  - `ICobolTextLine` implemented by `CobolTextLine`
    - `ITokensLine` implemented by `TokensLine` which inherits from `CobolTextLine`
      - `IProcessedTokensLine` implemented by `ProcessedTokensLine` which inherits from `TokensLine`
        - `ICodeElementsLine` implemented by `CodeElementsLine` which inherits from `ProcessedTokensLine`
  - `TextLineSnapshot`

The `TextLineSnapshot` class is used to capture only a text and a line number, the other classes all wrap an `ITextLine` instance and adds their own data, related to the step they are used in.

|Interface|Role / Content|
|-|-|
|`ITextLine`|Represents any line of source code, it has a `Text` and a `LineIndex`.|
|`ICobolTextLine`|Specialized `ITextLine` for Cobol text, the Cobol text has a layout and is divided into 4 regions: sequence number, indicator, source and comments. For performance reasons, only `Source` and `Indicator` are materialized as `TextArea`, comments and sequence number are only available as text.|
|`ITokensLine`|Specialized `ICobolTextLine` for Cobol text that has been parsed into tokens, this type of line holds the tokens and the scanner scan state.|
|`IProcessedTokensLine`|Specialized `ITokensLine` for Cobol tokens that have been preprocessed. This type of line holds the compiler directives data.|
|`ICodeElementsLine`|Specialized `IProcessedTokensLine` for tokens that have been assembled into code elements. This type of line holds the `CodeElement`s. The current `CompilationDocument` is made of a list of `CodeElementsLine`s which are updated or re-instanciated by successive compilation steps.|
