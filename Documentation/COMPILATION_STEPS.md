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

## Text Change Notification Handling

|Class|Method|Input|Output|
|-|-|-|-|
|`TypeCobolServer`|`OnDidChangeTextDocument`|`DidChangeTextDocumentParams` instance|`RangeUpdate` array|

This step is merely a translation step from LSP objects to parser objects. Each `TextDocumentContentChangeEvent` contained in the notification is turned into a `RangeUpdate` instance. However if one of the change does correspond to a document clear action, the incremental compilation is skipped and the document is reopened, triggering a full parsing.

The `RangeUpdate` array is passed to `Workspace.UpdateSourceFile` method.

## Text Update

|Class|Method|Input|Output|
|-|-|-|-|
|`Workspace`|`UpdateSourceFile`|`RangeUpdate` array|`void`|
|`CompilationDocument`|`UpdateTextLines`|`RangeUpdate` array|Updated `compilationDocumentLines`, list of `DocumentChangedEvent<ICobolTextLine>`|

This step aims at applying text changes to the current in-memory representation of the document. It then starts the incremental steps in same thread and schedules remaining non-incremental steps using a timer.

### Source file update

1. Forward the `RangeUpdate`s to the `UpdateTextLines` method
2. Start incremental parsing, meaning up to and including _Code Element Update_
3. Schedule remaining non-incremental steps on a timer. After **750ms**, and if no other text changes have been received, the non-incremental parsing steps will be performed.

### Text lines update

The goal of this step is to apply the text changes (received as an array of `RangeUpdate`) on the `compilationDocumentLines` of the current `CompilationDocument`. Range updates are translated into `TextChange` instances. While a `RangeUpdate` describe a local modification on a portion of text, a `TextChange` describe a modification on a given line. The line can be added, updated or removed and each `TextChange` contains the whole text of the line after being modified. Consequently `TextChange` are created using both the `RangeUpdate`s and the existing lines.

- from RangeUpdate to TextChange

TODO

- ApplyTextChange

TODO
