using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Antlr4.Runtime;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Sql.CodeElements.Statements;
using TypeCobol.Compiler.Sql.Model;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Utils
{
    /// <summary>
    /// Define a method of comparison between actual compilation results and expected results (formatted in a given text file)
    /// </summary>
    internal class Comparison
    {
        public string ChangeId { get; }
        private readonly string _expectedResultPath;
        private readonly ICompilationResultFormatter _formatter;

        public bool IsEI { get; }

        public Comparison(string changeId, string expectedResultPath, ICompilationResultFormatter formatter, bool isEI)
        {
            ChangeId = changeId;
            _expectedResultPath = expectedResultPath;
            _formatter = formatter;
            IsEI = isEI;
        }

        public void Compare(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            string stepDescription = ChangeId != null ? $"after incremental change: {ChangeId}" : "after initial parsing";
            string comparisonName = $"{compilationResult.TextSourceInfo.Name} vs {_formatter.GetType().Name} ({stepDescription})";
            string actual = _formatter.Format(compilationResult, history);
            using (var expected = new StreamReader(new FileStream(_expectedResultPath, FileMode.Open)))
            {
                ParserUtils.CheckWithResultReader(comparisonName, actual, expected, _expectedResultPath);
            }
        }
    }

    /// <summary>
    /// Define a method of formatting some compilation result into a text result.
    /// Each formatter implementation allows to examine one facet of CompilationUnit, for example tokens, nodes, etc...
    /// </summary>
    internal interface ICompilationResultFormatter
    {
        string Format(CompilationUnit compilationResult, IncrementalChangesHistory history);
    }

    /// <summary>
    /// Repository of formatters/comparisons.
    /// DON'T FORGET TO ADD YOUR NEW FORMATTER HERE WITH ITS IDENTIFIER !
    /// </summary>
    internal static class Comparisons
    {
        private static readonly Dictionary<string, Func<ICompilationResultFormatter>> _Activators;

        static Comparisons()
        {
            _Activators = new Dictionary<string, Func<ICompilationResultFormatter>>(StringComparer.OrdinalIgnoreCase)
                          {
                              { "ANTLR",        () => new AntlrProfiling() },
                              { "RPN",          () => new ArithmeticStatements() },
                              { "CodeElements", () => new CodeElements() },
                              { "NY",           () => new CodeElementsAndDiagnosticsCount() },
                              { "DocJson",      () => new Documentation() },
                              { "Doc",          () => new DocumentationProperties() },
                              { "TEXT",         () => new FullText() },
                              { "INC",          () => new IncrementalChanges() },
                              { "MEM",          () => new MemoryMap() },
                              { "Nodes",        () => new Nodes() },
                              { "PGM",          () => new ProgramsClassesAndDiagnostics() },
                              { "Mix",          () => new SourceMixedWithDiagnostics() },
                              { "SQL",          () => new SqlStatements() },
                              { "SYM",          () => new Symbols() },
                              { "Tokens",       () => new Tokens() },
                              { "Copies",       () => new Copies() }
                          };
        }

        public static Comparison GetComparison(string expectedResultPath)
        {
            string fileName = Path.GetFileName(expectedResultPath);
            string[] parts = fileName.Split('.');
            string changeId;
            string format;
            switch (parts.Length)
            {
                case 1:
                case 2:
                    throw new ArgumentException($"Expected result file must have at least 3 parts: name, format identifier and extension. '{expectedResultPath}' is not a valid result file path.", nameof(expectedResultPath));
                case 3:
                    // name.format.ext
                    changeId = null;
                    format = parts[1];
                    break;
                case 4:
                    // name.changeid.format.ext
                    changeId = parts[1];
                    format = parts[2];
                    break;
                default:
                    throw new ArgumentException($"Base name of expected result file cannot contain dot. '{expectedResultPath}' is not a valid result file path.", nameof(expectedResultPath));
            }

            bool isEI = false;
            if (format.EndsWith("-EI", StringComparison.OrdinalIgnoreCase))
            {
                format = format.Substring(0, format.Length - 3);
                isEI = true;
            }

            if (!_Activators.TryGetValue(format, out var activator))
            {
                throw new ArgumentException($"Could not find compilation result formatter for identifier '{format}'.", nameof(expectedResultPath));
            }

            var formatter = activator();

            return new Comparison(changeId, expectedResultPath, formatter, isEI);
        }
    }

    internal class CodeElements : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var codeElements = compilationResult.CodeElementsDocumentSnapshot.CodeElements;
            var diagnostics = compilationResult.AllDiagnostics(true);
            return ParserUtils.DumpResult(codeElements, diagnostics);
        }
    }

    internal class ArithmeticStatements : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();
            var codeElements = compilationResult.CodeElementsDocumentSnapshot.CodeElements;
            foreach (var codeElement in codeElements)
            {
                if (codeElement is ArithmeticStatement arithmeticStatement)
                {
                    foreach (var affectation in arithmeticStatement.Affectations)
                    {
                        foreach (var expression in affectation.Value)
                        {
                            result.Append(affectation.Key).Append(" = ").Append(expression).Append(", ");
                        }
                    }

                    if (arithmeticStatement.Affectations.Count > 0)
                    {
                        result.Length -= 2;
                    }

                    result.AppendLine();
                }
            }

            return result.ToString();
        }
    }

    internal class CodeElementsAndDiagnosticsCount : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();
            var codeElements = compilationResult.CodeElementsDocumentSnapshot.CodeElements;
            var diagnostics = compilationResult.AllDiagnostics(true);
            foreach (var codeElement in codeElements)
            {
                if (codeElement is SentenceEnd)
                {
                    continue;
                }

                result.AppendLine("Y");
            }

            foreach (var _ in diagnostics)
            {
                result.AppendLine("N");
            }

            return result.ToString();
        }
    }

    internal class ProgramsClassesAndDiagnostics : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var programs = compilationResult.ProgramClassDocumentSnapshot.Root.Programs;
            var classes = compilationResult.ProgramClassDocumentSnapshot.Root.Classes;
            var diagnostics = compilationResult.AllDiagnostics();
            return ParserUtils.DumpResult(programs, classes, diagnostics);
        }
    }

    internal class Symbols : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();
            var diagnostics = compilationResult.AllDiagnostics();
            var programs = compilationResult.ProgramClassDocumentSnapshot.Root.Programs.ToList();

            //Write diagnostics
            if (diagnostics.Count > 0)
            {
                result.AppendLine(ParserUtils.DiagnosticsToString(diagnostics));
            }

            //Write program symbols
            if (programs.Count > 0)
            {
                foreach (var program in programs)
                {
                    result.AppendLine("--- Program ---");
                    result.AppendLine(program.SemanticData?.ToString() ?? "No Semantic Data !");
                }
            }

            //TODO Comparison for classes ?

            return result.ToString();
        }
    }

    internal class SourceMixedWithDiagnostics : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var diagnosticsEnumerator = compilationResult.AllDiagnostics().OrderBy(d => d.LineStart).GetEnumerator();

            //Create result file

            //Read original source code
            var result = new StringBuilder();
            int linePos = 0;

            var nextDiagnostic = diagnosticsEnumerator.MoveNext() ? diagnosticsEnumerator.Current : null;
            foreach (var cobolTextLine in compilationResult.CobolTextLines)
            {
                string line = cobolTextLine.Text;
                linePos++;

                while (nextDiagnostic != null && nextDiagnostic.LineStart <= linePos)
                {
                    result.Append(nextDiagnostic).Append("\n");
                    nextDiagnostic = diagnosticsEnumerator.MoveNext() ? diagnosticsEnumerator.Current : null;
                }
                result.Append(line).Append("\n");
            }

            //Print all remaining diagnostics
            while (nextDiagnostic != null)
            {
                result.Append(nextDiagnostic).Append("\n");
                nextDiagnostic = diagnosticsEnumerator.MoveNext() ? diagnosticsEnumerator.Current : null;
            }

            diagnosticsEnumerator.Dispose();

            return result.ToString();
        }
    }

    internal class Nodes : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();
            var diagnostics = compilationResult.AllDiagnostics();
            foreach (var diagnostic in diagnostics)
            {
                result.AppendLine(diagnostic.ToString());
            }

            result.AppendLine("--- Nodes ---");
            result.Append(compilationResult.ProgramClassDocumentSnapshot.Root);

            return result.ToString();
        }
    }

    internal class Tokens : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();

            var diagnostics = compilationResult.AllDiagnostics();
            foreach (var diagnostic in diagnostics)
            {
                result.AppendLine(diagnostic.ToString());
            }

            result.AppendLine("--- Tokens ---");
            foreach (var tokensLine in compilationResult.TokensLines)
            {
                result.AppendLine("---------------------------------");
                result.AppendLine("_" + tokensLine.SourceText + "_");
                foreach (var sourceToken in tokensLine.SourceTokens)
                {
                    result.AppendLine("    _" + sourceToken.SourceText + "_    " + sourceToken);
                }
            }

            return result.ToString();
        }
    }

    internal class MemoryMap : ICompilationResultFormatter
    {
        private static IEnumerable<DataDefinition> GetDataDefinitions(Node node)
        {
            foreach (var child in node.Children)
            {
                if (child is DataDefinition dataDefinition)
                {
                    yield return dataDefinition;
                    if (child.Children.Count > 0)
                    {
                        foreach (var childDataDefinition in GetDataDefinitions(child))
                        {
                            yield return childDataDefinition;
                        }
                    }
                }
            }
        }

        private static string CreateLine(DataDefinition data, bool slackByteIsHandled)
        {
            Node parentData = data.Parent;
            var res = new StringBuilder();
            int indent = 4;

            while (parentData is DataSection == false)
            {
                parentData = parentData.Parent;
                res.Append(new string(' ', indent));
            }

            if (!slackByteIsHandled)
            {
                res.Append("SlackByte");

                res.AppendLine(InsertValues(res.Length, (data.StartPosition - data.SlackBytes).ToString(), (data.StartPosition - 1).ToString(),
                    data.SlackBytes.ToString()));

                res.Append(CreateLine(data, true));
            }
            else
            {
                var dataEntry = data.CodeElement;
                if (dataEntry != null)
                {
                    res.Append(data.IsTableOccurence
                        ? $"{dataEntry.LevelNumber} {data.Name} ({data.MaxOccurencesCount})"
                        : $"{dataEntry.LevelNumber} {data.Name}");

                    res.Append(InsertValues(res.Length, data.StartPosition.ToString(), data.PhysicalPosition.ToString(),
                        data.PhysicalLength.ToString()));
                }
            }

            return res.ToString();
        }

        private static string InsertValues(int lineLength, string startPosition, string physicalPosition, string physicalLength)
        {
            StringBuilder str = new StringBuilder();
            const int columnStartPosition = 60;
            const int offsetBetweenValue = 8;

            str.Append(new string(' ', Math.Max(columnStartPosition - lineLength - startPosition.Length, 0)) + startPosition);
            str.Append(new string(' ', offsetBetweenValue - physicalPosition.Length) + physicalPosition);
            str.Append(new string(' ', offsetBetweenValue - physicalLength.Length) + physicalLength);

            return str.ToString();
        }

        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();
            var dataDefinitions = new List<DataDefinition>();
            result.AppendLine("Diagnostics");
            result.AppendLine("------------");
            var diagnostics = compilationResult.AllDiagnostics();
            if (diagnostics.Count > 0)
            {
                foreach (var diagnostic in compilationResult.AllDiagnostics())
                {
                    result.AppendLine(diagnostic.ToString());
                }
            }
            else
            {
                result.AppendLine("  None");
            }

            result.AppendLine("   ");
            result.AppendLine("--------- FIELD LEVEL|NAME ---------- START     END  LENGTH");

            foreach (var program in compilationResult.ProgramClassDocumentSnapshot.Root.Programs)
            {
                foreach (var node in program.Children.First(c => c is DataDivision).Children.Where(c => c is DataSection))
                {
                    dataDefinitions.AddRange(GetDataDefinitions(node));
                }

                foreach (var dataDefinition in dataDefinitions)
                {
                    //TODO: Issue #1192 handle correctly a DataRenames
                    if (dataDefinition is DataRenames == false && dataDefinition.CodeElement?.LevelNumber?.Value != 88)
                        result.AppendLine(CreateLine(dataDefinition, dataDefinition.SlackBytes == 0));
                }
            }

            return result.ToString();
        }
    }

    internal class AntlrProfiling : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history) => compilationResult.AntlrResult;
    }

    internal class Documentation : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();
            var diagnostics = compilationResult.AllDiagnostics();
            foreach (var diagnostic in diagnostics)
            {
                result.AppendLine(diagnostic.ToString());
            }

            result.AppendLine("======================== Documentation ========================");
            var documentedNodes = ParserUtils.GetDocumentedNodes(compilationResult.ProgramClassDocumentSnapshot.Root);
            foreach (var node in documentedNodes)
            {
                var doc = TypeCobol.Compiler.Nodes.Documentation.CreateAppropriateDocumentation(node);
                result.Append(doc.SerializeToJson(true));
                result.AppendLine();
                result.AppendLine("---------------------");
                result.AppendLine();
            }

            return result.ToString();
        }
    }

    internal class DocumentationProperties : ICompilationResultFormatter
    {
        private static void WriteDocumentedNodeProperties(TypeCobol.Compiler.Nodes.Documentation doc, StringBuilder sb)
        {
            sb.AppendLine("Name : " + doc.Name);
            sb.AppendLine("Description : " + doc.Description);
            sb.AppendLine("Visibility : " + doc.Visibility);
            sb.AppendLine("Namespace : " + doc.Namespace);
            sb.AppendLine("NodeType : " + (doc.IsTypeDef
                ? "TypeDef"
                : (doc.IsFunction
                    ? "Function"
                    : (doc.IsProgram
                        ? "Program"
                        : ""))));

            sb.AppendLine("IsDeprecated : " + doc.IsDeprecated);
            sb.AppendLine("Deprecated : " + doc.Deprecated);
            sb.AppendLine("ReplacedBy : " + doc.ReplacedBy);
            sb.AppendLine("Restriction : " + doc.Restriction);
            sb.AppendLine("See : " + doc.See);
            sb.AppendLine("Needs : ");
            foreach (var need in doc.Needs ?? Enumerable.Empty<string>())
                sb.AppendLine("    " + need);
            sb.AppendLine("ToDo : ");
            foreach (var toDo in doc.ToDo ?? Enumerable.Empty<string>())
                sb.AppendLine("    " + toDo);

            // Typedefs Specific:
            if (doc is DocumentationForType typeDoc)
            {
                sb.AppendLine("IsBlankWhenZero : " + typeDoc.IsBlankWhenZero);
                sb.AppendLine("Justified : " + typeDoc.Justified);
                sb.AppendLine("DocDataType : " + typeDoc.DocDataType);
                WriteDocDataType(sb, typeDoc.DocDataType, 0);
                // Children is not initialized with an empty list to prevent the documentation export flooding
                if (typeDoc.Childrens != null)
                {
                    foreach (var child in typeDoc.Childrens)
                    {
                        WriteTypeDefChildren(sb, child, 0);
                    }
                }
            }

            // Programs and Functions Specific:
            DocumentationForFunction funcDoc = doc as DocumentationForFunction;
            DocumentationForProgram pgmDoc = doc as DocumentationForProgram;
            if (funcDoc != null || pgmDoc != null)
            {
                sb.AppendLine("Parameters : ");
                bool isFirstParam = true;
                foreach (var param in (funcDoc?.Parameters ?? pgmDoc?.Parameters) ?? Enumerable.Empty<DocumentationParameter>())
                {
                    if (isFirstParam)
                        isFirstParam = false;
                    else
                        sb.AppendLine("    --------");
                    sb.AppendLine("    " + "Name : " + param.Name);
                    sb.AppendLine("    " + "Info : " + param.Info);
                    sb.AppendLine("    " + "PassingType : " + param.PassingType);
                    WriteDocDataType(sb, param.DocDataType, 1);
                }
            }
        }

        private static void WriteDocumentedCodeElementProperties(TypeCobol.Compiler.Nodes.Documentation doc, StringBuilder sb)
        {
            sb.AppendLine("== " + doc.Name + " ==");
            FormalizedCommentDocumentation formCom = doc.FormCom;

            if (formCom != null)
            {
                sb.AppendLine("Description : " + formCom.Description);
                sb.AppendLine("Deprecated : " + (formCom.Deprecated == "" ? "*is present*" : formCom.Deprecated));
                sb.AppendLine("ReplacedBy : " + formCom.ReplacedBy);
                sb.AppendLine("Restriction : " + formCom.Restriction);
                sb.AppendLine("See : " + formCom.See);
                sb.AppendLine("Needs : ");
                foreach (var need in formCom.Needs ?? Enumerable.Empty<string>())
                    sb.AppendLine("    " + need);
                sb.AppendLine("ToDo : ");
                foreach (var toDo in formCom.ToDo ?? Enumerable.Empty<string>())
                    sb.AppendLine("    " + toDo);
                sb.AppendLine("Parameters : ");
                foreach (var parameter in formCom.Parameters ?? new Dictionary<string, string>())
                {
                    sb.AppendLine("    " + parameter.Key + " : " + parameter.Value);

                }
            }
            else
                sb.AppendLine("No formalized comment found");
        }

        private static void WriteDocDataType(StringBuilder sb, DocumentationDataType docDataType, int level)
        {
            sb.AppendLine(new string(' ', level * 4) + "DataType : ");
            sb.AppendLine(new string(' ', (level + 1) * 4) + "Usage : " + docDataType.Usage);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "MaxOccurence : " + docDataType.MaxOccurence);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "DefaultValue : " + docDataType.DefaultValue);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "TypeName : " + docDataType.TypeName);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "Picture : " + docDataType.Picture);
        }

        private static void WriteTypeDefChildren(StringBuilder sb, DocumentationTypeChildren child, int level)
        {
            sb.AppendLine(new string(' ', level * 4) + "TypeDefChild : ");
            sb.AppendLine(new string(' ', (level + 1) * 4) + "Name : " + child.Name);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "IsBlankWhenZero : " + child.IsBlankWhenZero);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "Justified : " + child.Justified);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "IsLevel77 : " + child.IsLevel77);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "IsLevel88 : " + child.IsLevel88);

            sb.AppendLine(new string(' ', (level + 1) * 4) + "ConditionValues : ");
            if (child.ConditionValues != null)
            {
                for (int i = 0; i < child.ConditionValues.Length; i++)
                {
                    sb.AppendLine(new string(' ', (level + 2) * 4) + $"Condition{i} : {child.ConditionValues[i]}");
                }
            }

            sb.AppendLine(new string(' ', (level + 1) * 4) + "ConditionValuesRanges : ");
            if (child.ConditionValuesRanges != null)
            {
                for (int i = 0; i < child.ConditionValuesRanges.Length; i++)
                {
                    sb.AppendLine(new string(' ', (level + 2) * 4) + $"ConditionRanges{i} : {child.ConditionValuesRanges[i].MinValue} -> {child.ConditionValuesRanges[i].MaxValue}");
                }
            }

            WriteDocDataType(sb, child.DocDataType, level + 1);

            if (child.Childrens != null)
            {
                foreach (var subChild in child.Childrens)
                {
                    WriteTypeDefChildren(sb, subChild, level + 1);
                }
            }
        }

        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();

            var diagnostics = compilationResult.AllDiagnostics();
            foreach (var diagnostic in diagnostics)
            {
                result.AppendLine(diagnostic.ToString());
            }

            result.AppendLine("======================== Nodes properties ========================");
            var documentedNodes = ParserUtils.GetDocumentedNodes(compilationResult.ProgramClassDocumentSnapshot.Root);
            foreach (var node in documentedNodes)
            {
                var doc = TypeCobol.Compiler.Nodes.Documentation.CreateAppropriateDocumentation(node);
                WriteDocumentedNodeProperties(doc, result);
                result.AppendLine();
                result.AppendLine("---------------------");
                result.AppendLine();
            }

            result.AppendLine("======================== Code Element properties ========================");
            foreach (var node in documentedNodes)
            {
                var doc = TypeCobol.Compiler.Nodes.Documentation.CreateAppropriateDocumentation(node);
                WriteDocumentedCodeElementProperties(doc, result);
                result.AppendLine();
                result.AppendLine("---------------------");
                result.AppendLine();
            }

            return result.ToString();
        }
    }

    internal class SqlStatements : ICompilationResultFormatter
    {
        /// <summary>
        /// Selects the SQL code elements
        /// </summary>
        private class ASTVisitor : AbstractAstVisitor
        {
            private readonly StringWriter _writer;

            public ASTVisitor(StringBuilder builder)
            {
                _writer = new StringWriter(builder);
            }

            private void DumpObject(string name, object value)
            {
                SqlObject.DumpProperty(_writer, name, value, 0);
            }

            public override bool Visit(SelectStatement selectStatement)
            {
                _writer.WriteLine($"line {selectStatement.Line}: {nameof(SelectStatement)}");
                DumpObject(nameof(selectStatement.FullSelect), selectStatement.FullSelect);
                return true;
            }

            public override bool Visit(RollbackStatement rollbackStatement)
            {
                _writer.WriteLine($"line {rollbackStatement.Line}: {nameof(RollbackStatement)}");
                DumpObject(nameof(rollbackStatement.SavePointClause), rollbackStatement.SavePointClause);
                return true;
            }

            public override bool Visit(CommitStatement commitStatement)
            {
                _writer.WriteLine($"line {commitStatement.Line}: {nameof(CommitStatement)}");
                //No SqlObject in CommitStatement
                return true;
            }

            public override bool Visit(TruncateStatement truncateStatement)
            {
                _writer.WriteLine($"line {truncateStatement.Line}: {nameof(TruncateStatement)}");
                DumpObject(nameof(truncateStatement.TableName), truncateStatement.TableName);
                DumpObject(nameof(truncateStatement.StorageManagement), truncateStatement.StorageManagement);
                DumpObject(nameof(truncateStatement.DeleteTriggersHandling), truncateStatement.DeleteTriggersHandling);
                DumpObject(nameof(truncateStatement.IsImmediate), truncateStatement.IsImmediate);

                return true;
            }

            public override bool Visit(WhenEverStatement whenEverStatement)
            {
                _writer.WriteLine($"line {whenEverStatement.Line}: {nameof(WhenEverStatement)}");
                DumpObject(nameof(whenEverStatement.ExceptionCondition), whenEverStatement.ExceptionCondition);
                DumpObject(nameof(whenEverStatement.BranchingType), whenEverStatement.BranchingType);
                DumpObject(nameof(whenEverStatement.TargetSectionOrParagraph), whenEverStatement.TargetSectionOrParagraph);
                return true;
            }

            public override bool Visit(SavepointStatement savepointStatement)
            {
                _writer.WriteLine($"line {savepointStatement.Line}: {nameof(SavepointStatement)}");
                DumpObject(nameof(savepointStatement.Name), savepointStatement.Name);
                DumpObject(nameof(savepointStatement.IsUnique), savepointStatement.IsUnique);
                DumpObject(nameof(savepointStatement.RetainLocks), savepointStatement.RetainLocks);
                return true;
            }

            public override bool Visit(LockTableStatement lockTableStatement)
            {
                _writer.WriteLine($"line {lockTableStatement.Line}: {nameof(LockTableStatement)}");
                DumpObject(nameof(lockTableStatement.Table), lockTableStatement.Table);
                DumpObject(nameof(lockTableStatement.PartitionId), lockTableStatement.PartitionId);
                DumpObject(nameof(lockTableStatement.Mode), lockTableStatement.Mode);
                return true;
            }

            public override bool Visit(ReleaseSavepointStatement releaseSavepointStatement)
            {
                _writer.WriteLine($"line {releaseSavepointStatement.Line}: {nameof(ReleaseSavepointStatement)}");
                DumpObject(nameof(releaseSavepointStatement.SavepointName), releaseSavepointStatement.SavepointName);
                return true;
            }

            public override bool Visit(ConnectStatement connectStatement)
            {
                _writer.WriteLine($"line {connectStatement.Line}: {nameof(ConnectStatement)}");
                DumpObject(nameof(connectStatement.Target), connectStatement.Target);
                DumpObject(nameof(connectStatement.Authorization), connectStatement.Authorization);
                DumpObject(nameof(connectStatement.Reset), connectStatement.Reset);
                return true;
            }

            public override bool Visit(DropTableStatement dropTableStatement)
            {
                _writer.WriteLine($"line {dropTableStatement.Line}: {nameof(DropTableStatement)}");
                DumpObject(nameof(dropTableStatement.TableOrAliasName), dropTableStatement.TableOrAliasName);
                return true;
            }

            public override bool Visit(SetAssignmentStatement setAssignmentStatement)
            {
                _writer.WriteLine($"line {setAssignmentStatement.Line}: {nameof(SetAssignmentStatement)}");
                DumpObject(nameof(setAssignmentStatement.Assignments), setAssignmentStatement.Assignments);
                return true;
            }

            public override bool Visit(GetDiagnosticsStatement getDiagnosticsStatement)
            {
                _writer.WriteLine($"line {getDiagnosticsStatement.Line}: {nameof(GetDiagnosticsStatement)}");
                DumpObject(nameof(getDiagnosticsStatement.IsCurrent), getDiagnosticsStatement.IsCurrent);
                DumpObject(nameof(getDiagnosticsStatement.IsStacked), getDiagnosticsStatement.IsStacked);
                DumpObject(nameof(getDiagnosticsStatement.RequestedInformation), getDiagnosticsStatement.RequestedInformation);
                return true;
            }

            public override bool Visit(AlterSequenceStatement alterSequenceStatement)
            {
                _writer.WriteLine($"line {alterSequenceStatement.Line}: {nameof(AlterSequenceStatement)}");
                DumpObject(nameof(AlterSequenceStatement.SequenceName), alterSequenceStatement.SequenceName);
                DumpObject(nameof(AlterSequenceStatement.Restart), alterSequenceStatement.Restart);
                DumpObject(nameof(AlterSequenceStatement.RestartValue), alterSequenceStatement.RestartValue);
                DumpObject(nameof(AlterSequenceStatement.IncrementValue), alterSequenceStatement.IncrementValue);
                DumpObject(nameof(AlterSequenceStatement.HasMinValue), alterSequenceStatement.HasMinValue);
                DumpObject(nameof(AlterSequenceStatement.MinValue), alterSequenceStatement.MinValue);
                DumpObject(nameof(AlterSequenceStatement.HasMaxValue), alterSequenceStatement.HasMaxValue);
                DumpObject(nameof(AlterSequenceStatement.MaxValue), alterSequenceStatement.MaxValue);
                DumpObject(nameof(AlterSequenceStatement.Cycle), alterSequenceStatement.Cycle);
                DumpObject(nameof(AlterSequenceStatement.HasCache), alterSequenceStatement.HasCache);
                DumpObject(nameof(AlterSequenceStatement.CacheSize), alterSequenceStatement.CacheSize);
                DumpObject(nameof(AlterSequenceStatement.Ordered), alterSequenceStatement.Ordered);
                return true;
            }
            public override bool Visit(ExecuteImmediateStatement executeImmediateStatement)
            {
                _writer.WriteLine($"line {executeImmediateStatement.Line}: {nameof(ExecuteImmediateStatement)}");
                DumpObject(nameof(executeImmediateStatement.StatementVariable), executeImmediateStatement.StatementVariable);
                DumpObject(nameof(executeImmediateStatement.StatementExpression), executeImmediateStatement.StatementExpression);
                return true;
            }
        }

        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();

            var diagnostics = compilationResult.AllDiagnostics();
            if (diagnostics.Count > 0)
            {
                result.AppendLine(ParserUtils.DiagnosticsToString(diagnostics));
            }

            result.AppendLine("--- Sql Statements ---");
            compilationResult.ProgramClassDocumentSnapshot.Root.AcceptASTVisitor(new ASTVisitor(result));

            return result.ToString();
        }
    }

    internal class FullText : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            // Build full source code from lines
            var actual = new StringBuilder();
            foreach (var tokensLine in compilationResult.TokensDocumentSnapshot.Lines)
            {
                actual.AppendLine(tokensLine.Text);
            }

            return actual.ToString();
        }
    }

    internal class IncrementalChanges : ICompilationResultFormatter
    {
        private static void DumpAnyTokens(StringBuilder output, IEnumerable<IToken> tokens)
        {
            foreach (var token in tokens)
            {
                output.Append(token);
                output.Append(' ');
            }
        }

        private static void DumpCobolTextLine(StringBuilder output, ICobolTextLine cobolTextLine)
        {
            output.AppendLine(cobolTextLine.Text);
        }

        private static void DumpTokensLine(StringBuilder output, ITokensLine tokensLine)
        {
            DumpAnyTokens(output, tokensLine.SourceTokens);
            output.AppendLine();
        }

        private static void DumpProcessedTokensLine(StringBuilder output, IProcessedTokensLine processedTokensLine)
        {
            if (processedTokensLine.HasCompilerDirectives)
            {
                DumpAnyTokens(output, processedTokensLine.TokensWithCompilerDirectives);
            }

            output.AppendLine();
        }

        private static void DumpCodeElementsLine(StringBuilder output, ICodeElementsLine codeElementsLine)
        {
            if (codeElementsLine.CodeElements != null)
            {
                DumpAnyTokens(output, codeElementsLine.CodeElements);
            }

            output.AppendLine();
        }

        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            if (history == null)
            {
                // This comparator cannot be used during an initial comparison.
                throw new ArgumentNullException(nameof(history), "Cannot use incremental changes formatter without change history !");
            }

            // Dump full incremental history
            var actual = new StringBuilder();
            DumpEvents(history.TextChangedEvents, DumpCobolTextLine);
            DumpEvents(history.TokensChangedEvents, DumpTokensLine);
            DumpEvents(history.ProcessedTokensChangedEvents, DumpProcessedTokensLine);
            DumpEvents(history.CodeElementsChangedEvents, DumpCodeElementsLine);
            return actual.ToString();

            void WriteSeparator(string title, char separatorChar = '=')
            {
                int paddingLength = 78 - title.Length;
                string left = new string(separatorChar, paddingLength / 2);
                string right = new string(separatorChar, paddingLength / 2 + paddingLength % 2);
                actual.AppendLine($"{left} {title} {right}");
            }

            void DumpEvents<TLine>(IEnumerable<DocumentChangedEvent<TLine>> changeEvents, Action<StringBuilder, TLine> dumpLine)
                where TLine : ICobolTextLine
            {
                WriteSeparator(typeof(TLine).Name);

                bool hasChanges = false;
                DocumentVersion<TLine> first = null;
                DocumentVersion<TLine> last = null;
                foreach (var changeEvent in changeEvents)
                {
                    if (first == null)
                    {
                        first = changeEvent.DocumentVersionBefore;
                    }

                    last = changeEvent.DocumentVersionAfter;

                    DumpChanges(changeEvent.DocumentChanges);
                }

                if (hasChanges)
                {
                    if (first != null && last != null && first != last)
                    {
                        WriteSeparator(nameof(DocumentVersion<TLine>.GetReducedAndOrderedChangesInNewerVersion), '-');
                        var reducedChanges = first.GetReducedAndOrderedChangesInNewerVersion(last);
                        DumpChanges(reducedChanges);
                    }
                }
                else
                {
                    actual.AppendLine("No change");
                }

                void DumpChanges(IEnumerable<DocumentChange<TLine>> documentChanges)
                {
                    foreach (var documentChange in documentChanges)
                    {
                        hasChanges = true;
                        string text = $"Line {documentChange.LineIndex,+4}: {documentChange.Type,-15} -> "; // Line number on 4 columns right align, change type on 15 columns left align
                        actual.Append(text);
                        if (documentChange.NewLine != null)
                        {
                            dumpLine(actual, documentChange.NewLine);
                        }
                        else
                        {
                            actual.AppendLine("No new line");
                        }
                    }
                }
            }
        }
    }

    internal class Copies : ICompilationResultFormatter
    {
        public string Format(CompilationUnit compilationResult, IncrementalChangesHistory history)
        {
            var result = new StringBuilder();

            // Diagnostics
            var diagnostics = compilationResult.AllDiagnostics();
            if (diagnostics.Count > 0)
            {
                result.AppendLine(ParserUtils.DiagnosticsToString(diagnostics));
            }

            // Dump CopyTextNamesVariations
            result.AppendLine("--- CopyTextNamesVariations ---");
            foreach (var textNameVariation in compilationResult.CopyTextNamesVariations)
            {
                result.AppendLine(textNameVariation.ToString());
            }

            return result.ToString();
        }
    }
}
