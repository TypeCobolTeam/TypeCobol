using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Report
{
    public class CopyMoveInitializeReport : SyntaxDrivenAnalyzerBase, IReport
    {
        /// <summary>
        /// Report Variables written by an Initialize statement
        /// </summary>
        /// <param name="writer">Output TextWriter</param>
        /// <param name="initialize">The Initialize statement</param>
        private static void ReportVariablesWritten(TextWriter writer, Initialize initialize)
        {
            ReceivingStorageArea[] receivings = initialize.CodeElement.ReceivingStorageAreas;
            if (receivings != null)
            {
                foreach (ReceivingStorageArea receiving in receivings)
                {
                    ReportVariable(writer, initialize, receiving.StorageArea);
                }
            }
        }

        /// <summary>
        /// Report all variables written by a move statement.
        /// </summary>
        /// <param name="writer">Output TextWriter</param>
        /// <param name="move">The Move statement</param>
        private static void ReportVariablesWritten(TextWriter writer, Move move)
        {
            IDictionary<StorageArea, object> variables = move.VariablesWritten;
            foreach (var variable in variables)
            {
                ReportVariable(writer, move, variable.Key);
            }
        }

        /// <summary>
        /// Report a variable.
        /// </summary>
        /// <param name="writer">Output TextWriter</param>
        /// <param name="node">Node that contains the variable</param>
        /// <param name="variable">StorageArea instance associated to the variable</param>
        private static void ReportVariable(TextWriter writer, Node node, StorageArea variable)
        {
            StorageArea wname = variable;
            if (wname == null || !wname.NeedDeclaration)
                return;
            var area = wname.GetStorageAreaThatNeedDeclaration;
            if (area.SymbolReference == null)
                return;

            var found = node.GetDataDefinitionFromStorageAreaDictionary(wname);
            if (found != null)
            {
                //Take in account only if one instance has been discovered.
                var dataCopy = new List<DataDefinition>();
                CollectInsideCopy(found, dataCopy);
                if (dataCopy.Count > 0)
                {
                    string name = found.Name;
                    string sourceText = node.CodeElement.SourceText.Replace('\r', ' ').Replace('\n', ' ');
                    int line = node.CodeElement.Line;
                    int column = node.CodeElement.Column;

                    bool isMove = node is Move;
                    string kind = isMove ? "MOVE" : "INITIALIZE";
                    foreach (DataDefinition d in dataCopy)
                    {
#if DEBUG_REPORT_CMR_FULL_FIELDS
                        string copySourceText = d.CodeElement.SourceText.Replace('\r', ' ').Replace('\n', ' ');
                        int copyLine = d.CodeElement.Line;
                        int copyColumn = d.CodeElement.Column;
#endif
                        Preprocessor.ImportedToken firstImportedToken = d.CodeElement.ConsumedTokens.OfType<Preprocessor.ImportedToken>().First();
                        if (firstImportedToken != null)
                        {
                            string copyName = firstImportedToken.CopyDirective.TextName;
#if DEBUG_REPORT_CMR_FULL_FIELDS
                            string fileName = node.CodeElement.TokenSource.SourceName;
                            writer.WriteLine($"CopyName={copyName};{kind};Variable={name};SourceText={sourceText};Line={line};Column={column};FileName={fileName};CopySourceText={copySourceText};CopyLine={copyLine};CopyColumn={copyColumn};");
#else
                            writer.WriteLine($"CopyName={copyName};{kind};Variable={name};Line={line};Column={column};SourceText={sourceText}");
#endif
                            break; //Don't recurse within move or initialize.
                        }
                    }
                }
            }

            void CollectInsideCopy(DataDefinition data, List<DataDefinition> dataCopy)
            {
                if (data.IsInsideCopy())
                {
                    dataCopy.Add(data);
                }
                if (data.Children != null)
                {
                    foreach (var child in data.Children)
                    {
                        CollectInsideCopy(child as DataDefinition, dataCopy);
                    }
                }
            }
        }

        private readonly List<Node> _moveOrInitializeNodes;

        public CopyMoveInitializeReport()
            : base(nameof(CopyMoveInitializeReport))
        {
            _moveOrInitializeNodes = new List<Node>();
        }

        /// <summary>
        /// Collects all MOVE or INITIALIZE nodes.
        /// </summary>
        /// <param name="node">Current node being built.</param>
        /// <param name="program">Owner program of the node.</param>
        public override void OnNode(Node node, Program program)
        {
            if (node.CodeElement != null)
            {
                switch (node.CodeElement.Type)
                {
                    case CodeElementType.MoveStatement:
                    case CodeElementType.InitializeStatement:
                        _moveOrInitializeNodes.Add(node);
                        break;
                }
            }
        }

        /// <summary>
        /// Result of this analyzer.
        /// </summary>
        /// <returns>A List of Node.</returns>
        public override object GetResult() => _moveOrInitializeNodes;

        public void Report(TextWriter writer, CompilationUnit unit = null)
        {
            foreach (Node node in _moveOrInitializeNodes)
            {
                switch (node.CodeElement.Type)
                {
                    case CodeElementType.MoveStatement:
                        ReportVariablesWritten(writer, (Move) node);
                        break;
                    case CodeElementType.InitializeStatement:
                        ReportVariablesWritten(writer, (Initialize) node);
                        break;
                }
            }
        }
    }
}
