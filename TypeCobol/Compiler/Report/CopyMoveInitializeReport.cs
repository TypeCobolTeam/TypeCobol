using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.Report
{
    public class CopyMoveInitializeReport<TCtx> : AbstractReport, NodeListener<TCtx> where TCtx : class
    {
        /// <summary>
        /// The list of all MoveStatement and InitializeStatement Nodes
        /// </summary>
        public List<Node> MoveInitializeNodes
        {
            get;
            private set;
        }

        /// <summary>
        /// Internal Writer.
        /// </summary>
        private TextWriter Writer
        {
            get;
            set;
        }

        /// <summary>
        /// Empty constructor
        /// </summary>
        public CopyMoveInitializeReport()
        {
            MoveInitializeNodes = new List<Node>();
        }
        /// <summary>
        /// Collect Move and Initialize Nodes.
        /// </summary>
        /// <param name="node">The Node</param>
        /// <param name="context">The parsing context</param>
        /// <param name="program">The underlying program.</param>
        public void OnNode(Node node, TCtx context, Program program)
        {
            if (node.CodeElement != null)
            {
                switch (node.CodeElement.Type)
                {
                    case CodeElements.CodeElementType.MoveStatement:
                    case CodeElements.CodeElementType.InitializeStatement:
                        MoveInitializeNodes.Add(node);
                        break;
                }
            }
        }

        public override void Report(TextWriter writer)
        {
            Writer = writer;
            foreach (Node node in MoveInitializeNodes)
            {
                switch (node.CodeElement.Type)
                {
                    case CodeElements.CodeElementType.MoveStatement:
                        {
                            Move move = node as Move;
                            ReportVariablesWritten(move);
                        }
                        break;
                    case CodeElements.CodeElementType.InitializeStatement:
                        {
                            Initialize initialize = node as Initialize;
                            ReportVariablesWritten(initialize);
                        }
                        break;
                }
            }
        }

        /// <summary>
        /// Report Variables written by an Initialize statement
        /// </summary>
        /// <param name="initialize">The Initialize statement</param>
        private void ReportVariablesWritten(Initialize initialize)
        {
            ReceivingStorageArea[] receivings = (initialize.CodeElement as InitializeStatement).ReceivingStorageAreas;
            if (receivings != null)
            {
                foreach (ReceivingStorageArea receiving in receivings)
                {
                    ReportVariable(initialize, receiving.StorageArea);
                }
            }
        }

        /// <summary>
        /// Report all variables written by a move statement.
        /// </summary>
        /// <param name="move">The move statement</param>
        private void ReportVariablesWritten(Move move)
        {
            IDictionary<StorageArea, object> variables = move.VariablesWritten;
            foreach (var variable in variables)
            {
                ReportVariable(move, variable.Key);
            }
        }

        /// <summary>
        /// Recurive check for a DataDefinition if it has field defined inside a COPYs
        /// </summary>
        /// <param name="data">The Data to check</param>
        /// <param name="dataCopy">Output list to store all Copy data</param>
        /// <returns></returns>
        private void CollectInsideCopy(DataDefinition data, List<DataDefinition> dataCopy)
        {
            if (data.IsInsideCopy())
            {
                dataCopy.Add(data);
            }
            if (data.Children != null)
            {
                foreach (var node in data.Children)
                {
                    CollectInsideCopy(node as DataDefinition, dataCopy);
                }
            }
        }

        /// <summary>
        /// Report a variable.
        /// </summary>
        /// <param name="node">Node that contains the variable</param>
        private void ReportVariable(Node node, StorageArea variable)
        {
            StorageArea wname = variable;
            if (wname == null || !wname.NeedDeclaration)
                return;
            var area = wname.GetStorageAreaThatNeedDeclaration;
            if (area.SymbolReference == null)
                return;
            
            List<KeyValuePair<string, DataDefinition>>  foundQualified =
                node.SymbolTable.GetVariablesExplicitWithQualifiedName(area.SymbolReference != null
                    ? area.SymbolReference.URI
                    : new CodeElements.Expressions.URI(area.ToString()),
                    null);
            IEnumerable<DataDefinition> found = foundQualified.Select(v => v.Value);
            if (found.Count() == 1)
            {//Take in account only if one instance has been discovered.
                foreach (var v in found)
                {
                    List<DataDefinition> dataCopy = new List<DataDefinition>();
                    CollectInsideCopy(v, dataCopy);
                    if (dataCopy.Count > 0)
                    {
                        string name = v.Name;
                        string sourceText = node.CodeElement.SourceText.Replace('\r', ' ').Replace('\n', ' ');
                        int line = node.CodeElement.Line;
                        int column = node.CodeElement.Column;
                        string fileName = node.CodeElement.TokenSource.SourceName;

                        bool isMove = node is Move;
                        string kind = isMove ? "MOVE" : "INITIALIZE";
                        foreach (DataDefinition d in dataCopy)
                        {
#if DEBUG_REPORT_CMR_FULL_FIELDS                                                
                        string copySourceText = d.CodeElement.SourceText.Replace('\r', ' ').Replace('\n', ' ');
                        int copyLine = d.CodeElement.Line;
                        int copyColumn = d.CodeElement.Column;
#endif
                            Preprocessor.ImportedToken firstImportedToken =
                                d.CodeElement.ConsumedTokens.First(t => t is Preprocessor.ImportedToken) as
                                    Preprocessor.ImportedToken;
                            if (firstImportedToken != null)
                            {
                                string copyName = firstImportedToken.CopyDirective.TextName;
#if DEBUG_REPORT_CMR_FULL_FIELDS                                                
                            Writer.WriteLine(string.Format("CopyName={0};{1};Variable={2};SourceText={3};Line={4};Column={5};FileName={6};CopySourceText={7};CopyLine={8};CopyColumn={9};",
                            copyName, kind, name, sourceText, line, column, fileName, copySourceText, copyLine, copyColumn));
#else
                                Writer.WriteLine(
                                    string.Format("CopyName={0};{1};Variable={2};Line={3};Column={4};SourceText={5}",
                                        copyName, kind, name, line, column, sourceText));
#endif
                                break; //Don't recurse within move or initialize.
                            }
                        }
                    }
                }
            }
            return;
        }
    }
}
