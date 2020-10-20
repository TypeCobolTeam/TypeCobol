using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Report
{
    public class ZCallPgmReport : SyntaxDrivenAnalyzerBase, IReport
    {
        private static readonly string[] _ZCallPgmVariants = {
                                                                 "zcallpgm",
                                                                 "zcallpgf",
                                                                 "zcallpgg",
                                                                 "zcallpgr",
                                                                 "zcallpgt",
                                                                 "zcallpgx",
                                                                 "zcallsrv"
                                                             };

        private static void ReportVariablesWritten(TextWriter writer, Call call)
        {
            CallStatement callStatement = call.CodeElement;
            var pgmCalled = call
                .SymbolTable
                .DataEntries
                .First(s => s.Key == callStatement.InputParameters.First().StorageAreaOrValue.MainSymbolReference.Name)
                .Value;

            if (pgmCalled != null)
            {
                foreach (DataDefinition pgm in pgmCalled)
                {
                    ReportVariable(writer, call, pgm);
                }
            }
        }

        private static void ReportVariable(TextWriter writer, Call call, DataDefinition pgm)
        {
            var name = ((DataDescriptionEntry) pgm.CodeElement).InitialValue;
            string sourceText = call.CodeElement.SourceText.Replace('\r', ' ').Replace('\n', ' ').Trim();
            int line = call.CodeElement.Line;
            int column = call.CodeElement.Column;

            //Remove all unneeded space
            sourceText = Regex.Replace(sourceText, @"\s+", " ");

            writer.WriteLine($"ObjectName={name};Line={line};Column={column};SourceText={sourceText}");
        }

        private readonly List<Call> _calls;

        public ZCallPgmReport()
            : base(nameof(ZCallPgmReport))
        {
            _calls = new List<Call>();
        }

        /// <summary>
        /// Collects all call statements to ZCALLPGM and its variants.
        /// </summary>
        /// <param name="node">Current Node built.</param>
        /// <param name="program">Owner program of the node.</param>
        public override void OnNode(Node node, Program program)
        {
            if (node.CodeElement != null)
            {
                switch (node.CodeElement.Type)
                {
                    case CodeElementType.CallStatement:
                        var target = node.CodeElement.CallSites.First().CallTarget;
                        foreach (var callStyle in _ZCallPgmVariants)
                        {
                            if (target != null && target.ToString().Equals(callStyle, StringComparison.OrdinalIgnoreCase))
                                _calls.Add((Call) node);
                        }
                        
                        break;
                }
            }
        }

        /// <summary>
        /// Result of this analyzer.
        /// </summary>
        /// <returns>A List of Call nodes.</returns>
        public override object GetResult() => _calls;

        public void Report(TextWriter writer, CompilationUnit unit = null)
        {
            foreach (Call call in _calls)
            {
                ReportVariablesWritten(writer, call);
            }
        }
    }
}
