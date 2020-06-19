using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.Report
{
    public class ZCallPgmReport : AbstractReport, INodeListener
    {
        /// <summary>
        /// The list of all ZCALLXXX we need to detect
        /// </summary>
        public List<string> AlternativeCallList = new List<string>()
        {
            "zcallpgm",
            "zcallpgf",
            "zcallpgg",
            "zcallpgr",
            "zcallpgt",
            "zcallpgx",
            "zcallsrv",
        };
        /// <summary>
        /// The list of all CallStatement Nodes
        /// </summary>
        public List<Call> CallNodes { get; private set; }

        /// <summary>
        /// Internal Writer.
        /// </summary>
        private TextWriter Writer { get; set; }

        public ZCallPgmReport(string filepath)
        {
            Filepath = filepath;
            CallNodes = new List<Call>();
        }

        public void OnNode(Node node, Program program)
        {
            if (node.CodeElement != null)
            {
                switch (node.CodeElement.Type)
                {
                    case CodeElements.CodeElementType.CallStatement:
                        var target = node.CodeElement.CallSites.First().CallTarget;
                        foreach (var callStyle in AlternativeCallList)
                        {
                            if (target != null && target.ToString().Equals(callStyle, StringComparison.OrdinalIgnoreCase))
                                CallNodes.Add(node as Call);
                        }
                        
                        break;
                }
            }
        }

        public override void Report(TextWriter writer)
        {
            try
            {
                Writer = writer;
                foreach (Call call in CallNodes)
                {
                    ReportVariablesWritten(call);
                }
            }
            catch (Exception e)
            {
                throw new Exception(string.Format(
                    "Failed to emit report '{0}' on CALL statements : {1}",
                    ((writer as StreamWriter).BaseStream as FileStream).Name, e.Message));
            }
        }

        /// <summary>
        /// Report Call statement
        /// </summary>
        /// <param name="call">The Initialize statement</param>
        private void ReportVariablesWritten(Call call)
        {
            CallStatement callStatement = call.CodeElement as CallStatement;
            var pgmCalled = call.SymbolTable.DataEntries.First(s =>
                s.Key == callStatement?.InputParameters.First().StorageAreaOrValue.MainSymbolReference.Name).Value;

            if (pgmCalled != null)
            {
                foreach (DataDefinition pgm in pgmCalled)
                {
                    ReportVariable(call, pgm);
                }
            }
        }

        /// <summary>
        /// Report a call.
        /// </summary>
        /// <param name="node">Node that contains the variable</param>
        private void ReportVariable(Call call, DataDefinition pgm)
        {
            var name = ((DataDescriptionEntry) pgm.CodeElement).InitialValue;
            string sourceText = call.CodeElement.SourceText.Replace('\r', ' ').Replace('\n', ' ').Trim();
            int line = call.CodeElement.Line;
            int column = call.CodeElement.Column;

            //Remove all unneeded space
            sourceText = Regex.Replace(sourceText, @"\s+", " ");

            Writer.WriteLine(
                string.Format("ObjectName={0};Line={1};Column={2};SourceText={3}",
                    name, line, column, sourceText));

        }
    }
}
