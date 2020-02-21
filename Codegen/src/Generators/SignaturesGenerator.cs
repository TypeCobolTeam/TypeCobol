using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Diagnostics;
using DataDivision = TypeCobol.Compiler.Nodes.DataDivision;
using FunctionDeclaration = TypeCobol.Compiler.Nodes.FunctionDeclaration;
using LocalStorageSection = TypeCobol.Compiler.Nodes.LocalStorageSection;
using ProcedureDivision = TypeCobol.Compiler.Nodes.ProcedureDivision;
using WorkingStorageSection = TypeCobol.Compiler.Nodes.WorkingStorageSection;
using System.Text;
using TypeCobol.Codegen.Actions;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Tools;
using LinkageSection = TypeCobol.Compiler.Nodes.LinkageSection;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// The Default Generator
    /// </summary>
    public class SignaturesGenerator : IGenerator
    {
        private StringBuilder Destination { get; set; }
        private CompilationUnit CompilationUnit { get; set; }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="Document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        /// <param name="typeCobolVersion">Version of the TypeCobol Parser/Generator</param>
        public SignaturesGenerator(StringBuilder destination, string typeCobolVersion) {
            this.Destination = destination;
            TypeCobolVersion = typeCobolVersion;
        }
   


        public void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat)
        {
            var stopwatch = Stopwatch.StartNew();

            this.CompilationUnit = compilationUnit;
            Destination.Append("");
            //Add version to output file
            if (!string.IsNullOrEmpty(TypeCobolVersion))
                Destination.AppendLine("      *TypeCobol_Version:" + TypeCobolVersion);

            var sourceFile = compilationUnit.ProgramClassDocumentSnapshot.Root;
            sourceFile.AcceptASTVisitor(new ExportToDependency());
            bool insideMultilineComment = false;

            var buildExportDataElapsed = stopwatch.Elapsed;
            stopwatch.Restart();

            foreach (var textLine in GenerateLinesForChildren(sourceFile.Children))
            {
                bool shouldWriteLine = true;
                if (textLine is ITokensLine tokensLine)
                {
                    if (tokensLine.SourceTokens.Any(t => t.TokenType == TokenType.CommentLine))
                        shouldWriteLine = false;

                    if (tokensLine.SourceTokens.Any(t => t.TokenType == TokenType.MULTILINES_COMMENTS_START))
                    {
                        shouldWriteLine = false;
                        insideMultilineComment = true;
                    }

                    if (tokensLine.SourceTokens.Any(t => t.TokenType == TokenType.MULTILINES_COMMENTS_STOP))
                    {
                        shouldWriteLine = false;
                        insideMultilineComment = false;
                    }
                }

                if (shouldWriteLine && !insideMultilineComment)
                {
                    if (textLine is TextLineSnapshot)
                    {
                        var test = CobolTextLine.Create(textLine.Text, ColumnsLayout.CobolReferenceFormat);
                        Destination.AppendLine(test.First().Text);
                    }
                    else
                    {
                        Destination.AppendLine(textLine.Text);
                    }
                }
            }

            var writeExportDataElapsed = stopwatch.Elapsed;
            stopwatch.Reset();

            PerformanceReport = new Dictionary<string, TimeSpan>()
                                {
                                    {"BuildExportData", buildExportDataElapsed},
                                    {"WriteExportData", writeExportDataElapsed}
                                };
        }

        /// <summary>
        /// Select or create the lines for each node that can be exported
        /// </summary>
        /// <param name="children"></param>
        /// <returns></returns>
        private IEnumerable<ITextLine> GenerateLinesForChildren(IReadOnlyList<Node> children)
        {
            var lines = new List<ITextLine>();
            //Contains the copy directives that have already been generated
            List<CopyDirective> generatedCopyDirectives = new List<CopyDirective>();

            foreach (var child in children)
            {
                if (child.IsInsideCopy() && child.CodeElement != null && this.CompilationUnit != null)
                {
                    //If the child comes from a copy directive
                    CopyDirective copy = child.CodeElement.FirstCopyDirective;
                    //Check if the line corresponding to this copy has already been generated
                    if (generatedCopyDirectives.Contains(copy) == false)
                    {
                        int copyLine = copy.COPYToken.Line;
                        // Check if the original line of the copy is stored in the CobolTextLines
                        if (copyLine > 0 && copyLine <= this.CompilationUnit.CobolTextLines.Count)
                        {
                            if (copy.COPYToken != null)
                            {
                                //Collect all COPY Tokens
                                var copyTokens = copy.ConsumedTokens.SelectedTokensOnSeveralLines.SelectMany(tokenLine => tokenLine).ToList();

                                //Create node with the COPY Tokens
                                LinearNodeSourceCodeMapper.LinearCopyNode copyNode =
                                    new LinearNodeSourceCodeMapper.LinearCopyNode(new Qualifier.TokenCodeElement(copyTokens));

                                var linesContent = copyNode.CodeElement.SourceText.Split(new string[] {System.Environment.NewLine}, System.StringSplitOptions.None);

                                //Indent the line according to its declaration
                                foreach (string line in linesContent)
                                {
                                    //Only the line containing copy can be badly indented. 
                                    string lineText;
                                    if (line.IndexOf("COPY", StringComparison.OrdinalIgnoreCase) >= 0)
                                    {
                                        //Indent this line with the same indentation than the declaring line.
                                        lineText = copyNode.Lines.First().Text.GetIndent() + line.Trim();
                                    }
                                    else
                                    {
                                        //Otherwise generate the other lines as is, with a little extra for the columns 1-7
                                        lineText = new string(' ', 7) + line.TrimEnd();
                                    }
                                    
                                    lines.Add(new CobolTextLine(new TextLineSnapshot(-1,  lineText, null), ColumnsLayout.CobolReferenceFormat));
                                }
                                generatedCopyDirectives.Add(copy);
                            }
                        }
                    }
                }
                else
                {
                    //All the other cases
                    if (child is DataDescription data)
                    {
                        //In case the node contains a line with multiple instructions
                        //Create new line containing only the CodeElement text
                        string indent = child.Lines.First().Text.GetIndent();
                        var lineText = indent + data.CodeElement.SourceText.Trim().Replace(Environment.NewLine, Environment.NewLine + indent);
                        lines.Add(new CobolTextLine(new TextLineSnapshot(data.CodeElement.Line, lineText, null), ColumnsLayout.CobolReferenceFormat));
                    }
                    else
                    {
                        lines.AddRange(child.Lines);
                    }

                    lines.AddRange(GenerateLinesForChildren(child.Children));
                }
            }

            return lines;
        }

        public void GenerateLineMapFile(Stream stream)
        {            
        }

        public List<Diagnostic> Diagnostics { get; }
        public IReadOnlyDictionary<string, TimeSpan> PerformanceReport { get; private set; }
        public string TypeCobolVersion { get; }

        public bool HasLineMapData => false;
    }



    public class ExportToDependency : AbstractAstVisitor
    {

        private bool _isInsideProcedureDivision = false;

        public override bool IsStopVisitingChildren
        {
            get { return false; }
        }

        /// <summary>
        /// CodeGen visitor can modify Node's children
        /// </summary>
        public override bool CanModifyChildrenNode => true;

        public override bool BeginNode(Node node)
        {
            if (_isInsideProcedureDivision && !(node is FunctionDeclaration) && !(node is End))
            {
                node.Remove();
                return false;
            }
            return true;
        }


        public override bool Visit(FunctionDeclaration node)
        {
            if (node.CodeElement.Visibility != AccessModifier.Public) {
                node.Remove();
                return false;
            }
            //Note : There is only 1 FunctionEnd
            var functionEnds = node.GetChildren<FunctionEnd>();
            node.RemoveAllChildren();
            foreach (var child in functionEnds)
            {
                node.Add(child);
            }

            return false;
        }


        public override bool Visit(DataDivision dataDivision)
        {
            if (_isInsideProcedureDivision) {
                dataDivision.Remove();
                return false;
            }
               
            
            return true;
        }

        public override bool Visit(LocalStorageSection localStorageSection) {
            RemoveNonPublicDataDeclaration(localStorageSection);
            return false;
        }
        public override bool Visit(WorkingStorageSection workingStorageSection) {
            RemoveNonPublicDataDeclaration(workingStorageSection);
            return false;
        }

        /// <summary>
        /// Check the Linkage section for private variables
        /// </summary>
        /// <param name="linkageSection"></param>
        /// <returns></returns>
        public override bool Visit(LinkageSection linkageSection)
        {
            RemoveNonPublicDataDeclaration(linkageSection);
            //remove linkage section if it doesn't contain any children
            if (linkageSection.Children.Count == 0)
            {
                linkageSection.Remove();
            }
            return false;
        }

        private void RemoveNonPublicDataDeclaration(Node parent) {
            //keep all public type
            var children = parent.GetChildren<Node>();
            foreach (var child in children)
            {
                var typeDefinition = child as TypeDefinition;
                if (typeDefinition == null || typeDefinition.CodeElement.Visibility != AccessModifier.Public)
                {
                    child.Remove();
                }
            }
        }




        public override bool Visit(ProcedureDivision procedureDivision)
        {
            _isInsideProcedureDivision = true;
            return true;
        }

    }
}
