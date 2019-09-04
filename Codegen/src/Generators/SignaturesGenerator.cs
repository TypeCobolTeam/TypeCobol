using System;
using System.Collections.Generic;
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
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using LinkageSection = TypeCobol.Compiler.Nodes.LinkageSection;
using String = System.String;

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
            this.CompilationUnit = compilationUnit;
            Destination.Append("");
            //Add version to output file
            if (!String.IsNullOrEmpty(TypeCobolVersion))
                Destination.AppendLine("      *TypeCobol_Version:" + TypeCobolVersion);

            var sourceFile = compilationUnit.ProgramClassDocumentSnapshot.Root;
            sourceFile.AcceptASTVisitor(new ExportToDependency());
            var lines = sourceFile.SelfAndChildrenLines;
            bool insideFormalizedComment = false;
            bool insideMultilineComment = false;

            foreach (var textLine in GenerateLinesForChildren(sourceFile.Children))
            {
                string text = textLine is TextLineSnapshot ?
                    CobolTextLine.Create(textLine.Text, ColumnsLayout.CobolReferenceFormat).First().Text :
                    textLine.Text;

                ITokensLine tokensLine = textLine as ITokensLine;
                if (tokensLine != null)
                {
                    if (tokensLine.SourceTokens.Any(t => t.TokenType == TokenType.FORMALIZED_COMMENTS_START))
                        insideFormalizedComment = true;
                    if (tokensLine.SourceTokens.Any(t => t.TokenType == TokenType.MULTILINES_COMMENTS_START))
                        insideMultilineComment = true;

                    if (insideFormalizedComment || insideMultilineComment)
                        text = text.Substring(0, 6) + '*' + text.Substring(7, text.Length - 7);

                    if (tokensLine.SourceTokens.Any(t => t.TokenType == TokenType.FORMALIZED_COMMENTS_STOP))
                        insideFormalizedComment = false;
                    if (tokensLine.SourceTokens.Any(t => t.TokenType == TokenType.MULTILINES_COMMENTS_STOP))
                        insideMultilineComment = false;
                }

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

        private IEnumerable<ITextLine> GenerateLinesForChildren(IReadOnlyList<Node> children, List<string> parentOneLinerMultiInstructionStack = null)
        {
            var lines = new List<ITextLine>();
            List<CopyDirective> generatedCopyDirectives = new List<CopyDirective>();

            List<string> oneLinerMultiInstructionStack = new List<string>();
            if (parentOneLinerMultiInstructionStack != null)
            {
                oneLinerMultiInstructionStack = parentOneLinerMultiInstructionStack;
            }

            foreach (var child in children)
            {
                if (child.IsInsideCopy() && child.CodeElement != null && this.CompilationUnit != null)
                {
                    CopyDirective copy = child.CodeElement.FirstCopyDirective;
                    if (generatedCopyDirectives.Contains(copy) == false)
                    {
                        int copyLine = copy.COPYToken.Line;
                        if (copyLine > 0 && copyLine <= this.CompilationUnit.CobolTextLines.Count)
                        {
                            ICobolTextLine copyTextLine = this.CompilationUnit.CobolTextLines[copyLine - 1];
                            var copyToken = ((Compiler.Parser.CodeElementsLine)copyTextLine).SourceTokens.FirstOrDefault(
                                t => t == copy.COPYToken);
                            if (copyToken != null)
                            {
                                //We got one ==>
                                //Collect all COPY Tokens
                                bool bEnd = false;
                                bool bCopySeen = false;
                                //A Copy of a Commented node must have is lines commented also.
                                bool bCopyCommented = child.Comment.HasValue && child.Comment.Value;
                                int lastLine = -1;

                                var copyTokens = CopyTokensYielder(copy).Where(t =>
                                {
                                    if (t == copyToken)
                                    {
                                        bCopySeen = true;
                                    }

                                    bool bResult = bCopySeen && !bEnd;
                                    if (t.TokenType == TokenType.PeriodSeparator)
                                    {
                                        bEnd = true;
                                    }

                                    return bResult;
                                });

                                //Create node with the COPY Tokens
                                LinearNodeSourceCodeMapper.LinearCopyNode copyNode =
                                    new LinearNodeSourceCodeMapper.LinearCopyNode(new Qualifier.TokenCodeElement(copyTokens.ToList()));

                                var spacing = GetLastLine(child).Text.TakeWhile(char.IsWhiteSpace).Count();

                                var linesContent = copyNode.CodeElement.SourceText.Split(new string[] {"\r\n"}, System.StringSplitOptions.None).Select(l => l.Trim());

                                foreach (string line in linesContent)
                                {
                                    lines.Add(new CobolTextLine(new TextLineSnapshot(-1, new string(' ', spacing) + line, null), ColumnsLayout.CobolReferenceFormat));
                                }

                                
                                generatedCopyDirectives.Add(copy);
                            }
                        }
                    }
                        
                }
                else
                {
                    if (child is DataDescription data)
                    {
                        var joinedLines = string.Join(" ", data.Lines.Select(l => l.Text));
                        var spacing = joinedLines.TakeWhile(char.IsWhiteSpace).Count();
                        var codeElementLineContents = joinedLines.Split('.').Select(l => l.Trim()).Where(l => !l.StartsWith("COPY", StringComparison.InvariantCultureIgnoreCase) && l != string.Empty);

                        if (codeElementLineContents.Any())
                        {
                            if (codeElementLineContents.Count() == 1)
                            {
                                lines.Add(new CobolTextLine(new TextLineSnapshot(data.CodeElement.Line, new string(' ', spacing) + codeElementLineContents.First() + '.', null), ColumnsLayout.CobolReferenceFormat));
                            }
                            else
                            {
                                if (oneLinerMultiInstructionStack.Any() == false)
                                {
                                    oneLinerMultiInstructionStack.AddRange(codeElementLineContents);
                                    oneLinerMultiInstructionStack.Add(new string(' ', spacing));
                                }
                                lines.Add(new CobolTextLine(new TextLineSnapshot(data.CodeElement.Line, oneLinerMultiInstructionStack.Last() + oneLinerMultiInstructionStack.First() + '.', null), ColumnsLayout.CobolReferenceFormat));
                                oneLinerMultiInstructionStack.RemoveAt(0);
                            }
                            lines.AddRange(GenerateLinesForChildren(child.Children, oneLinerMultiInstructionStack));
                        }
                    }


                    else
                    {
                        lines.AddRange(child.Lines);
                        lines.AddRange(GenerateLinesForChildren(child.Children));
                    }
                    
                }
            }

            return lines;
        }

        private ITextLine GetLastLine(Node parent)
        {
            var notCopyChild = parent.Children.FirstOrDefault(c => !c.IsInsideCopy());
                
            if (notCopyChild == null)
            {
                if (parent.Parent != null)
                {
                    return GetLastLine(parent.Parent);
                }
            }
            else
            {
                return notCopyChild.Lines.First();
            }

            return null;
        }

        public void GenerateLineMapFile(Stream stream)
        {            
        }

        public List<Diagnostic> Diagnostics { get; }
        public string TypeCobolVersion { get; set; }

        public bool HasLineMapData => false;

        /// <summary>
        /// Capture for a Token within a copy, its Line and if it is a '.' token
        /// </summary>
        /// <param name="lastLine">ref variable to store the last line</param>
        /// <param name="periodSeparatorSeen">ref variable to store if the PeriodSeparator token has been seen.</param>
        /// <param name="t">The current token</param>
        /// <returns>The current token</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static Token CaptureEndCopyLine(ref int lastLine, ref bool periodSeparatorSeen, Token t)
        {
            lastLine = t.Line - 1;
            periodSeparatorSeen = periodSeparatorSeen || t.TokenType == TokenType.PeriodSeparator;
            return t;
        }

        /// <summary>
        /// Yield all token from a CopyDirective
        /// </summary>
        /// <param name="copyDirective">The CopyDirective instance to yield all tokens.</param>
        /// <returns>The Token Enumerable instance</returns>
        private IEnumerable<Token> CopyTokensYielder(CopyDirective copyDirective)
        {
            if (this.CompilationUnit != null)
            {
                int lastLine = -1;
                bool periodSeparatorSeen = false;
                //Starting from the COPY Line and the COPY Token capture all to token till to encounter a PeriodSeparator.
                var tl = this.CompilationUnit.TokensLines[copyDirective.COPYToken.Line - 1];
                bool copyTokenSeen = false;
                foreach (var t in tl.SourceTokens)
                {
                    if (t == copyDirective.COPYToken)
                        copyTokenSeen = true;
                    if (copyTokenSeen)
                        yield return CaptureEndCopyLine(ref lastLine, ref periodSeparatorSeen, t);
                }

                if (lastLine >= 0 && !periodSeparatorSeen)
                {
                    lastLine++;
                    int count = this.CompilationUnit.TokensLines.Count;
                    for (int l = lastLine; l < count && !periodSeparatorSeen; l++)
                    {
                        var tl2 = this.CompilationUnit.TokensLines[l];
                        foreach (var t in tl2.SourceTokens)
                        {
                            if (periodSeparatorSeen)
                                yield break;
                            else
                                yield return CaptureEndCopyLine(ref lastLine, ref periodSeparatorSeen, t);
                        }
                    }
                }
            }
        }
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
