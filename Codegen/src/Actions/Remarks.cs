using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Emit;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Actions
{
    public class Remarks : EventArgs, Action
    {
        public string Group { get; private set; }
        internal Node Source;
        internal Node Destination;
        internal string DestinationURI;
        public CompilationDocument CompilationDocument { get; set; }



        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="source">The source Node to be expanded</param>
        /// <param name="destination">The destination node of the new expanded node, the new new node will added in
        /// Destination's parent node at the right index.</param>
        /// <param name="destinationURI">The dotted path of the destination, that will be used to calculate the
        /// Destination parent's node index to which to insert the new expanded node as child.</param>
        public Remarks(Node source, Node destination, string destinationURI, CompilationDocument compilationDocument)
        {
            Source = source;
            Destination = destination;
            DestinationURI = destinationURI;
            CompilationDocument = compilationDocument;
        }

        public IList<Action> Execute()
        {
#if EUROINFO_RULES
            if (!CompilationDocument.CompilerOptions.UseEuroInformationLegacyReplacingSyntax || CompilationDocument.CompilerOptions.IsCobolLanguage)
            {
                /*
                 * The original REMARKS directive (if any) is parsed only when UseEuroInformationLegacyReplacingSyntax is active
                 * and only for TypeCobol. So here we use the negation: if the legacy syntax is disabled or if we are in pure cobol mode,
                 * we do not attempt to generate the REMARKS directive.
                 *
                 * Note 1: to auto-generate the directive, we need to parse the one found in source (to know which lines are replaced by
                 *         our own auto REMARKS).
                 * Note 2: if the source contains a REMARKS directive and we don't parse it, it is treated as comments and returned
                 *         without change in generated code.
                 */
                return null;
            }

            if (!((Program) Source).IsMainProgram)
                return null; //We don't have to care about nested/stacked programs. It prevents from generating REMARKS directive multiple times

            //Get tokensLine
            var tokensLines = (CompilationDocument.CobolTextLines as IReadOnlyList<Compiler.Scanner.TokensLine>);

            //Get all the CopyTextNameVariations
            var copys = CompilationDocument.CopyTextNamesVariations;
            if (copys == null || copys.Count == 0)
                return null;

            //Get remarks lines
            var remarksLines = (tokensLines as IList<TypeCobol.Compiler.Parser.CodeElementsLine>).Where(l => l.ScanState.InsideRemarksDirective).ToList(); 

            var generatedCodeElement = new GeneratedCodeElement(remarksLines.SelectMany(t => t.SourceTokens, (a, b) => b).ToList());
            var fakeRemarksNode = new FakeRemars(generatedCodeElement, copys);

            fakeRemarksNode.NeedGeneration = true;

            if (remarksLines.Count == 0)
            {
                fakeRemarksNode.SetFlag(Node.Flag.FactoryGeneratedNodeKeepInsertionIndex, true);
                fakeRemarksNode.SetFlag(Node.Flag.FactoryGeneratedNode, true);

                Source.Add(fakeRemarksNode, 0);
            }
            else
                Source.Add(fakeRemarksNode);
#endif
            return null;
        }

        internal class FakeRemars : FakeGeneratedNode
        {
            public List<RemarksDirective.TextNameVariation> Copys { get; set; }
            public FakeRemars(CodeElement CodeElement, List<RemarksDirective.TextNameVariation> copys) : base(CodeElement)
            {
                Copys = copys;
            }

            public override IEnumerable<ITextLine> Lines
            {
                get
                {
                    var lines = new List<ITextLine>();
                    lines.Add(new TextLineSnapshot(-1, "", null)); //Add first blank line because CodeGen doesn't handle generating comment line.
                    lines.Add(new TextLineSnapshot(-1, "*REMARKS. COPY=(", null));
                    foreach (var copy in Copys)
                    {
                        lines.Add(new TextLineSnapshot(-1, "*        "+copy.TextNameWithSuffix, null));
                    }
                    lines.Add(new TextLineSnapshot(-1, "*        ).", null));

                    return lines;
                }
            }
        }
    }
}
