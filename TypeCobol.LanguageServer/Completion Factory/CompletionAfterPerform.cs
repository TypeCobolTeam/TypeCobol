using System.Diagnostics;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionAfterPerform : CompletionContext
    {
        public CompletionAfterPerform(Token userFilterToken)
            : base(userFilterToken)
        {

        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var performNode = GetMatchingNode(compilationUnit, codeElement);
            IEnumerable<Paragraph> paragraphs = null;
            IEnumerable<Section> sections = null;
            IEnumerable<DataDefinition> variables = null;

            if (performNode?.SymbolTable != null)
            {
                paragraphs = performNode.SymbolTable.GetParagraphs(MatchesWithUserFilter);
                sections = performNode.SymbolTable.GetSections(MatchesWithUserFilter);
                variables = performNode.SymbolTable.GetVariables(da => MatchesWithUserFilter(da) && da.Picture != null && da.DataType == DataType.Numeric, SymbolTable.Scope.Program);
            }

            if (paragraphs != null)
            {
                yield return paragraphs.Select(para => new CompletionItem() { label = para.Name, kind = CompletionItemKind.Reference });
            }

            if (sections != null)
            {
                yield return sections.Select(s => new CompletionItem() { label = s.Name, kind = CompletionItemKind.Reference });
            }

            if (variables != null)
            {
                yield return variables.Select(ToCompletionItem);
            }

            static CompletionItem ToCompletionItem(DataDefinition numericVariable)
            {
                Debug.Assert(numericVariable.Picture != null);
                return new CompletionItem
                {
                    label = $"{numericVariable.Name} PIC {numericVariable.Picture.NormalizedValue}",
                    insertText = numericVariable.Name,
                    kind = CompletionItemKind.Variable
                };
            }
        }
    }
}
