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

        public override List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var performNode = GetMatchingNode(compilationUnit, codeElement);
            IEnumerable<Paragraph> paragraphs = null;
            IEnumerable<Section> sections = null;
            IEnumerable<DataDefinition> variables = null;
            var completionItems = new List<CompletionItem>();

            if (performNode?.SymbolTable != null)
            {
                paragraphs = performNode.SymbolTable.GetParagraphs(MatchesWithUserFilter);
                sections = performNode.SymbolTable.GetSections(MatchesWithUserFilter);
                variables = performNode.SymbolTable.GetVariables(da => MatchesWithUserFilter(da) && da.Picture != null && da.DataType == DataType.Numeric, SymbolTable.Scope.Program);
            }

            if (paragraphs != null)
            {
                completionItems.AddRange(paragraphs.Select(para => new CompletionItem() { label = para.Name, kind = CompletionItemKind.Reference }));
            }

            if (sections != null)
            {
                completionItems.AddRange(sections.Select(s => new CompletionItem() { label = s.Name, kind = CompletionItemKind.Reference }));
            }

            if (variables != null)
            {
                foreach (var variable in variables)
                {
                    var completionItem = new CompletionItem
                    {
                        label = $"{variable.Name} PIC{variable.Picture.NormalizedValue}",
                        insertText = variable.Name,
                        kind = CompletionItemKind.Variable
                    };
                    completionItems.Add(completionItem);
                }
            }

            return completionItems;
        }
    }
}
