using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForVariable : CompletionContext
    {
        private readonly Predicate<DataDefinition> _dataDefinitionFilter;

        public CompletionForVariable(Token userFilterToken, Predicate<DataDefinition> additionalDataDefinitionFilter)
            : base(userFilterToken)
        {
            _dataDefinitionFilter = additionalDataDefinitionFilter;
        }

        public override List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var completionItems = new List<CompletionItem>();
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node == null)
                return completionItems;

            var variables = node.SymbolTable
                .GetVariables(d => StartsWithUserFilter(d) && _dataDefinitionFilter(d), SymbolTable.Scope.Program)
                .Select(v => new KeyValuePair<DataDefinitionPath, DataDefinition>(null, v));
            completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, compilationUnit.CompilerOptions));

            return completionItems;
        }
    }
}
