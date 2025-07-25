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
        private readonly bool _addSubscript;

        public CompletionForVariable(Token userFilterToken, Predicate<DataDefinition> additionalDataDefinitionFilter, bool addSubscript = true)
            : base(userFilterToken)
        {
            _dataDefinitionFilter = additionalDataDefinitionFilter;
            _addSubscript = addSubscript;
        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node == null)
                return [];

            var variables = node.SymbolTable
                .GetVariables(d => MatchesWithUserFilter(d) && _dataDefinitionFilter(d), SymbolTable.Scope.Program)
                .Select(v => new KeyValuePair<DataDefinitionPath, DataDefinition>(null, v));

            return [ CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, _addSubscript, compilationUnit.CompilerOptions) ];
        }
    }
}
