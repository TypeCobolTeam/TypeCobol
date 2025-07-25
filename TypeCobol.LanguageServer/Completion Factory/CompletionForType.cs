using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForType : CompletionContext
    {
        public CompletionForType(Token userFilterToken)
            : base(userFilterToken)
        {

        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node?.SymbolTable == null)
                return [];

            var types = node.SymbolTable.GetTypes(StartsWithUserFilter, SymbolTable.Scope.Intrinsic);
            return [ CompletionFactoryHelpers.CreateCompletionItemsForType(types, node) ];
        }
    }
}
