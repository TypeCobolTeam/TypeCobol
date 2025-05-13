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

        public override List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node?.SymbolTable == null)
                return new List<CompletionItem>();

            var types = node.SymbolTable.GetTypes(StartsWithUserFilter, SymbolTable.Scope.Intrinsic);
            return CompletionFactoryHelpers.CreateCompletionItemsForType(types, node);
        }
    }
}
