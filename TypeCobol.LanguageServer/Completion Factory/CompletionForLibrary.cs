using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForLibrary : CompletionContext
    {
        public CompletionForLibrary(Token userFilterToken)
            : base(userFilterToken)
        {

        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var callNode = GetMatchingNode(compilationUnit, codeElement);
            if (callNode?.SymbolTable == null)
            {
                return [];
            }

            IEnumerable<Program> programs = callNode.SymbolTable.GetPrograms(StartsWithUserFilter);
            return [ programs.Select(prog => new CompletionItem() { label = prog.Name, kind = CompletionItemKind.Module }) ];
        }
    }
}
