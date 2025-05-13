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

        public override List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var callNode = GetMatchingNode(compilationUnit, codeElement);
            if (callNode?.SymbolTable == null)
            {
                return new List<CompletionItem>();
            }

            IEnumerable<Program> programs = callNode.SymbolTable.GetPrograms(StartsWithUserFilter);
            return programs.Select(prog => new CompletionItem() { label = prog.Name, kind = CompletionItemKind.Module }).ToList();
        }
    }
}
