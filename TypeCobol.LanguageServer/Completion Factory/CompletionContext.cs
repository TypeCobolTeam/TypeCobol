using System.Text.RegularExpressions;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    public abstract class CompletionContext
    {
        public static Node GetMatchingNode(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var codeElementToNode = compilationUnit.ProgramClassDocumentSnapshot?.NodeCodeElementLinkers;
            if (codeElementToNode != null && codeElementToNode.TryGetValue(codeElement, out var node))
            {
                return node;
            }

            return null;
        }

        public Token UserFilterToken { get; }

        protected CompletionContext(Token userFilterToken)
        {
            UserFilterToken = userFilterToken;
        }

        public string UserFilterText => UserFilterToken?.Text ?? string.Empty;

        protected bool StartsWithUserFilter(Node symbol)
        {
            string symbolName = symbol?.Name;
            return !string.IsNullOrEmpty(symbolName) && symbolName.StartsWith(UserFilterText, StringComparison.OrdinalIgnoreCase);
        }

        protected bool MatchesWithUserFilter(Node symbol)
        {
            string userFilterText = UserFilterText;
            if (userFilterText.Length == 0)
            {
                return true;
            }

            string symbolName = symbol?.Name;
            return !string.IsNullOrEmpty(symbolName) && Regex.IsMatch(symbolName, $"(^{userFilterText})|((-|_){userFilterText})", RegexOptions.IgnoreCase);
        }

        public abstract List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement);
    }
}
