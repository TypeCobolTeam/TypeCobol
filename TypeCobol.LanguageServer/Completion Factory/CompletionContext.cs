using System.Text.RegularExpressions;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal abstract class CompletionContext
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

        protected bool StartsWithUserFilter(FunctionDeclaration symbol)
        {
            string symbolName = symbol?.Name;
            string userFilterText = UserFilterText;
            return !string.IsNullOrEmpty(symbolName)
                   &&
                   (symbolName.StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase) || symbol.VisualQualifiedName.ToString().StartsWith(userFilterText, StringComparison.OrdinalIgnoreCase));
        }

        protected bool MatchesWithUserFilter(Node symbol)
        {
            string symbolName = symbol?.Name;
            if (string.IsNullOrEmpty(symbolName))
            {
                return false;
            }

            string userFilterText = UserFilterText;
            if (userFilterText.Length == 0)
            {
                return true;
            }

            // Starts with userFilterText or contains -userFilterText or contains _userFilterText
            return Regex.IsMatch(symbolName, $"(^{userFilterText})|((-|_){userFilterText})", RegexOptions.IgnoreCase);
        }

        public List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var result = new List<CompletionItem>();
            var proposalGroups = ComputeProposalGroups(compilationUnit, codeElement);

            // Flatten proposal groups
            foreach (var proposalGroup in proposalGroups)
            {
                foreach (var completionItem in proposalGroup.OrderBy(item => item.label)) // Order proposals alphabetically in each group
                {
                    if (UserFilterToken != null)
                    {
                        //Add the range object to let the client know the position of the user filter token
                        var range = VsCodeProtocol.Range.FromPositions(UserFilterToken.Line - 1, UserFilterToken.StartIndex, UserFilterToken.Line - 1, UserFilterToken.StopIndex + 1);

                        //-1 on line to 0 based / +1 on stop index to include the last character
                        if (completionItem.data is object[] array)
                            array[0] = range;
                        else
                            completionItem.data = range;
                    }

                    result.Add(completionItem);
                }
            }

            return result;
        }

        /// <summary>
        /// Compute proposals grouped by logical categories. Using groups allows to order proposals over two dimensions:
        /// - First: order of computed groups is preserved allowing the implementors to prioritize certain kinds over some other
        ///   (i.e. after PERFORM, paragraphs are more often used than sections or numeric variables)
        /// - order of proposals within each group: we use alphabetical order to help user find data quickly
        /// </summary>
        /// <param name="compilationUnit">The current compilation unit being edited.</param>
        /// <param name="codeElement">The code element on which completion has been requested.</param>
        /// <returns>Non-null, potentially deferred, of groups of proposals.</returns>
        protected abstract IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement);
    }
}
