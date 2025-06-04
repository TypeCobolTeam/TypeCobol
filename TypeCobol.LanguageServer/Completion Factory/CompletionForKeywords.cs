using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForKeywords : CompletionContext
    {
        /// <summary>
        /// Hard-coded statement-starting keywords. The list is adapted for EI context
        /// as some statements are discouraged in EI environment.
        /// </summary>
        private static TokenType[] _StatementStartingKeywords = new[]
        {
            TokenType.ACCEPT,
            TokenType.ADD,
#if !EUROINFO_RULES
            TokenType.ALLOCATE,
            TokenType.ALTER,
#endif
            TokenType.CALL,
#if !EUROINFO_RULES
            TokenType.CANCEL,
#endif
            TokenType.CLOSE,
            TokenType.COMPUTE,
            TokenType.CONTINUE,
            TokenType.DELETE,
            TokenType.DISPLAY,
            TokenType.DIVIDE,
            TokenType.ELSE,
            TokenType.END_ADD,
            TokenType.END_CALL,
            TokenType.END_COMPUTE,
            TokenType.END_DELETE,
            TokenType.END_DIVIDE,
            TokenType.END_EVALUATE,
            TokenType.END_IF,
#if !EUROINFO_RULES
            TokenType.END_INVOKE,
#endif
            TokenType.END_JSON,
            TokenType.END_MULTIPLY,
            TokenType.END_PERFORM,
            TokenType.END_READ,
            TokenType.END_RETURN,
            TokenType.END_REWRITE,
            TokenType.END_SEARCH,
            TokenType.END_START,
            TokenType.END_STRING,
            TokenType.END_SUBTRACT,
            TokenType.END_UNSTRING,
            TokenType.END_WRITE,
            TokenType.END_XML,
#if !EUROINFO_RULES
            TokenType.ENTRY,
#endif
            TokenType.EVALUATE,
            TokenType.EXIT,
#if !EUROINFO_RULES
            TokenType.FREE,
#endif
            TokenType.GOBACK,
#if !EUROINFO_RULES
            TokenType.GO,
#endif
            TokenType.IF,
            TokenType.INITIALIZE,
            TokenType.INSPECT,
#if !EUROINFO_RULES
            TokenType.INVOKE,
#endif
            TokenType.JSON,
#if !EUROINFO_RULES
            TokenType.MERGE,
#endif
            TokenType.MOVE,
            TokenType.MULTIPLY,
            TokenType.OPEN,
            TokenType.PERFORM,
            TokenType.READ,
#if !EUROINFO_RULES
            TokenType.RELEASE,
#endif
            TokenType.RETURN,
            TokenType.REWRITE,
            TokenType.SEARCH,
            TokenType.SET,
            TokenType.SORT,
            TokenType.START,
#if !EUROINFO_RULES
            TokenType.STOP,
#endif
            TokenType.STRING,
            TokenType.SUBTRACT,
            TokenType.UNSTRING,
            TokenType.WHEN,
            TokenType.WRITE,
            TokenType.XML
        };

        public CompletionForKeywords(Token userFilterToken)
            : base(userFilterToken)
        {

        }

        public override List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            return _StatementStartingKeywords
                .Select(TokenUtils.GetTokenStringFromTokenType)
                .Where(t => t.StartsWith(UserFilterText, StringComparison.OrdinalIgnoreCase))
                .Select(t => new CompletionItem() { label = t, kind = CompletionItemKind.Keyword })
                .ToList();
        }
    }
}
