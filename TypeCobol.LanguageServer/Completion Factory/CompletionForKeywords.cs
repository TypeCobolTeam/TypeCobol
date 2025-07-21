using System.Diagnostics;
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
        /// Keys of the dictionary are keyword strings, each associated with an array of variations
        /// of this keyword when it may be followed by other keywords. If the keyword is meant to
        /// be used alone, the variation array is null.
        /// </summary>
        private static Dictionary<string, string[]> _KeywordSuggestions;

        static CompletionForKeywords()
        {
            _KeywordSuggestions = new Dictionary<string, string[]>();
            TokenType[] statementStartingKeywords = new[]
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

            foreach (var keywordType in statementStartingKeywords)
            {
                string keyword = TokenUtils.GetTokenStringFromTokenType(keywordType);
                Debug.Assert(keyword != null);

                string[] variants = keywordType switch
                {
                    TokenType.EXIT => [$"{keyword} ", $"{keyword} PARAGRAPH ", $"{keyword} PERFORM ", $"{keyword} PROGRAM ", $"{keyword} SECTION "],
                    TokenType.JSON => [$"{keyword} GENERATE ", $"{keyword} PARSE "],
                    TokenType.XML => [$"{keyword} GENERATE ", $"{keyword} PARSE "],
                    // Add a trailing space so it will be included in insertText, except for END-xxx keywords
                    _ when keyword.StartsWith("END-") => [keyword],
                    _ => [$"{keyword} "]
                };

                _KeywordSuggestions.Add(keyword, variants);
            }
        }

        public CompletionForKeywords(Token userFilterToken)
            : base(userFilterToken)
        {

        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            // Group suggestions: first group for keywords starting with user text, last group for keywords containing
            // user text preceded by '-'. Second group is actually made of END-xxx keywords.

            List<CompletionItem> suggestionsStartingWithUserText = [];
            List<CompletionItem> suggestionsContainingUserText = [];
            foreach (var keywordSuggestion in _KeywordSuggestions)
            {
                string keyword = keywordSuggestion.Key;
                string[] suggestions = keywordSuggestion.Value;
                if (keyword.StartsWith(UserFilterText, StringComparison.OrdinalIgnoreCase))
                {
                    AddSuggestionsTo(suggestionsStartingWithUserText);
                }
                else if (keyword.Contains($"-{UserFilterText}", StringComparison.OrdinalIgnoreCase))
                {
                    AddSuggestionsTo(suggestionsContainingUserText);
                }
                // else not a suitable keyword for given user text

                void AddSuggestionsTo(List<CompletionItem> completionItems) =>
                    completionItems.AddRange(suggestions.Select(suggestion => new CompletionItem() { label = suggestion, kind = CompletionItemKind.Keyword }));
            }

            yield return suggestionsStartingWithUserText;
            yield return suggestionsContainingUserText;
        }
    }
}
