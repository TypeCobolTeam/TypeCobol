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
        /// of this keyword when it may be followed by other keywords.
        /// </summary>
        private static readonly Dictionary<string, string[]> _StartingKeywordSuggestions;

        /// <summary>
        /// Hard-coded statement-ending keywords.
        /// </summary>
        private static readonly List<string> _EndingKeywordSuggestions;

        static CompletionForKeywords()
        {
            _StartingKeywordSuggestions = new Dictionary<string, string[]>();
            _EndingKeywordSuggestions = new List<string>();
            TokenType[] statementKeywords = new[]
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

            foreach (var keywordType in statementKeywords)
            {
                string keyword = TokenUtils.GetTokenStringFromTokenType(keywordType);
                Debug.Assert(keyword != null);

                if (keyword.StartsWith("END-"))
                {
                    // Add token string as is, no space after an END-xxx keyword
                    _EndingKeywordSuggestions.Add(keyword);
                }
                else
                {
                    // Create variants
                    string[] variants = keywordType switch
                    {
                        TokenType.EXIT => [$"{keyword} ", $"{keyword} PARAGRAPH ", $"{keyword} PERFORM ", $"{keyword} PROGRAM ", $"{keyword} SECTION "],
                        TokenType.JSON => [$"{keyword} GENERATE ", $"{keyword} PARSE "],
                        TokenType.XML => [$"{keyword} GENERATE ", $"{keyword} PARSE "],
                        // Add a trailing space so it will be included in insertText
                        _ => [$"{keyword} "]
                    };

                    _StartingKeywordSuggestions.Add(keyword, variants);
                }
            }
        }

        public CompletionForKeywords(Token userFilterToken)
            : base(userFilterToken)
        {

        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            // Group suggestions: first group is for starting keywords and second is for ending keywords.

            List<CompletionItem> filteredStartingKeywordSuggestions = [];
            foreach (var startingKeywordSuggestion in _StartingKeywordSuggestions)
            {
                // Using the regex filter here: only keywords starting with the user filter will match for now.
                // In the event a new keyword containing a dash is introduced it may also match even without starting
                // with the user filter but containing '-<UserFilter>'.
                if (MatchesWithUserFilter(startingKeywordSuggestion.Key))
                {
                    filteredStartingKeywordSuggestions.AddRange(startingKeywordSuggestion.Value.Select(ToCompletionItem));
                }
            }

            yield return filteredStartingKeywordSuggestions;

            List<CompletionItem> filteredEndingKeywordSuggestions = [];
            foreach (var endingKeywordSuggestion in _EndingKeywordSuggestions)
            {
                // Using the regex filter: when the user starts typing an opening keyword, we will match the associated
                // ending keyword (if any) here.
                if (MatchesWithUserFilter(endingKeywordSuggestion))
                {
                    filteredEndingKeywordSuggestions.Add(ToCompletionItem(endingKeywordSuggestion));
                }
            }

            yield return filteredEndingKeywordSuggestions;

            static CompletionItem ToCompletionItem(string suggestion) => new CompletionItem() { label = suggestion, kind = CompletionItemKind.Keyword };
        }
    }
}
