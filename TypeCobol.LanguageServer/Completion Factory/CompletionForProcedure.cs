using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    internal class CompletionForProcedure : CompletionContext
    {
        private readonly Dictionary<SignatureInformation, FunctionDeclaration> _functionDeclarationSignatureDictionary;

        public CompletionForProcedure(Token userFilterToken, Dictionary<SignatureInformation, FunctionDeclaration> functionDeclarationSignatureDictionary)
            : base(userFilterToken)
        {
            _functionDeclarationSignatureDictionary = functionDeclarationSignatureDictionary;
        }

        protected override IEnumerable<IEnumerable<CompletionItem>> ComputeProposalGroups(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node?.SymbolTable == null)
                yield break;

            var procedures = node.SymbolTable.GetFunctions(StartsWithUserFilter, SymbolTable.Scope.Intrinsic);
#if EUROINFO_RULES
            // No dynamic CALL => do not propose variables
            var variables = Enumerable.Empty<DataDefinition>();
#else
            var variables = node.SymbolTable.GetVariables(da => MatchesWithUserFilter(da) && da.Picture != null && da.DataType == DataType.Alphanumeric, SymbolTable.Scope.Program);
#endif

            yield return CompletionFactoryHelpers.CreateCompletionItemsForProcedures(procedures, node, _functionDeclarationSignatureDictionary);
            yield return variables.Select(ToCompletionItem);

            static CompletionItem ToCompletionItem(DataDefinition variable)
            {
                return new CompletionItem
                {
                    label = variable.Name,
                    insertText = variable.Name,
                    kind = CompletionItemKind.Variable
                };
            }
        }
    }
}
