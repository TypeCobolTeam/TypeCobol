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

        public override List<CompletionItem> ComputeProposals(CompilationUnit compilationUnit, CodeElement codeElement)
        {
            IEnumerable<FunctionDeclaration> procedures = null;
            IEnumerable<DataDefinition> variables;
            var completionItems = new List<CompletionItem>();
            var node = GetMatchingNode(compilationUnit, codeElement);
            if (node == null)
                return completionItems;

            if (node.SymbolTable != null)
            {
                procedures = node.SymbolTable.GetFunctions(StartsWithUserFilter, SymbolTable.Scope.Intrinsic);
                variables = node.SymbolTable.GetVariables(da => StartsWithUserFilter(da) && da.Picture != null && da.DataType == DataType.Alphanumeric, SymbolTable.Scope.Program);
            }
            else
            {
                variables = [];
            }

            completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForProcedures(procedures, node, _functionDeclarationSignatureDictionary));

            foreach (var variable in variables)
            {
                var completionItem = new CompletionItem
                {
                    label = variable.Name,
                    insertText = variable.Name,
                    kind = CompletionItemKind.Variable
                };
                completionItems.Add(completionItem);
            }

            return completionItems;
        }
    }
}
