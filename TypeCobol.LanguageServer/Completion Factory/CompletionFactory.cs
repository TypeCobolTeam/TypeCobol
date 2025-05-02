using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    public static class CompletionFactory
    {
        #region Variable Completion
        public static List<CompletionItem> GetCompletionForVariable(FileCompiler fileCompiler, CodeElement codeElement, Func<DataDefinition, bool> predicate)
        {
            var completionItems = new List<CompletionItem>();
            var node = CompletionFactoryHelpers.GetMatchingNode(fileCompiler, codeElement);
            if (node == null)
                return completionItems;

            var variables = node.SymbolTable.GetVariables(predicate, SymbolTable.Scope.Program).Select(v => new KeyValuePair<DataDefinitionPath, DataDefinition>(null, v));
            completionItems.AddRange(CompletionFactoryHelpers.CreateCompletionItemsForVariableSetAndDisambiguate(variables, fileCompiler.CompilerOptions));

            return completionItems;
        }
        #endregion
    }
}
