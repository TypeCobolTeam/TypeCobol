using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    public static class CompletionFactoryHelpers
    {
        public static IEnumerable<CompletionItem> CreateCompletionItemsForType(List<TypeDefinition> types, Node node, bool enablePublicFlag = true)
        {
            var completionItems = new List<CompletionItem>();

            foreach (var type in types)
            {
                bool typeIsPublic = false;
                if (enablePublicFlag)
                    typeIsPublic = (type.CodeElement as DataTypeDescriptionEntry).Visibility ==
                                   Compiler.CodeElements.AccessModifier.Public
                                   &&
                                   !(node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations)
                                         .Types.Values.Any(t => t.Contains(type))
                                     //Ignore public if type is in the current program
                                     || type.IsFlagSet(Node.Flag.NodeIsIntrinsic)); //Ignore public if type is in intrinsic

                var typeDisplayName = typeIsPublic ? type.QualifiedName.ToString() : type.Name;
                var completionItem = new CompletionItem(typeDisplayName);
                completionItem.insertText = typeIsPublic
                    ? string.Format("{0}::{1}", type.QualifiedName.Tail, type.QualifiedName.Head)
                    : type.Name;
                completionItem.kind = CompletionItemKind.Class;
                completionItems.Add(completionItem);
            }
            return completionItems;
        }

        public static IEnumerable<CompletionItem> CreateCompletionItemsForProcedures(List<FunctionDeclaration> procedures, Node node, bool enablePublicFlag = true)
        {
            var completionItems = new List<CompletionItem>();

            foreach (var proc in procedures)
            {
                string inputParams = null, outputParams = null, inoutParams = null;

                if (proc.Profile != null)
                {
                    if (proc.Profile.InputParameters != null && proc.Profile.InputParameters.Count > 0)
                        inputParams = string.Format("INPUT: {0}",
                            string.Join(", ",
                                proc.Profile.InputParameters.Select(
                                    p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                    if (proc.Profile.OutputParameters != null && proc.Profile.OutputParameters.Count > 0)
                        outputParams = string.Format("| OUTPUT: {0}",
                            string.Join(", ",
                                proc.Profile.OutputParameters.Select(
                                    p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                    if (proc.Profile.InoutParameters != null && proc.Profile.InoutParameters.Count > 0)
                        inoutParams = string.Format("| INOUT: {0}",
                            string.Join(", ",
                                proc.Profile.InoutParameters.Select(
                                    p => string.Format("{0}({1})", p.DataName, p.DataType.Name))));
                }
                bool procIsPublic = false;
                if (enablePublicFlag)
                    procIsPublic = (proc.CodeElement as FunctionDeclarationHeader).Visibility ==
                                   Compiler.CodeElements.AccessModifier.Public
                                   &&
                                   !(node.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations)
                                         .Functions.Values.Any(t => t.Contains(proc))
                                     //Ignore public if proc is in the current program
                                     || proc.IsFlagSet(Node.Flag.NodeIsIntrinsic)); //Ignore public if proc is in intrinsic;
                var procDisplayName = procIsPublic ? proc.QualifiedName.ToString() : proc.Name;
                var completionItem =
                    new CompletionItem(string.Format("{0} ({1} {2} {3})", procDisplayName, inputParams, outputParams,
                        inoutParams));
                completionItem.insertText = procIsPublic
                    ? string.Format("{0}::{1}", proc.QualifiedName.Tail, proc.QualifiedName.Head)
                    : proc.Name;
                completionItem.kind = proc.Profile.IsFunction ? CompletionItemKind.Function : CompletionItemKind.Method;
                completionItems.Add(completionItem);
            }

            return completionItems;
        }

        public static IEnumerable<CompletionItem> CreateCompletionItemsForVariables(IEnumerable<DataDefinition> variables, bool useQualifiedName = true)
        {
            var completionItems = new List<CompletionItem>();

            foreach (var variable in variables)
            {
                completionItems.Add(CreateCompletionItemForVariable(variable, useQualifiedName));
            }

            return completionItems;
        }

        public static CompletionItem CreateCompletionItemForVariable(DataDefinition variable, bool useQualifiedName = true)
        {
            var variableArrangedQualifiedName = useQualifiedName
                ? string.Join("::",
                    variable.QualifiedName.ToString()
                        .Split(variable.QualifiedName.Separator)
                        .Skip(variable.QualifiedName.Count > 1 ? 1 : 0)) //Skip Program Name
                : variable.Name;

            var variableDisplay = string.Format("{0} ({1}) ({2})", variable.Name, variable.DataType.Name, variableArrangedQualifiedName);
            return new CompletionItem(variableDisplay) { insertText = variableArrangedQualifiedName, kind = CompletionItemKind.Variable };
        }

        public static CompletionItem CreateCompletionItemForIndex(Compiler.CodeElements.SymbolInformation index, DataDefinition variable)
        {
            var display = string.Format("{0} (from {1})", index.Name, variable.Name);
            return new CompletionItem(display) {insertText = index.Name, kind = CompletionItemKind.Variable};
        }
    }
}
