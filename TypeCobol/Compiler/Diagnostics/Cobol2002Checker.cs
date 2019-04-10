using System.Linq;
using System;
using System.Collections.Generic;
using Antlr4.Runtime;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Nodes;
using Analytics;
using Castle.Core.Internal;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Diagnostics
{
    class TypeDefinitionEntryChecker
    {
        public static void CheckRedefines(DataRedefinesEntry redefines, CodeElementsParser.DataDescriptionEntryContext context)
        {
            if (context.cobol2002TypedefClause() != null)
            {
                string message = "REDEFINES clause cannot be specified with TYPEDEF clause";
                DiagnosticUtils.AddError(redefines, message, context.redefinesClause());
            }
        }

        public static void CheckTypedef(DataTypeDescriptionEntry typedef, CodeElementsParser.DataDescriptionEntryContext context)
        {
            if (typedef.LevelNumber?.Value != 1)
            {
                string message = "TYPEDEF clause can only be specified for level 01 entries";
                DiagnosticUtils.AddError(typedef, message, context.cobol2002TypedefClause());
            }

            if (typedef.IsExternal)
            {
                string message = "EXTERNAL clause cannot be specified with TYPEDEF clause";
                foreach (var external in context.externalClause())
                    DiagnosticUtils.AddError(typedef, message, external);
            }

#if EUROINFO_LEGACY_TYPEDEF
            if (typedef.RestrictionLevel != RestrictionLevel.STRICT)
            {
                string message = "Custom EI rule : Only TYPEDEF STRICT is allowed.";
                DiagnosticUtils.AddError(typedef, message, context.cobol2002TypedefClause());
                return;
            }
#endif

            if (typedef.RestrictionLevel == RestrictionLevel.STRICT) //Manage as a STRICT TYPEDEF
            {
                if (typedef.IsSynchronized != null && typedef.IsSynchronized.Value == true)
                {
                    DiagnosticUtils.AddError(typedef, "SYNC clause cannot be used with a STRICT type definition", context.cobol2002TypedefClause());
                }
            }

            if (typedef.RestrictionLevel == RestrictionLevel.STRONG) //Manage as a STRONG TYPEDEF
            {
                if (typedef.InitialValue != null)
                {
                    string message = "STRONG TYPEDEF cannot contain VALUE clause:";
                    foreach (var valeuClause in context.valueClause())
                        DiagnosticUtils.AddError(typedef, message, valeuClause);
                }

                if (typedef.Picture != null)
                {
                    string message = "Elementary TYPEDEF cannot be STRONG";
                    string rulestack = RuleStackBuilder.GetRuleStack(context.cobol2002TypedefClause());
                    DiagnosticUtils.AddError(typedef, message,
                        ParseTreeUtils.GetFirstToken(context.cobol2002TypedefClause().STRONG()), rulestack);
                }
            }
        }
    }

    class TypeDefinitionChecker
    {
        public static void CheckTypeDefinition(TypeDefinition typeDefinition)
        {
            if (typeDefinition.SymbolTable.GetType(new URI(typeDefinition.DataType.Name)).Any(t => t != typeDefinition))
            {
                var message = string.Format("TYPE '{0}' has already been declared", typeDefinition.DataType.Name);
                DiagnosticUtils.AddError(typeDefinition, message, MessageCode.SemanticTCErrorInParser);
            }

            if (typeDefinition.CodeElement.Picture == null && typeDefinition.Children.Count < 1 &&
                !typeDefinition.Usage.HasValue)
            {
                string message = "TYPEDEF \'" + typeDefinition.Name + "\' has no description.";
                DiagnosticUtils.AddError(typeDefinition, message, MessageCode.SemanticTCErrorInParser);
            }
            if (typeDefinition.RestrictionLevel == RestrictionLevel.STRONG)
            {
                foreach (var sub in typeDefinition.Children)
                {
                    CheckForValueClause(sub, typeDefinition.Name);
                }
            }
            // Add a warning if a parameters field is set inside the formalized comment
            if (typeDefinition.CodeElement.FormalizedCommentDocumentation != null &&
                !typeDefinition.CodeElement.FormalizedCommentDocumentation.Parameters.IsNullOrEmpty())
            {
                var token = typeDefinition.CodeElement.ConsumedTokens
                    .FirstOrDefault(t => t.TokenType == TokenType.FORMALIZED_COMMENTS_PARAMETERS);
                if (token != null)
                {
                    DiagnosticUtils.AddError(typeDefinition.CodeElement,
                        "Type Definition does not support Parameters field",
                        token, code: MessageCode.Warning);
                }
            }

            //Check circular reference if not already done by TypeCobolLinker
            TypeCobolLinker.CheckCircularReferences(typeDefinition);
        }

        private static void CheckForValueClause(Node node, string typedefName)
        {
            var codeElement = node.CodeElement as DataDescriptionEntry;
            if (codeElement != null && codeElement.InitialValue != null)
            {
                string message = "Illegal VALUE clause for subordinate \'" + node.Name + "\' of STRONG TYPEDEF \'" +
                                 typedefName + "\'";
                DiagnosticUtils.AddError(node, message, codeElement, code:MessageCode.SemanticTCErrorInParser);
            }
            foreach (var sub in node.Children) CheckForValueClause(sub, typedefName);
        }
    }

    class RedefinesChecker
    {
        public static void OnNode(DataRedefines redefinesNode)
        {
            if (redefinesNode == null)
                return; //not my job

            if (redefinesNode.IsPartOfATypeDef)
            {
                DiagnosticUtils.AddError(redefinesNode, "Illegal REDEFINES as part of a TYPEDEF",
                    MessageCode.SemanticTCErrorInParser);
            }

            var redefinesSymbolReference = redefinesNode.CodeElement.RedefinesDataName;
            var redefinedVariable = redefinesNode.SymbolTable.GetRedefinedVariable(redefinesNode, redefinesSymbolReference);

            if (redefinedVariable == null)
            {
                string message = "Illegal REDEFINES: Symbol \'" + redefinesSymbolReference + "\' is not referenced";
                DiagnosticUtils.AddError(redefinesNode, message, redefinesSymbolReference, code: MessageCode.SemanticTCErrorInParser);
                return;
            }
            redefinedVariable.AddDataRedefinition(redefinesNode);


            if (redefinedVariable.IsStronglyTyped || redefinedVariable.IsStrictlyTyped)
            {
                string message = string.Format("Illegal REDEFINES: '{0}' is {1}", redefinesSymbolReference,
                    redefinedVariable.IsStronglyTyped ? "strongly-typed" : "strictly-typed");
                DiagnosticUtils.AddError(redefinesNode, message, redefinesSymbolReference, code:MessageCode.SemanticTCErrorInParser);
            }
        }
    }

    class RenamesChecker
    {
        public static void OnNode(Node node)
        {
            var renames = node as DataRenames;
            if (renames == null)
            {
                return; //not my job
            }
            if (renames.CodeElement?.RenamesFromDataName != null)
                Check(renames.CodeElement.RenamesFromDataName, renames);
            if(renames.CodeElement?.RenamesToDataName != null)
                Check(renames.CodeElement.RenamesToDataName, renames);
        }

        private static void Check(SymbolReference renames, Node node)
        {
            var founds = node.SymbolTable.GetVariables(renames);
            if (founds.Count() > 1)
            {
                string message = "Illegal RENAMES: Ambiguous reference to symbol \'" + renames + "\'";
                DiagnosticUtils.AddError(node, message, renames, code: MessageCode.SemanticTCErrorInParser);
                return;
            }
            if (!founds.Any())
            {
                string message = "Illegal RENAMES: Symbol \'" + renames + "\' is not referenced";
                DiagnosticUtils.AddError(node, message, renames, code: MessageCode.SemanticTCErrorInParser);
                return;
            }

            var found = founds.First();
            var foundCodeElement = found.CodeElement;
           
            if (found.IsStronglyTyped || found.IsStrictlyTyped)
            {
                string message = string.Format("Illegal RENAMES: '{0}' is {1}", renames,
                    found.IsStronglyTyped ? "strongly-typed" : "strictly-typed");
                DiagnosticUtils.AddError(node, message, renames, code: MessageCode.SemanticTCErrorInParser);
            }

            if (foundCodeElement?.LevelNumber != null && foundCodeElement.LevelNumber.Value == 01)
            {
                string message = string.Format("Illegal RENAMES: '{0}' is level 01", renames);
                DiagnosticUtils.AddError(node, message, renames, code: MessageCode.SemanticTCErrorInParser);
            }
            
        }
    }

    class TypedDeclarationChecker
    {

        private static List<Node> browsedTypes = new List<Node>();

        public static void OnNode(Node node)
        {
            DataDefinition dataDefinition = node as DataDefinition;
            if (dataDefinition == null || node is TypeDefinition)
            {
                return; //not my job
            }

            var data = dataDefinition.CodeElement as DataDescriptionEntry;
            if (data != null && data.UserDefinedDataType != null && data.Picture != null)
            {
                string message = "PICTURE clause incompatible with TYPE clause";
                DiagnosticUtils.AddError(node, message, data.Picture.Token);
            }

            var type = dataDefinition.DataType;
            TypeDefinition foundedType = dataDefinition.TypeDefinition;

            if (foundedType == null || data == null || data.LevelNumber == null)
                return;

            if (data.LevelNumber.Value == 88 || data.LevelNumber.Value == 66)
            {
                DiagnosticUtils.AddError(node,
                    string.Format("A {0} level variable cannot be typed", data.LevelNumber.Value),
                    data, code: MessageCode.SemanticTCErrorInParser);
            }

            if (data.LevelNumber.Value == 77 && foundedType.Children.Count > 0)
            {
                DiagnosticUtils.AddError(node, "A 77 level variable cannot be typed with a type containing children",
                    data, code: MessageCode.SemanticTCErrorInParser);
            }

            if (data.LevelNumber.Value <= 49)
            {
                browsedTypes.Clear(); //Clear list of browsed types before testing a path
                long simulatedTypeLevel = SimulatedTypeDefLevel(data.LevelNumber.Value, foundedType);
                if (simulatedTypeLevel > 49)
                {
                    var message =
                        string.Format(
                            "Variable '{0}' has to be limited to level {1} because of '{2}' maximum estimated children level",
                            data.Name, data.LevelNumber.Value - (simulatedTypeLevel - 49), foundedType.Name);
                    DiagnosticUtils.AddError(node, message, data, code: MessageCode.SemanticTCErrorInParser);
                }
            }

            //Check if initial value equals true/false for boolean TYPEDEF
            if (type == DataType.Boolean && data.InitialValue != null &&
                data.InitialValue.LiteralType != Value.ValueLiteralType.Boolean)
            {
                DiagnosticUtils.AddError(node, "Boolean type requires TRUE/FALSE value clause",
                    MessageCode.SemanticTCErrorInParser);
            }
        }

        private static long SimulatedTypeDefLevel(long startingLevel, DataDefinition node)
        {
            if (node == null)//case where the type of variable is not found
            {
                return startingLevel;
            }

            var maximalLevelReached = startingLevel;
            

            if (node is TypeDefinition)
            {
                if (browsedTypes.Contains(node))
                    return maximalLevelReached; //Stop here because of self referencing type
                else
                    browsedTypes.Add(node);
            }
            foreach (DataDefinition child in node.Children)
            {
                var calculatedLevel = startingLevel;
                if (child.DataType.CobolLanguageLevel > CobolLanguageLevel.Cobol85) //If variable is typed
                {
                    calculatedLevel = SimulatedTypeDefLevel(++calculatedLevel, child.TypeDefinition);
                }
                else if (child.Children.Count > 0) //If variable is not typed, check if there is children
                {
                    calculatedLevel = SimulatedTypeDefLevel(++calculatedLevel, child);
                }
                else //It's a final variable, just add one level
                {
                    calculatedLevel++;
                }

                if (calculatedLevel > maximalLevelReached)
                    maximalLevelReached = calculatedLevel;
            }
            return maximalLevelReached;
        }
    }
    
}