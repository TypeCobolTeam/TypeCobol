namespace TypeCobol.Compiler.Diagnostics {

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

    class TypeDefinitionEntryChecker: CodeElementListener {

        public void OnCodeElement(CodeElement e, ParserRuleContext c) {
            var context = c as CodeElementsParser.DataDescriptionEntryContext;
            CheckRedefines(e as DataRedefinesEntry, context);
            CheckTypedef(e as DataTypeDescriptionEntry, context);
        }
        private void CheckRedefines(DataRedefinesEntry redefines, CodeElementsParser.DataDescriptionEntryContext context) {
            if (redefines == null) return;
            if (context.cobol2002TypedefClause() != null) {
                string message = "REDEFINES clause cannot be specified with TYPEDEF clause";
                DiagnosticUtils.AddError(redefines, message, context.redefinesClause());
            }
        }
        private void CheckTypedef(DataTypeDescriptionEntry typedef, CodeElementsParser.DataDescriptionEntryContext context) {
            if (typedef == null) return;

            if (typedef.LevelNumber.Value != 1)
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

            }

            if (typedef.RestrictionLevel == RestrictionLevel.STRONG)//Manage as a STRONG TYPEDEF
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
                    DiagnosticUtils.AddError(typedef, message, ParseTreeUtils.GetFirstToken(context.cobol2002TypedefClause().STRONG()), rulestack);
                }
            }
        }
    }

    class TypeDefinitionChecker {

        public static void CheckTypeDefinition(TypeDefinition typeDefinition) {
            AnalyticsWrapper.Telemetry.TrackEvent("[Type-Used] " + typeDefinition.Name);

            if (typeDefinition.CodeElement().Picture == null && typeDefinition.Children.Count < 1) {
                string message = "TYPEDEF \'" + typeDefinition.Name + "\' has no description.";
                DiagnosticUtils.AddError(typeDefinition, message, MessageCode.SemanticTCErrorInParser);
            }
            if (typeDefinition.RestrictionLevel == RestrictionLevel.STRONG) {
                foreach (var sub in typeDefinition.Children) {
                    CheckForValueClause(sub, typeDefinition.QualifiedName);
                }
            }
        }

        private static void CheckForValueClause(Node node, QualifiedName typedef) {
            var codeElement = node.CodeElement as DataDescriptionEntry;
            if (codeElement != null && codeElement.InitialValue != null) {
                string message = "Illegal VALUE clause for subordinate \'"+node.Name+"\' of STRONG TYPEDEF \'"+typedef.Head+"\'";
                DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
            }
            foreach(var sub in node.Children) CheckForValueClause(sub, typedef);
        }
    }

    class RedefinesChecker: NodeListener {

        public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
            var redefinesNode = node as DataRedefines;
            if (redefinesNode == null) {
                return; //not my job
            }
            if (redefinesNode.IsPartOfATypeDef) {
                DiagnosticUtils.AddError(node, "Illegal REDEFINES as part of a TYPEDEF", MessageCode.SemanticTCErrorInParser);
                return;
            }
            var redefinesSymbolReference = redefinesNode.CodeElement().RedefinesDataName;
            var redefinedVariable = node.SymbolTable.GetRedefinedVariable(redefinesNode, redefinesSymbolReference);

            if (redefinedVariable == null)
            {
                string message = "Illegal REDEFINES: Symbol \'" + redefinesSymbolReference + "\' is not referenced";
                DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
                return;
            }

            if (redefinedVariable.IsStronglyTyped || redefinedVariable.IsStrictlyTyped)
            {
                string message = string.Format("Illegal REDEFINES: '{0}' is {1}", redefinesSymbolReference, redefinedVariable.IsStronglyTyped ? "strongly-typed" : "strictly-typed");
                DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
            }
        }
    }

class RenamesChecker {
	public static void OnNode(Node node) {
		var renames = node as DataRenames;
	    if (renames == null) {
	        return; //not my job
	    }
	    Check(renames.CodeElement().RenamesFromDataName, renames);
	    Check(renames.CodeElement().RenamesToDataName, renames);
	}
	private static void Check(SymbolReference renames, Node node) {
		var found = node.SymbolTable.GetVariables(renames);
		if (found.Count > 1) {
			string message = "Illegal RENAMES: Ambiguous reference to symbol \'"+renames+"\'";
			DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
		}
		if (found.Count < 1) {
			string message = "Illegal RENAMES: Symbol \'"+renames+"\' is not referenced";
			DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
		}
		foreach(var v in found) {
                if (v.IsStronglyTyped || v.IsStrictlyTyped)
                {
                    string message = string.Format("Illegal RENAMES: '{0}' is {1}", renames, v.IsStronglyTyped ? "strongly-typed" : "strictly-typed");
                    DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
                }
		}
	}
}

class TypedDeclarationChecker {
	public IList<Type> GetNodes() { return new List<Type>() { typeof(ITypedNode), }; }

	public static void OnNode(Node node) {
        DataDefinition dataDefinition = node as DataDefinition;
	    if (dataDefinition == null || node is TypeDefinition) {
	        return; //not my job
	    }

		var data = dataDefinition.CodeElement as DataDescriptionEntry;
		if (data != null && data.UserDefinedDataType != null && data.Picture != null) {
			string message = "PICTURE clause incompatible with TYPE clause";
			DiagnosticUtils.AddError(node, message, data.Picture.Token);
		}
		var type = dataDefinition.DataType;
        TypeDefinitionHelper.Check(node, type); //Check if the type exists and is not ambiguous
		
	}
}

    public static class TypeDefinitionHelper
    {
        /// <summary>
        /// Generic method to check if a type is referenced or not or if it is ambiguous.
        /// </summary>
        /// <param name="node"></param>
        /// <param name="type"></param>
        public static void Check(Node node, DataType type)
        {
            if (type.CobolLanguageLevel == CobolLanguageLevel.Cobol85) return; //nothing to do, Type exists from Cobol 2002
            var found = node.SymbolTable.GetType(type);
            if (found.Count < 1)
            {
                 string message = "TYPE \'" + type.Name + "\' is not referenced";
                DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
            }
            else if (found.Count > 1)
            {
                string message = "Ambiguous reference to TYPE \'" + type.Name + "\'";
                DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
            }
        }
    }
    /*
    class StronglyTypedReceiverChecker: NodeListener {
        public IList<Type> GetNodes() { return new List<Type> { typeof(VariableWriter) }; }

        public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
            if (node is Initialize || node is Move || node is Release || node is Return
                || node is Read || node is Write || node is Rewrite
                // SET is unspecified, but as a level 88 variable cannot be strongly typed we don't need to check this case
                // + SET myBool TO TRUE (where myBool is of type BOOL) need to works
                || node is Set)
                return;

            var variables = ((VariableWriter)node).VariablesWritten;
            foreach (var v in variables) {
                var names = node.SymbolTable.GetVariables(v.Key);
                foreach (var name in names) {
                    if (name.IsStronglyTyped) {
                        string sending = v.Value.ToString();
                        var enumerable = v.Value as System.Collections.IEnumerable;
                        if (enumerable != null) {
                            var str = new System.Text.StringBuilder();
                            int c = 0;
                            foreach(var item in enumerable) { str.Append(item).Append(','); c++; }
                            if (c > 0) str.Length -= 1;
                            sending = str.ToString();
                        }
                        string message = "Cannot write "+sending+" to strongly typed variable "+name.Name+":"+((Typed)name).DataType.Name;
                        DiagnosticUtils.AddError(node, message, MessageCode.SemanticTCErrorInParser);
                    }
                }
            }
        }
    }
    */
}