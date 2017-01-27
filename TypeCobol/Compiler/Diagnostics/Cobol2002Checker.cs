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
		if (typedef.LevelNumber.Value != 1) {
			string message = "TYPEDEF clause can only be specified for level 01 entries";
			DiagnosticUtils.AddError(typedef, message, context.cobol2002TypedefClause());
		}
		if (typedef.Picture != null && typedef.DataType.IsStrong) {
			string message = "Elementary TYPEDEF cannot be STRONG";
			string rulestack = new RuleStackBuilder().GetRuleStack(context.cobol2002TypedefClause());
			DiagnosticUtils.AddError(typedef, message, ParseTreeUtils.GetFirstToken(context.cobol2002TypedefClause().STRONG()), rulestack);
		}
		if (typedef.IsExternal) {
			string message = "EXTERNAL clause cannot be specified with TYPEDEF clause";
			foreach (var external in context.externalClause())
				DiagnosticUtils.AddError(typedef, message, external);
		}
		if (typedef.DataType.IsStrong && typedef.InitialValue != null) {
			string message = "STRONG TYPEDEF cannot contain VALUE clause:";
			foreach (var valeuClause in context.valueClause())
				DiagnosticUtils.AddError(typedef, message, valeuClause);
		}
	}
}
class TypeDefinitionChecker: NodeListener {

	public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
		var typedef = node as TypeDefinition;
	    if (typedef == null) {
	        return;//not my job
	    }
		if (typedef.CodeElement().Picture == null && typedef.Children.Count < 1) {
			string message = "TYPEDEF \'"+typedef.Name+"\' has no description.";
			DiagnosticUtils.AddError(typedef.CodeElement, message, MessageCode.SemanticTCErrorInParser);
		}
		if (typedef.IsStrong) foreach(var sub in typedef.Children) CheckForValueClause(sub, typedef.QualifiedName);
	}
	private void CheckForValueClause(Node node, QualifiedName typedef) {
		var data = node as DataDescription;
		if (data != null && data.CodeElement().InitialValue != null) {
			string message = "Illegal VALUE clause for subordinate \'"+data.Name+"\' of STRONG TYPEDEF \'"+typedef.Head+"\'";
			DiagnosticUtils.AddError(data.CodeElement, message, MessageCode.SemanticTCErrorInParser);
		}
		foreach(var sub in node.Children) CheckForValueClause(sub, typedef);
	}
}
    class RedefinesChecker: NodeListener {

	    private class Error: Dictionary<Error.Status,int> {
		    public enum Status { TYPEStrong, TYPEDEFPart, TYPEDEFStrong, }
		    public Error() { foreach(var value in Enum.GetValues(typeof(Status))) this.Add((Status)value,0); }
		    public override string ToString() {
			    var str = new System.Text.StringBuilder("{");
			    foreach(var v in this)
				    if (v.Value > 0 ) str.Append(' ').Append(v.Key).Append(':').Append(v.Value).Append(',');
			    if (this.Errors > 0) str.Length -= 1;
			    return str.Append(" }").ToString();
		    }
		    public int Errors {
			    get {
				    int count = 0;
				    foreach(var v in this.Values) count += v;
				    return count;
			    }
		    }
		    internal int Validate(List<Node> candidates) { return candidates.Count-Errors; }
	    }

	    public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
	        var redefinesNode = node as DataRedefines;
	        if (redefinesNode == null) {
	            return; //not my job
	        }
		    if (redefinesNode.IsPartOfATypeDef) {
			    DiagnosticUtils.AddError(node.CodeElement, "Illegal REDEFINES as part of a TYPEDEF", MessageCode.SemanticTCErrorInParser);
			    return;
		    }


		    var redefines = new URI(redefinesNode.CodeElement().RedefinesDataName.Name);
		    var errors = new Error();
		    var redefinedVariables  = node.SymbolTable.GetVariable(redefines);
		    foreach(var redefinedVariable in redefinedVariables) {
			    if (redefinedVariable.IsPartOfATypeDef) {
				    errors[Error.Status.TYPEDEFPart]++;
			    }
                if (IsStronglyTyped(redefinedVariable))
                {
                    errors[Error.Status.TYPEStrong]++;
                }
            }
		    int ValidDataRedefinitions = errors.Validate(redefinedVariables);
		    if (ValidDataRedefinitions == 1) return; // at least one data item redefinition OK

		    var types = node.SymbolTable.GetType(redefines);
		    foreach(var v in types) {
			    if (((TypeDefinition)v).IsStrong) {
				    errors[Error.Status.TYPEDEFStrong]++;
			    }
		    }
		    int validTypeRedefinitions = errors.Validate(redefinedVariables);
		    if (validTypeRedefinitions == 1) return; // at least one _weak_ TYPEDEF redefinition OK

		    if (ValidDataRedefinitions > 1) {
			    string message = "Illegal REDEFINES: Ambiguous reference to symbol \'"+redefines.Head+"\'";
			    DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
		    }
		    if (validTypeRedefinitions > 1) {
			    string message = "Illegal REDEFINES: Ambiguous reference to TYPEDEF \'"+redefines.Head+"\'";
			    DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
		    }
			
		    if (errors.Errors > 0) {
			    if (errors[Error.Status.TYPEStrong] > 0) {
				    string message = "Illegal REDEFINES: \'"+redefines.Head+"\' is strongly-typed";
				    DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
			    }
			    if (errors[Error.Status.TYPEDEFPart] > 0) {
				    string message = "Illegal REDEFINES: \'"+redefines.Head+"\' is part of a TYPEDEF";
				    DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
			    }
			    if (errors[Error.Status.TYPEDEFStrong] > 0) {
				    string message = "Illegal REDEFINES: \'"+redefines.Head+"\' is a STRONG TYPEDEF";
				    DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
			    }
		    } else {
			    if (ValidDataRedefinitions+validTypeRedefinitions < 1) {
				    string message = "Illegal REDEFINES: Symbol \'"+redefines.Head+"\' is not referenced";
				    DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
			    }
		    }
	    }
	    internal static bool IsStronglyTyped(Node node) {
		    var typed = node as ITypedNode;
		    if (typed == null) return false;
		    if (typed.DataType.IsStrong) return true;
		    return IsStronglyTyped(node.Parent);
	    }
    }

class RenamesChecker: NodeListener {
	public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
		var renames = node as DataRenames;
	    if (renames == null) {
	        return; //not my job
	    }
		Check(new URI(renames.CodeElement().RenamesFromDataName.Name), node);
		Check(new URI(renames.CodeElement().RenamesToDataName.Name), node);
	}
	private void Check(QualifiedName renames, Node node) {
		var found = node.SymbolTable.GetVariable(renames);
		if (found.Count > 1) {
			string message = "Illegal RENAMES: Ambiguous reference to symbol \'"+renames.Head+"\'";
			DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
		}
		if (found.Count < 1) {
			string message = "Illegal RENAMES: Symbol \'"+renames.Head+"\' is not referenced";
			DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
		}
		foreach(var v in found) {
			if (RedefinesChecker.IsStronglyTyped(v)) {
				string message = "Illegal RENAMES: \'"+renames.Head+"\' is strongly-typed";
				DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
			}
		}
	}
}

class TypedDeclarationChecker: NodeListener {
	public IList<Type> GetNodes() { return new List<Type>() { typeof(ITypedNode), }; }

	public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
	    var typedNode = node as ITypedNode;
        if(typedNode == null || node is TypeDefinition) {
            return; //not my job
        }

		var data = node.CodeElement as DataDescriptionEntry;
		if (data != null && data.UserDefinedDataType != null && data.Picture != null) {
			string message = "PICTURE clause incompatible with TYPE clause";
			DiagnosticUtils.AddError(node.CodeElement, message, data.Picture.Token);
		}
		var type = ((ITypedNode)node).DataType;
		if (type.CobolLanguageLevel == CobolLanguageLevel.Cobol85) return; //nothing to do, Type exists from Cobol 2002
		var found = node.SymbolTable.GetType(new URI(type.Name));
		if (found.Count < 1) {
			string message = "TYPE \'"+type.Name+"\' is not referenced";
			DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
		} else if (found.Count > 1) {
		    string message = "Ambiguous reference to TYPE \'" + type.Name + "\'";
		    DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
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
			var names = node.SymbolTable.GetVariable(v.Key);
			foreach (var name in names) {
				if (RedefinesChecker.IsStronglyTyped(name as Node)) {
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
					DiagnosticUtils.AddError(node.CodeElement, message, MessageCode.SemanticTCErrorInParser);
				}
			}
		}
	}
}
*/
}