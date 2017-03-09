﻿
using System;

namespace TypeCobol.Compiler.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.CodeModel;



public static class Attributes {
	internal static object Get(Node node, string attribute) {
		var table = node.SymbolTable;
		object value = node;
		try {
			foreach(var attr in attribute.Split('.')) {
				if (value == null) break;
				value = attributes[attr].GetValue(value, table);
			}
			return value;
		} catch (KeyNotFoundException) {
			DEFAULT.Key = attribute;
			return DEFAULT.GetValue(value, table);
		}
	}

	private static Dictionary<string,Attribute> attributes;
	static Attributes() {
		attributes = new Dictionary<string,Attribute>();
		attributes["name"]  = new NameAttribute();
		attributes["level"] = new LevelAttribute();
		attributes["type"]  = new TypeAttribute();
		attributes["sender"] = new SenderAttribute();
		attributes["receiver"] = new ReceiverAttribute();
		attributes["unsafe"] = new UnsafeAttribute();
		attributes["function"] = new FunctionUserAttribute();
		attributes["definitions"] = new DefinitionsAttribute();
            //not used?
		attributes["typecobol"] = new TypeCobolAttribute();
		attributes["visibility"] = new VisibilityAttribute();
		attributes["copyname"] = new LibraryCopyAttribute();
		attributes["programName8"] = new ProgramName8Attribute();
	}
	private static ContainerAttribute DEFAULT = new ContainerAttribute();
}

internal class ContainerAttribute: Attribute {
	internal string Key { get; set; }
	public object GetValue(object o, SymbolTable table) {
		return null;
	}
}



public interface Attribute {
	object GetValue(object o, SymbolTable table);
}

internal class NameAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var node = o as Node;
        if (node != null)
        {
            var named = ((Node)o).CodeElement as NamedCodeElement;
            return named.Name;
        }
        else
            return null;
	}
}
internal class TypeAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
        bool result;
        if(bool.TryParse(o.ToString(), out result)) {
            return "BOOL";
        }

		var node = o as DataDescription;
	    if (node != null) {
                var data = node.CodeElement as DataDescriptionEntry;
	        if (data != null) {
                    return /*data.Picture!=null? data.Picture.Value :*/ data.UserDefinedDataType != null ? data.UserDefinedDataType.Name : null;
                }
            }
	    return null;

	}
}

internal class LevelAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var data = o as DataDefinition;
		if (data == null) return null;
		return string.Format("{0:00}", ((DataDefinitionEntry)data.CodeElement).LevelNumber.Value);
	}
}


internal class TypeCobolAttribute: Attribute {
	internal string Key { get; set; }
	public object GetValue(object o, SymbolTable table) {
		var map = o as IDictionary<StorageArea,object>;
		var results = new Dictionary<StorageArea,object>();
		foreach (var kv in map)
			if (kv.Key.SymbolReference is TypeCobolQualifiedSymbolReference)
				results.Add(kv.Key,kv.Value);
		return results;
	}
}

internal class SenderAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var ce = ((Node)o).CodeElement;

		var set = ce as SetStatementForConditions;
		if (set != null) return new URI(set.SendingValue.Value.ToString());

	    if (ce.StorageAreaReads == null || ce.StorageAreaReads.Count == 0) {
	        return null;
	    }
	    if (ce.StorageAreaReads.Count > 1) {
	        throw new System.ArgumentOutOfRangeException("Too many sending items (" + ce.StorageAreaReads.Count + ")");
	    }
	    return ce.StorageAreaReads[0].ToString();
	}
}
internal class ReceiverAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
	    var codeElement = ((Node)o).CodeElement;
	    var variablesWritten = codeElement.StorageAreaWrites;
	    if (variablesWritten == null) return null;
        if (variablesWritten.Count == 0) return null;
        if (variablesWritten.Count == 1) return variablesWritten[0].ToString();
		throw new System.ArgumentOutOfRangeException("Too many receiving items ("+ variablesWritten.Count+")");
	}
}

internal class UnsafeAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var statement = o as VariableWriter;
		if (statement == null) return null;
		return statement.IsUnsafe;
	}
}

internal class FunctionUserAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var statement = ((Node)o).CodeElement as FunctionCaller;
		if (statement == null) return null;
		var functions = new List<FunctionCallInfo>();
		
		var found = table.GetFunction(new URI(statement.FunctionCall.FunctionName));
			
		if (found.Count > 1) throw new System.ArgumentException("Resolve ambiguity for "+found.Count+" items");
		var declaration = found[0];
		functions.Add(Create(statement.FunctionCall, declaration));
		
		if (functions.Count == 0) return null;
		if (functions.Count == 1) return functions[0];
		return functions;
	}

	private static FunctionCallInfo Create(FunctionCall call, FunctionDeclaration declaration) {
		var result = new FunctionCallInfo(new URI(call.FunctionName), declaration.Library, declaration.Copy);
		if (declaration.Profile == null) return result;
		int count = declaration.Profile.InputParameters.Count + declaration.Profile.InoutParameters.Count + declaration.Profile.OutputParameters.Count;
		// declaration.Profile.ReturningParameter is not used because
		// the same data is always used by (and hardcoded in) function call codegen: <function.Name>-RESULT
		for(int i=0; i < count; i++) {
			var pAsDefined = GetParameter(i, declaration);
			var pAsUsed    = GetParameter(i, call);
			result.InputParameters.Add(Create(pAsDefined, pAsUsed));
		}
		return result;
	}
	private static CallParameter Create(ParameterDescriptionEntry pAsDefined, CallParameter pAsUsed) {
		if (pAsUsed != null) return pAsUsed;
		return new EmptyCallParameter();
	}
	private static ParameterDescriptionEntry GetParameter(int index, FunctionDeclaration function) {
		int offset = 0;
		if (index - offset < function.Profile.InputParameters.Count) return function.Profile.InputParameters[index-offset];
		offset += function.Profile.InputParameters.Count;
		if (index - offset < function.Profile.InoutParameters.Count) return function.Profile.InoutParameters[index-offset];
		offset += function.Profile.InoutParameters.Count;
		if (index - offset < function.Profile.OutputParameters.Count) return function.Profile.OutputParameters[index-offset];
		offset += function.Profile.OutputParameters.Count;
		if (index - offset < 1) return function.Profile.ReturningParameter;
		throw new System.ArgumentOutOfRangeException("Expected: "+index+" < "+function.Profile.InputParameters.Count
		                                                                 +'+'+function.Profile.InoutParameters.Count
		                                                                 +'+'+function.Profile.OutputParameters.Count
		                                                                 +'+'+(function.Profile.ReturningParameter!=null?1:0));
	}
	private static CallParameter GetParameter(int index, FunctionCall function) {
		if (function.Arguments != null && index < function.Arguments.Length) return new CallParameter(function.Arguments[index].StorageAreaOrValue);
		return null;
	}
}

    public class FunctionCallInfo
    {
        public FunctionCallInfo(FunctionCallResult call)
        {
            QualifiedName = new URI(call.FunctionCall.FunctionName);
            foreach (var arg in call.FunctionCall.Arguments)
                InputParameters.Add(new CallParameter(arg.StorageAreaOrValue));
        }
        /// <summary>Used for codegen.</summary>
        public FunctionCallInfo(QualifiedName name, string lib, string copy)
        {
            QualifiedName = name;
            Library = lib;
            Copy = copy;
        }

        public string Name { get { return QualifiedName.Head; } }
        public QualifiedName QualifiedName { get; private set; }

        public IList<CallParameter> InputParameters = new List<CallParameter>();

        public string Library { get; set; }
        public string Copy { get; set; }
    }

    public class CallParameter
    {

        private Variable voe;
        public CallParameter(Variable voe) { this.voe = voe; }

        public virtual bool IsLiteral { get { return voe.IsLiteral; } }
        public virtual string SendingMode { get { return IsLiteral ? "CONTENT" : "REFERENCE"; } }
        public virtual string Value { get { return voe.MainSymbolReference != null ? voe.MainSymbolReference.Name : null; } }
    }
    public class EmptyCallParameter : CallParameter
    {
        public EmptyCallParameter() : base(null) { }
        public override bool IsLiteral { get { return true; } }
        public override string SendingMode { get { return "CONTENT"; } }
        public override string Value { get { return "SPACE"; } }
    }

internal class DefinitionsAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var definitions = new Definitions();
		definitions.types = GetTypes(table);
		definitions.functions = GetFunctions(table);
		return definitions;
	}
	private Definitions.NList GetTypes(SymbolTable table) {
		var list = new Definitions.NList();
		if (table == null) return list;
		foreach(var items in table.Types) list.AddRange(items.Value);
		list.AddRange(GetTypes(table.EnclosingScope));
		return list;
	}
	private Definitions.NList GetFunctions(SymbolTable table) {
		var list = new Definitions.NList();
		if (table == null) return list;
		foreach(var items in table.Functions) list.AddRange(items.Value);
		list.AddRange(GetFunctions(table.EnclosingScope));
		return list;
	}
}
public class Definitions {
	public NList types;
	public NList functions;

	public override string ToString() {
		var str = new System.Text.StringBuilder();
		str.Append("Types:[");
		foreach(var item in types) str.Append(item.Name).Append(',');
		if (types.Count > 0) str.Length -= 1;
		str.Append("] Functions:[");
		foreach(var item in functions) str.Append(item.Name).Append(',');
		if (functions.Count > 0) str.Length -= 1;
		str.Append(']');
		return str.ToString();
	}

	public class NList: List<Node> {
		internal NList(): base() { }
		public List<Node> Public  { get { return retrieve(AccessModifier.Public); } }
		public List<Node> Private { get { return retrieve(AccessModifier.Private); } }
		private List<Node> retrieve(AccessModifier visibility) {
			var results = new List<Node>();
			foreach(var node in this) {
				var fun = node as FunctionDeclaration;
				if (fun == null) continue;
				if (fun.CodeElement().Visibility == visibility) results.Add(node);
			}
			return results;
		}
	}
}

internal class VisibilityAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var fun = o as FunctionDeclaration;
		if (fun != null) return fun.CodeElement().Visibility.ToString();
		return null;
	}
}

internal class LibraryCopyAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var pgm = (Program)((Node)o).Root.GetChildren<ProgramIdentification>()[0];
		var copies = pgm.GetChildren<LibraryCopyCodeElement>();
		var copy = copies.Count > 0? ((LibraryCopy)copies[0]) : null;
		return copy == null? "?TCRFUN_LIBRARY_COPY?" : copy.CodeElement().Name.Name;
	}
}
    /// <summary>
    /// return the name of enclosing program of the current node.
    /// The name is limited to 8 characters
    /// </summary>
    internal class ProgramName8Attribute: Attribute {
	    public object GetValue(object o, SymbolTable table) {
            var node = o as Node;
	        while (node != null) {
	            var pgm = node as Program;
	            if (pgm != null) {
	                var name = pgm.CodeElement().ProgramName.Name;
                    return pgm.CodeElement().ProgramName.Name.Substring(0,Math.Min(name.Length, 8));
	            }
	            node = node.Parent;
	        }
	        return "";
	    }
    }


}
