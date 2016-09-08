namespace TypeCobol.Compiler.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Functions;
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
		} catch (KeyNotFoundException ex) { return null; }
	}

	private static Dictionary<string,Attribute> attributes;
	static Attributes() {
		attributes = new Dictionary<string,Attribute>();
		attributes["name"]  = new NameAttribute();
		attributes["level"] = new LevelAttribute();
		attributes["type"]  = new TypeAttribute();
		attributes["sender"] = new SenderAttribute();
		attributes["receiver"] = new ReceiverAttribute();
		attributes["function"] = new FunctionUserAttribute();
		attributes["definitions"] = new DefinitionsAttribute();
	}
}



public interface Attribute {
	object GetValue(object o, SymbolTable table);
}

internal class NameAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var named = o as Named;
		if (named == null && o is Node)
			named = ((Node)o).CodeElement as Named;
		if (named == null) return null;
		return named.Name;
	}
}

internal class TypeAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		try { bool.Parse(o.ToString()); return "BOOL"; }
		catch(System.FormatException) { } // not a boolean
		var node = (DataDescription)o;
		var data = (DataDescriptionEntry)node.CodeElement;
		return /*data.Picture!=null? data.Picture.Value :*/ data.CustomType!=null? data.CustomType.Value : null;
	}
}

internal class LevelAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var data = o as DataDefinition;
		if (data == null) return null;
		return string.Format("{0:00}", ((DataDefinitionEntry)data.CodeElement).LevelNumber.Value);
	}
}

internal class SenderAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var statement = ((Node)o).CodeElement as Sending;
		if (statement == null) return null;
		var items = statement.SendingItems;
		if (items.Count == 0) return null;
		if (items.Count == 1) return statement.SendingItems[0];
		throw new System.ArgumentOutOfRangeException("Too many sending items ("+items.Count+")");
	}
}
internal class ReceiverAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var statement = ((Node)o).CodeElement as VariableWriter;
		if (statement == null) return null;
		if (statement.VariablesWritten.Count == 0) return null;
		if (statement.VariablesWritten.Count == 1) return new List<TypeCobol.Compiler.CodeElements.Expressions.QualifiedName>(statement.VariablesWritten.Keys)[0];
		throw new System.ArgumentOutOfRangeException("Too many receiving items ("+statement.VariablesWritten.Count+")");
	}
}

internal class FunctionUserAttribute: Attribute {
	public object GetValue(object o, SymbolTable table) {
		var statement = ((Node)o).CodeElement as FunctionCaller;
		if (statement == null) return null;
		var functions = new List<FunctionCall>();
		foreach(var fun in statement.FunctionCalls) {
			var found = table.GetFunction(fun.QualifiedName);
			if (found.Count < 1) continue;
			if (found.Count > 1) throw new System.ArgumentException("Resolve ambiguity for "+found.Count+" items");
			var declaration = (FunctionDeclaration)found[0];
			functions.Add(Create(fun, declaration));
		}
		if (functions.Count == 0) return null;
		if (functions.Count == 1) return functions[0];
		return functions;
	}

	private static FunctionCall Create(FunctionCall call, FunctionDeclaration declaration) {
		var result = new FunctionCall(call.QualifiedName, declaration.Library, declaration.Copy);
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
	private static CallParameter Create(ParameterDescription pAsDefined, CallParameter pAsUsed) {
		if (pAsUsed != null) return pAsUsed;
		return new EmptyCallParameter();
	}
	private static ParameterDescription GetParameter(int index, FunctionDeclaration function) {
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
		if (index < function.InputParameters.Count) return function.InputParameters[index];
		return null;
	}
}

internal class DefinitionsAttribute: Attribute {
	private long ct;
	private long cf;
	public object GetValue(object o, SymbolTable table) {
		ct = cf = 0;
		var definitions = new Definitions();
		definitions.types = GetTypes(table);
		definitions.functions = GetFunctions(table);
		return definitions;
	}
	private List<Named> GetTypes(SymbolTable table) {
		var list = new List<Named>();
		if (table == null) return list;
		foreach(var items in table.Types)
			foreach(var item in items.Value)
				list.Add(new Definitions.LabelledName(item, "\'T"+(++ct).ToString("0000000")+'\''));
		list.AddRange(GetTypes(table.EnclosingScope));
		return list;
	}
	private List<Named> GetFunctions(SymbolTable table) {
		var list = new List<Named>();
		if (table == null) return list;
		foreach(var items in table.Functions)
			foreach(var item in items.Value)
				list.Add(new Definitions.LabelledName(item, "\'F"+(++cf).ToString("0000000")+'\''));
		list.AddRange(GetFunctions(table.EnclosingScope));
		return list;
	}
}
public class Definitions {
	public List<Named> types;
	public List<Named> functions;

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

	public class LabelledName: Named {
		private Named item;
		public string label;
		public LabelledName(Named item, string label) { this.item = item; this.label = label; }
		public string Name { get { return item.Name; } }
		public CodeElements.Expressions.QualifiedName QualifiedName { get { return item.QualifiedName; } }
	}
}


}
