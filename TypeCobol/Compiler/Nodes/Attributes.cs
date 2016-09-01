namespace TypeCobol.Compiler.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeModel;



public static class Attributes {
	internal static object Get(Node node, string attribute) {
		var table = node.SymbolTable;
		object value = node;
		try {
			foreach(var attr in attribute.Split('.'))
				value = attributes[attr].GetValue(value, table);
			return value;
		} catch (KeyNotFoundException ex) { return null; }
	}

	private static Dictionary<string,Attribute> attributes;
	static Attributes() {
		attributes = new Dictionary<string,Attribute>();
		attributes["name"]  = new NameAttribute();
		attributes["level"] = new LevelAttribute();
		attributes["type"]  = new TypeAttribute();
//		Attributes["typedef"] = new TypeDefined();
		attributes["sender"] = new SenderAttribute();
		attributes["receiver"] = new ReceiverAttribute();
//		Attributes["functions"] = new UsesFunctions();
//		Attributes["function"] = new UsesFunctions(true);
//		Attributes["function-name"] = new UsesFunctions(true, true);
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
		return data.Picture!=null? data.Picture.Value : data.CustomType!=null? data.CustomType.Value : null;
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
		if (statement.SendingItems.Count == 0) return null;
		if (statement.SendingItems.Count == 1) return statement.SendingItems[0];
		throw new System.ArgumentOutOfRangeException("Too many sending items ("+statement.SendingItems.Count+")");
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


}
