namespace TypeCobol.Codegen.Nodes {

	using System;
	using System.Collections.Generic;

class Factory {

	private Dictionary<string,Type> GeneratedNodeTypes = new Dictionary<string,Type> {
			{ "data-division", typeof(DataDivision) },
			{ "working-storage", typeof(WorkingStorageSection) },
			{ "local-storage", typeof(LocalStorageSection) },
			{ "linkage", typeof(LinkageSection) },
			{ "file", typeof(FileSection) },
			{ "sentence", typeof(Sentence) },
			{ "end", typeof(SentenceEnd) },
		};
	private Dictionary<string,string[]> GeneratedChildren = new Dictionary<string,string[]> {
			{ "sentence", new string[] { "end", } },
		};

	private void GeneralizeName(ref string name) {
		if (name.StartsWith("sentence-")) name = "sentence";
	}
	private Type GetGeneratedNode(string name) {
		Type type;
		GeneratedNodeTypes.TryGetValue(name, out type);
		return type;
	}
	private string[] GetGeneratedChildren(string name) {
		string[] names;
		GeneratedChildren.TryGetValue(name, out names);
		if (names == null) names = new string[0];
		return names;
	}

	internal Compiler.Nodes.Node Create(string name) {
		GeneralizeName(ref name);
		var type = GetGeneratedNode(name);
		if (type == null) return null; // cannot generate
		var node = (Compiler.Nodes.Node)Activator.CreateInstance(type);
		foreach(var cname in GetGeneratedChildren(name)) {
			var child = Create(cname);
			if (child != null) node.Add(child);
		}
		return node;
	}
}

}
