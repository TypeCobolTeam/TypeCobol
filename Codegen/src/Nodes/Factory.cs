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
		};
	private Type GetGeneratedNode(string name) {
		Type type;
		GeneratedNodeTypes.TryGetValue(name, out type);
		return type;
	}

	internal Compiler.Nodes.Node Create(string name) {
		var type = GetGeneratedNode(name);
		if (type == null) return null; // cannot generate
		return (Compiler.Nodes.Node)Activator.CreateInstance(type);
	}
}

}
