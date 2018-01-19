namespace TypeCobol.Codegen.Nodes {

	using System;
	using System.Collections.Generic;

class Factory {
	/** These are the nodes we are able to generate */
	private Dictionary<string,Type> GeneratedNodeTypes = new Dictionary<string,Type> {
			{ "data-division", typeof(DataDivision) },
			{ "working-storage", typeof(WorkingStorageSection) },
			{ "local-storage", typeof(LocalStorageSection) },
			{ "linkage", typeof(LinkageSection) },
			{ "file", typeof(FileSection) },
			{ "sentence", typeof(Sentence) },
			{ "end", typeof(SentenceEnd) },
		};
	/** Some nodes want to be created with children */
	private Dictionary<string,string[]> GeneratedChildren = new Dictionary<string,string[]> {
			{ "sentence", new string[] { "end", } },
		};
	/** Some nodes want to be created next to some others */
	private Dictionary<string,string> Preceeds = new Dictionary<string,string> {
			{ "data-division", "procedure-division" },
            { "file", "working-storage" },
            { "working-storage", "local-storage" },
            { "local-storage", "linkage" },
            { "declaratives-header", "sentence" },
        };

    private Dictionary<string, List<string> > Previous = new Dictionary<string, List<string>>{
			{ "procedure-division", new List<string>(){"data-division" }},
            { "working-storage", new List<string>(){"file"} },
            { "local-storage", new List<string>(){"working-storage", "file"}  },
            { "linkage", new List<string>(){"local-storage", "working-storage", "file"} },
            { "sentence", new List<string>(){ "declaratives-header" } },
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
	private string GetNextSibling(string name) {
		string sibling;
		Preceeds.TryGetValue(name, out sibling);
		return sibling;
	}
    private List<string> GetPreviousSibling(string name)
    {
        List<string> previous;
        Previous.TryGetValue(name, out previous);
        return previous;
    }
    internal Compiler.Nodes.Node Create(string name, out string nextsibling, out List<string> previousSibling)
    {
		GeneralizeName(ref name);
		nextsibling = GetNextSibling(name);
        previousSibling = GetPreviousSibling(name);
		var type = GetGeneratedNode(name);
		if (type == null) return null; // cannot generate
		var node = (Compiler.Nodes.Node)Activator.CreateInstance(type);
		foreach(var cname in GetGeneratedChildren(name)) {
			string whatever;
            List<string> whatever2;
            var child = Create(cname, out whatever, out whatever2);
			if (child != null) node.Add(child);
		}
		return node;
	}
}

}
