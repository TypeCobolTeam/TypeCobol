
namespace TypeCobol.Compiler.Nodes {

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;



/// <summary>
/// Tree node, including:
/// - parent/children relations
/// - unique identification accross the tree.
/// </summary>
public abstract class Node<T> where T:CodeElement {
	protected readonly IList<Node<CodeElement>> children = new List<Node<CodeElement>>();
	/// <summary>Children Nodes</summary>
	public virtual IList<Node<CodeElement>> GetChildren() { return children; }

	/// <summary>Parent node.</summary>
	public Node<CodeElement> Parent { get; internal set; }
	/// <summary>First Node with null Parent among the parents of this Node.</summary>
	public Node<CodeElement> Root {
		get {
			var current = (Node<CodeElement>)(object)this;
			while (current.Parent != null) current = current.Parent;
			return current;
		}
	}
	/// <summary>Add or Insert a Node as a child of this Node.</summary>
	/// <param name="child">Node to be added to Children.</param>
	/// <param name="index"></param>
	public void Add<T>(Node<T> child, int index = -1) where T:CodeElement {
		var variant = (Node<CodeElement>)(object)child;
		if (index < 0) children.Add(variant);
		else children.Insert(index, variant);
		child.Parent = (Node<CodeElement>)(object)this;
	}
	/// <summary>Delete this node from Parent.Children, and set Parent to null.</summary>
	public void Remove() {
		Parent.children.Remove((Node<CodeElement>)(object)this);
		Parent = null;
	}

	public IList<Node<CodeElement>> GetChildren(System.Type type) {
		var results = new List<Node<CodeElement>>();
		foreach(var child in GetChildren())
			if (child.CodeElement != null && Tools.Reflection.IsTypeOf(child.CodeElement.GetType(), type))
				results.Add(child);
		return results;
	}



	/// <summary>Node strongly typed data</summary>
	public T CodeElement { get; internal set; }
	public Node(T CodeElement) { this.CodeElement = CodeElement; }

	/// <summary>Non-unique identifier of this node, according CodeElement type and name if applicable.</summary>
	public virtual string ID {
		get { return null; }
	}
	/// <summary>Node unique identifier (scope: the tree this Node belongs to)</summary>
	public string URI {
		get {
			if (ID == null) return null;
			string puri = Parent == null?null:Parent.URI;
			if (puri == null) return ID;
		return puri+'.'+ID;
		}
	}
	/// <summary>Get this node or one of its children that has a given URI.</summary>
	/// <param name="uri">Node unique identifier to search for</param>
	/// <returns>Node n for which n.URI == uri, or null if no such Node was found</returns>
	public Node<CodeElement> Get(string uri) {
		if (URI != null && URI.EndsWith(uri)) return (Node<CodeElement>)(object)this;
		foreach(var child in children) {
			var found = child.Get(uri);
			if (found != null) return found;
		}
		return null;
	}

	public CodeModel.SymbolTable SymbolTable { get; set; }
}



/// <summary>Root of any Node tree, with null CodeElement.</summary>
public class Root: Node<CodeElement> {
	public Root(): base(null) { }
}

public class Program: Node<ProgramIdentification> {
	public Program(ProgramIdentification identification): base(identification) { }
	public override string ID { get { return CodeElement.ProgramName.Name; } }
}

public class Class: Node<ClassIdentification> {
	public Class(ClassIdentification identification): base(identification) { }
	public override string ID { get { return CodeElement.ClassName.Name; } }
}

public class Factory: Node<FactoryIdentification> {
	public Factory(FactoryIdentification identification): base(identification) { }
	public override string ID { get { return "TODO#248"; } }
}

public class Method: Node<MethodIdentification> {
	public Method(MethodIdentification identification): base(identification) { }
	public override string ID { get { return CodeElement.MethodName.Name; } }
}

public class Object: Node<ObjectIdentification> {
	public Object(ObjectIdentification identification): base(identification) { }
	public override string ID { get { return "TODO#248"; } }
}

public class End: Node<CodeElementEnd> {
	public End(CodeElementEnd end): base(end) { }
	public override string ID { get { return "end"; } }
}



} // end of namespace TypeCobol.Compiler.Nodes
