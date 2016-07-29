
namespace TypeCobol.Compiler.Nodes {

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;



/// <summary>
/// Tree node, including:
/// - parent/children relations
/// - unique identification accross the tree.
/// </summary>
public abstract class Node {
	private readonly IList<Node> children_ = new List<Node>();
	/// <summary>Children Nodes</summary>
	public IList<Node> Children {
		get { return children_; }
		private set { throw new System.InvalidOperationException(); }
	}
	/// <summary>Parent node.</summary>
	public Node Parent { get; internal set; }
	/// <summary>First Node with null Parent among the parents of this Node.</summary>
	public Node Root {
		get {
			var current = this;
			while (current.Parent != null) current = current.Parent;
			return current;
		}
	}
	/// <summary>Add or Insert a Node as a child of this Node.</summary>
	/// <param name="child">Node to be added to Children.</param>
	/// <param name="index"></param>
	public void Add(Node child, int index = -1) {
		if (index < 0) children_.Add(child);
		else children_.Insert(index, child);
		child.Parent = this;
	}
	/// <summary>Delete this node from Parent.Children, and set Parent to null.</summary>
	public void Remove() {
		Parent.children_.Remove(this);
		Parent = null;
	}

	/// <summary>Node unique identifier (scope: the tree this Node belongs to)</summary>
	public abstract string URI { get; }
	/// <summary>Get this node or one of its children that has a given URI.</summary>
	/// <param name="uri">Node unique identifier to search for</param>
	/// <returns>Node n for which n.URI == uri, or null if no such Node was found</returns>
	public abstract Node Get(string uri);
}



/// <summary>A Node which holds a CodeElement as its data.</summary>
/// <typeparam name="T">Type of CodeElement, so data can be accessed in a strongly-typed way.</typeparam>
public abstract class CodeElementNode<T>: Node where T:CodeElement {
	/// <summary>Node strongly typed data</summary>
	public T CodeElement { get; internal set; }
	public CodeElementNode(T CodeElement) { this.CodeElement = CodeElement; }

	/// <summary>Non-unique identifier of this node, according CodeElement type and name if applicable.</summary>
	public virtual string ID {
		get { return null; }
	}

	public override string URI {
		get {
			if (ID == null) return null;
			string puri = Parent == null?null:Parent.URI;
			if (puri == null) return ID;
		return puri+'.'+ID;
		}
	}

	public override Node Get(string uri) {
		if (URI != null && URI.EndsWith(uri)) return this;
		foreach(var child in Children) {
			var found = child.Get(uri);
			if (found != null) return found;
		}
		return null;
	}
}



/// <summary>Root of any Node tree, with null CodeElement.</summary>
public class Root: CodeElementNode<CodeElement> {
	public Root(): base(null) { }
}

public class Program: CodeElementNode<ProgramIdentification> {
	public Program(ProgramIdentification identification): base(identification) { }
	public override string ID { get { return CodeElement.ProgramName.Name; } }
}

public class Class: CodeElementNode<ClassIdentification> {
	public Class(ClassIdentification identification): base(identification) { }
	public override string ID { get { return CodeElement.ClassName.Name; } }
}

public class Factory: CodeElementNode<FactoryIdentification> {
	public Factory(FactoryIdentification identification): base(identification) { }
	public override string ID { get { return "TODO#248"; } }
}

public class Object: CodeElementNode<ObjectIdentification> {
	public Object(ObjectIdentification identification): base(identification) { }
	public override string ID { get { return "TODO#248"; } }
}



} // end of namespace TypeCobol.Compiler.Nodes
