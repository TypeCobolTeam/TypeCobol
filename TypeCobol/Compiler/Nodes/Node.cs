
namespace TypeCobol.Compiler.Nodes {

using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;




public interface Node {
	/// <summary>Parent node (weakly-typed)</summary>
	Node Parent { get; }
	/// <summary>List of children nodes (weakly-typed, read-only)</summary>
	IReadOnlyList<Node> Children { get; }

	/// <summary>Adds a node as a children of this one.</summary>
	/// <typeparam name="T">Class (derived from CodeElement) of the child to-be.</typeparam>
	/// <param name="node">Child to-be.</param>
	/// <param name="index">Index of the list of children at which node must be added.</param>
	void Add<T>(Node<T> node, int index) where T:CodeElement;
	/// <summary>Removes a child from this node.</summary>
	/// <typeparam name="T">Class (derived from CodeElement) of the child node.</typeparam>
	/// <param name="node">Child to remove. If this is not one of this Node's current children, nothing happens.</param>
	void Remove<T>(Node<T> node) where T:CodeElement;
	/// <summary>Removes this node from its Parent's children list and set this.Parent to null.</summary>
	void Remove();

	/// <summary>Unique identifier of this Node in its tree.</summary>
	string URI { get; }
	/// <summary>Retrieves this Node or one of its (in)direct children using a node URI.</summary>
	/// <param name="uri">Node unique identifier.</param>
	/// <returns>The Node n with n.URI.EndsWith(uri), or null if there is no such Node.</returns>
	Node Get(string uri);

	void Dump(System.Text.StringBuilder str, int i);
}

/// <summary>
/// Tree node, including:
/// - strongly-typed CodeElement data
/// - parent/children relations
/// - unique identification accross the tree
/// </summary>
public abstract class Node<T>: Node where T:CodeElement {

	/// <summary>Node strongly typed data</summary>
	public T CodeElement { get; internal set; }
	public Node(T CodeElement) { this.CodeElement = CodeElement; }

	/// <summary>Parent node</summary>
	public Node Parent { get; internal set; }

	protected readonly List<Node> children = new List<Node>();
	/// <summary>List of children. If you want to modify it, use the Add and Remove methods.</summary>
	public IReadOnlyList<Node> Children { get { return children.AsReadOnly(); } }

/*
	public IList<Node<CodeElement>> GetChildren(System.Type type) {
		var results = new List<Node>();
		foreach(var child in GetChildren())
			if (child.CodeElement != null && Tools.Reflection.IsTypeOf(child.CodeElement.GetType(), type))
				results.Add(child);
		return results;
	}
*/


	/// <summary>Add or Insert a Node as a child of this Node.</summary>
	/// <typeparam name="T"></typeparam>
	/// <param name="child">Non-null Node to be added to the children list</param>
	/// <param name="index"></param>
	public void Add<T>(Node<T> child, int index = -1) where T:CodeElement {
		if (index < 0) children.Add(child);
		else children.Insert(index, child);
		child.Parent = this;
	}
	/// <summary>Remove a child Node from this Node.</summary>
	/// <typeparam name="T"></typeparam>
	/// <param name="child">Node to remove ; if it's not a child, nothing happens</param>
	public void Remove<T>(Node<T> child) where T:CodeElement {
		children.Remove(child);
		child.Parent = null;
	}
	/// <summary>Delete this node from Parent's children, and set Parent to null.</summary>
	public void Remove() {
		if (Parent != null) Parent.Remove(this);
	}



	/// <summary>Non-unique identifier of this node. Depends on CodeElement type and name (if applicable).</summary>
	public virtual string ID { get { return null; } }
	/// <summary>Node unique identifier (scope: tree this Node belongs to)</summary>
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
	public Node Get(string uri) {
		if (URI != null && URI.EndsWith(uri)) return this;
		foreach(var child in Children) {
			var found = child.Get(uri);
			if (found != null) return found;
		}
		return null;
	}
	/// <summary>As <see cref="Get"/> method, but can specify the type of Node to retrieve.</summary>
	/// <typeparam name="N"></typeparam>
	/// <param name="uri"></param>
	/// <returns>null if a node with the given URI is found but is not of the proper type</returns>
	public N Get<N>(string uri) where N:Node {
		var node = Get(uri);
		try { return (N)node; }
		catch(System.InvalidCastException) { return default(N); }
	}



	public override string ToString() {
		var str = new System.Text.StringBuilder();
		Dump(str, 0);
		return str.ToString();
	}
	// TODO: what if I DON'T want to put Dump(..) inside the Node interface?
	// Dump(..) should be private !!
	public void Dump(System.Text.StringBuilder str, int i) {
		for (int c=0; c<i; c++) str.Append("  ");
		if (CodeElement == null) str.AppendLine("?");
		else str.AppendLine(CodeElement.ToString());
		foreach(var child in Children) child.Dump(str, i+1);
	}





	/// <summary>First Node with null Parent among the parents of this Node.</summary>
	public Node Root {
		get {
			Node current = this;
			while (current.Parent != null) current = current.Parent;
			return current;
		}
	}
	/// <summary>
	/// How far removed from Root is this Node?
	/// Values are 0 if Root is this, 1 of Root is this.Parent,
	///	2 if Root is this.Parent.Parent, and so on.
	/// </summary>
	public int Generation {
		get {
			int generation = 0;
			var parent = this.Parent;
			while(parent != null) {
				generation++;
				parent = parent.Parent;
			}
			return generation;
		}
	}



	public CodeModel.SymbolTable SymbolTable { get; set; }

	public object this[string attribute] { get { return null; } }

	/// <summary>TODO: Codegen should do its stuff without pollutiong this class.</summary>
	public bool? Comment = null;

	/// <summary>Implementation of the GoF Visitor pattern.</summary>
	public void Accept(NodeVisitor visitor) {
		visitor.Visit(this);
	}
}



/// <summary>Implementation of the GoF Visitor pattern.</summary>
public interface NodeVisitor {
	void Visit<T>(Node<T> node) where T:CodeElement;
}





/// <summary>A <see cref="Node"/> who can type its parent more strongly should inherit from this.</summary>
/// <typeparam name="C">Class (derived from <see cref="Node{T}"/>) of the parent node.</typeparam>
public interface Child<P> where P:Node { }
/// <summary>Extension method to get a more strongly-typed parent than just Node.</summary>
public static class ChildExtension {
	/// <summary>Returns this node's parent in as strongly-typed.</summary>
	/// <typeparam name="P">Class (derived from <see cref="Node{T}"/>) of the parent.</typeparam>
	/// <param name="child">We want this <see cref="Node"/>'s parent.</param>
	/// <returns>This <see cref="Node"/>'s parent, but strongly-typed.</returns>
	public static P StrongParent<P>(this Child<P> child) where P:Node {
		var node = child as Node;
		if (node == null) throw new System.ArgumentException("Child must be a Node.");
		return (P)node.Parent;
    }
}

/// <summary>A <see cref="Node"/> who can type its children more strongly should inherit from this.</summary>
/// <typeparam name="C">Class (derived from <see cref="Node{T}"/>) of the children nodes.</typeparam>
public interface Parent<C> where C:Node { }
/// <summary>Extension method to get children more strongly-typed than just Node.</summary>
public static class ParentExtension {
	/// <summary>
	/// Returns a read-only list of strongly-typed children of a <see cref="Node"/>.
	/// The children are more strongly-typed than the ones in the Node.Children property.
	/// The list is read-only because the returned list is a copy of the Node.Children list property ;
	/// thus, writing node.StrongChildren().Add(child) will be a compilation error.
	/// Strongly-typed children are to be iterated on, but to modify a Node's children list you have
	/// to use the (weakly-typed) Node.Children property.
	/// </summary>
	/// <typeparam name="C">Class (derived from <see cref="Node{T}"/>) of the children.</typeparam>
	/// <param name="parent">We want this <see cref="Node"/>'s children.</param>
	/// <returns>Strongly-typed list of a <see cref="Node"/>'s children.</returns>
	public static IReadOnlyList<C> StrongChildren<C>(this Parent<C> parent) where C:Node {
		var node = parent as Node;
		if (node == null) throw new System.ArgumentException("Parent must be a Node.");
		//TODO? maybe use ConvertAll or Cast from LINQ, but only
		// if the performance is better or if it avoids a copy.
		var result = new List<C>();
		foreach(var child in node.Children) result.Add((C)child);
        return result.AsReadOnly();
    }
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
