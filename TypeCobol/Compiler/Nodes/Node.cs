
namespace TypeCobol.Compiler.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;
	using TypeCobol.Compiler.CodeElements.Expressions;
	using TypeCobol.Compiler.Text;




/// <summary>
/// Tree node, including:
/// - strongly-typed CodeElement data
/// - parent/children relations
/// - unique identification accross the tree
/// </summary>
public abstract class Node: Named {

	/// <summary>CodeElement data (weakly-typed)</summary>
	public CodeElement CodeElement { get; private set; }
	public Node(CodeElement CodeElement) { this.CodeElement = CodeElement; }

	/// <summary>Parent node (weakly-typed)</summary>
	public Node Parent { get; private set; }
	protected List<Node> children = new List<Node>();
	/// <summary>List of children  (weakly-typed, read-only).</summary>
	///	If you want to modify this list, use the <see cref="Add"/> and <see cref="Remove"/> methods.
	public IReadOnlyList<Node> Children { get { return children.AsReadOnly(); } }

	public IList<CodeElementHolder<T>> GetChildren<T>() where T:CodeElement {
		var results = new List<CodeElementHolder<T>>();
		foreach(var child in children) {
			if (child.CodeElement == null) continue;
			if (Tools.Reflection.IsTypeOf(child.CodeElement.GetType(), typeof(T)))
				results.Add((CodeElementHolder<T>)child);
		}
		return results;
	}


	/// <summary>Adds a node as a children of this one.</summary>
	/// <param name="child">Child to-be.</param>
	/// <param name="index">Child position in children list.</param>
	public void Add(Node child, int index = -1) {
		if (index < 0) children.Add(child);
		else children.Insert(index, child);
		child.Parent = this;
	}
	/// <summary>Removes a child from this node.</summary>
	/// <param name="node">Child to remove. If this is not one of this Node's current children, nothing happens.</param>
	public void Remove(Node child) {
		children.Remove(child);
		child.Parent = null;
	}
	/// <summary>Removes this node from its Parent's children list and set this.Parent to null.</summary>
	public void Remove() {
		if (Parent != null) Parent.Remove(this);
	}
	/// <summary>Position of a specific child among its siblings.</summary>
	/// <param name="child">Child to be searched for.</param>
	/// <returns>Position in the children list.</returns>
	/// <exception cref="System.ArgumentOutOfRangeException">As List</exception>
	public int IndexOf(Node child) {
		return children.IndexOf(child);
	}
	/// <summary>Delete all childrens of this node.</summary>
	public void Clear() {
		foreach(var child in children) child.Parent = null;
		children.Clear();
	}



	public string Name { get { return this.ID; } }
	public QualifiedName QualifiedName { get { return URI!=null? new URI(URI) : null; } }

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
	private void Dump(System.Text.StringBuilder str, int i) {
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

	public object this[string attribute] { get { return Attributes.Get(this, attribute); } }

	/// <summary>TODO: Codegen should do its stuff without pollutiong this class.</summary>
	public bool? Comment = null;
	/// <summary>TODO: Codegen should do its stuff without pollutiong this class.</summary>
	public void RemoveAllChildren() { children.Clear(); }

	public virtual IEnumerable<ITextLine> Lines {
		get {
			var lines = new List<ITextLine>();
			if (CodeElement == null) return lines;
			foreach(var token in CodeElement.ConsumedTokens)
				if (!lines.Contains(token.TokensLine))
					lines.Add(token.TokensLine);
			return lines;
		}
	}

	/// <summary>Implementation of the GoF Visitor pattern.</summary>
	public void Accept(NodeVisitor visitor) {
		visitor.Visit(this);
	}
}



/// <summary>Implementation of the GoF Visitor pattern.</summary>
public interface NodeVisitor {
	void Visit(Node node);
}





public interface CodeElementHolder<T> where T:CodeElement { }
public static class CodeElementHolderExtension {
	/// <summary>CodeElement data (strongly-typed)</summary>
	/// <typeparam name="T">Class (derived from <see cref="CodeElement"/>) of the data.</typeparam>
	/// <param name="holder">We want this <see cref="Node"/>'s data.</param>
	/// <returns>This <see cref="Node"/>'s CodeElement data, but strongly-typed.</returns>
	public static T CodeElement<T>(this CodeElementHolder<T> holder) where T:CodeElement {
		var node = holder as Node;
		if (node == null) throw new System.ArgumentException("CodeElementHolder must be a Node.");
		return (T)node.CodeElement;
    }
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
	public static P Parent<P>(this Child<P> child) where P:Node {
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
	public static IReadOnlyList<C> Children<C>(this Parent<C> parent) where C:Node {
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
public class Root: Node, CodeElementHolder<CodeElement> {
	public Root(): base(null) { }
}

public class Program: Node, CodeElementHolder<ProgramIdentification> {
	public Program(ProgramIdentification identification): base(identification) { }
	public override string ID { get { return this.CodeElement().ProgramName.Name; } }
}

public class Class: Node, CodeElementHolder<ClassIdentification> {
	public Class(ClassIdentification identification): base(identification) { }
	public override string ID { get { return this.CodeElement().ClassName.Name; } }
}

public class Factory: Node, CodeElementHolder<FactoryIdentification> {
	public Factory(FactoryIdentification identification): base(identification) { }
	public override string ID { get { return "TODO#248"; } }
}

public class Method: Node, CodeElementHolder<MethodIdentification> {
	public Method(MethodIdentification identification): base(identification) { }
	public override string ID { get { return this.CodeElement().MethodName.Name; } }
}

public class Object: Node, CodeElementHolder<ObjectIdentification> {
	public Object(ObjectIdentification identification): base(identification) { }
	public override string ID { get { return "TODO#248"; } }
}

public class End: Node, CodeElementHolder<CodeElementEnd> {
	public End(CodeElementEnd end): base(end) { }
	public override string ID { get { return "end"; } }
}



} // end of namespace TypeCobol.Compiler.Nodes
