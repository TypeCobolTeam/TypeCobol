﻿using System.Collections.Generic;
using TypeCobol.Compiler.CodeModel;

namespace TypeCobol.Compiler.CodeElements
{
	public class Node {
		private IList<Node> children_ = new List<Node>();
		public IList<Node> Children {
			get { return new System.Collections.ObjectModel.ReadOnlyCollection<Node>(children_); }
			private set { throw new System.InvalidOperationException(); }
		}
		public CodeElement CodeElement { get; internal set; }
		public Node Parent { get; internal set; }

		public Node(CodeElement e) { CodeElement = e; }

		internal void Add(Node child) {
			children_.Add(child);
			child.Parent = this;
		}
		internal void Remove() {
			Parent.children_.Remove(this);
			Parent = null;
		}

		/// <summary>Implementation of the GoF Visitor pattern.</summary>
		public void Accept(NodeVisitor visitor) {
			visitor.Visit(this);
		}

		public override string ToString() {
			if (CodeElement==null) return GetType().Name+"?";
			else return CodeElement.GetType().Name;
		}

		public CodeModel.SymbolTable SymbolTable { get; set; }



		private static Dictionary<string,Attribute> Attributes;
		static Node() {
			Attributes = new Dictionary<string,Attribute>();
			Attributes["TYPE"] = new Typed("TYPE");
			Attributes["TYPEDEF"] = new TypeDefinition("TYPEDEF");
		}
		public string this[string attribute] {
			get {
				try { return Attributes[attribute].GetValue(CodeElement, SymbolTable); }
				catch(KeyNotFoundException ex) { return null; }
			}
		}
	}

	/// <summary>Implementation of the GoF Visitor pattern.</summary>
	public interface NodeVisitor {
		void Visit(Node node);
	}

	public interface Attribute {
		string GetValue(CodeElement ce, SymbolTable table);
	}
	internal abstract class NodeAttribute: Attribute {
		public string Key { get; private set; }
		public NodeAttribute(string key) { this.Key = key; }
		public abstract string GetValue(CodeElement ce, SymbolTable table);
	}

	internal class Typed: NodeAttribute {
		public Typed(string key): base(key) { }
		public override string GetValue(CodeElement ce, SymbolTable table) {
			var data = ce as DataDescriptionEntry;
			if (data == null || data.IsTypeDefinition) return null;
			if (!table.IsCustomType(data.DataType)) return null;
			return data.DataType.Name;
		}
	}
	internal class TypeDefinition: NodeAttribute {
		public TypeDefinition(string key): base(key) { }
		public override string GetValue(CodeElement ce, SymbolTable table) {
			var data = ce as DataDescriptionEntry;
			if (data == null || !data.IsTypeDefinitionPart) return null;
			return data.DataType.Name;
		}
	}
}
