﻿namespace TypeCobol.Compiler.CodeModel {

	using Antlr4.Runtime;
	using System;
	using System.Collections.Generic;
	using TypeCobol.Compiler.Nodes;
	using TypeCobol.Compiler.CodeElements;



	public class SyntaxTree {
		/// <summary>Tree root</summary>
		public Root Root { get; private set; }
		/// <summary>Branch currently in construction</summary>
		private Stack<Tuple<Node,ParserRuleContext>> Branch;

		public SyntaxTree(): this(new Root(), null) { }
		public SyntaxTree(Root root, ParserRuleContext context) {
			this.Root = root ?? new Root();
			Branch = new Stack<Tuple<Node,ParserRuleContext>>();
			Branch.Push(new Tuple<Node,ParserRuleContext>(Root, context));
		}
		/// <returns>Tip of Branch.</returns>
		public Node CurrentNode { get { return Branch.Peek().Item1; } }
		public ParserRuleContext CurrentContext { get { return Branch.Peek().Item2; } }

		/// <summary>Add a node as the Head's first child.</summary>
		/// <param name="child">Node to add</param>
		/// <param name="context">Context child was created from</param>
		public void Enter(Node child, ParserRuleContext context) {
			CurrentNode.Add(child);
			Branch.Push(new Tuple<Node,ParserRuleContext>(child, context));
		}
		/// <summary>Head's parent becomes the new Head.</summary>
		/// <returns>Exited node</returns>
		public void Exit() {
			Branch.Pop();// will throw InvalidOperationException if Branch is empty
		}
		/// <summary>Delete Head.</summary>
		public void Delete() {
			CurrentNode.Remove();
			Branch.Pop();
		}



		public override string ToString() {
			var str = new System.Text.StringBuilder();
			ToString(Root, str, 0);
			return str.ToString();
		}
		private void ToString(Node node, System.Text.StringBuilder str, int indent) {
			for(int c=1; c<indent; c++) str.Append("  ");
			str.Append("+ ").Append(node.ToString());
			str.AppendLine();
			foreach(var child in node.Children) ToString(child, str, indent+1);
		}

		internal string BranchToString() {
			var str = new System.Text.StringBuilder();
			var reversed = new List<Node>();
			foreach(var node in Branch) reversed.Insert(0, node.Item1);
			foreach(var node in reversed) str.Append(node.ToString()).Append(" > ");
			str.Length -= 2;
			return str.ToString();
		}
	}
}
