﻿using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen {

	[TestClass]
	public class TreeGen {

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void TreeToCode() {
			var root1 = CreateSampleTree();
			root1.Children[0].Children.Insert(2, new Node("X"));
			root1.Children[2].Children.Insert(0, new Node("Y"));
			var writer1 = new TreeToCode();
			root1.Accept(writer1);
			System.Console.WriteLine(writer1.Output);
			//new TreeToCode().execute(root);
			var document = Parser.Parse("resources/input/TypeCobol/FUNCTION.cbl", Compiler.DocumentFormat.RDZReferenceFormat);
			var columns = document.Results.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
			var program = document.Results.ProgramClassDocumentSnapshot.Program;
			var root = program.SyntaxTree.Root;
			root.Get("Functions.data-division.working-storage").Children.Insert(1, new Node("codegen", false));
			var writer = new TreeToCode(document.Results.TokensLines);
			root.Accept(writer);
			System.Console.WriteLine(writer.Output);
		}



		public Compiler.CodeElements.Node CreateSampleTree() {
			//		0
			//     /|\
			//    1 7 8
			//   /|\  |
			//  2 3 4 9
			//     / \
			//    5   6
			var root = new Node("0");
			root.Children.Add(new Node("1"));
			root.Children.Add(new Node("7"));
			root.Children.Add(new Node("8"));
			root.Children[0].Children.Add(new Node("2"));
			root.Children[0].Children.Add(new Node("3"));
			root.Children[0].Children.Add(new Node("4"));
			root.Children[2].Children.Add(new Node("9"));
			root.Children[0].Children[2].Children.Add(new Node("5"));
			root.Children[0].Children[2].Children.Add(new Node("6"));
			return root;
		}
		[TestMethod]
		[TestProperty("Time","fast")]
		public void TestSampleTreeCreation() {
			var root = CreateSampleTree();
			// direct children
			Assert.AreEqual(3, root.Children.Count);//0
			Assert.AreEqual(3, root.Children[0].Children.Count);//1
			Assert.AreEqual(0, root.Children[0].Children[0].Children.Count);//2
			Assert.AreEqual(0, root.Children[0].Children[1].Children.Count);//3
			Assert.AreEqual(2, root.Children[0].Children[2].Children.Count);//4
			Assert.AreEqual(0, root.Children[0].Children[2].Children[0].Children.Count);//5
			Assert.AreEqual(0, root.Children[0].Children[2].Children[1].Children.Count);//6
			Assert.AreEqual(0, root.Children[1].Children.Count);//7
			Assert.AreEqual(1, root.Children[2].Children.Count);//8
			Assert.AreEqual(0, root.Children[2].Children[0].Children.Count);//9
			// direct & indirect children
			Assert.AreEqual(9, Node.CountAllChildren(root));//0
			Assert.AreEqual(5, Node.CountAllChildren(root.Children[0]));//1
			Assert.AreEqual(0, Node.CountAllChildren(root.Children[0].Children[0]));//2
			Assert.AreEqual(0, Node.CountAllChildren(root.Children[0].Children[1]));//3
			Assert.AreEqual(2, Node.CountAllChildren(root.Children[0].Children[2]));//4
			Assert.AreEqual(0, Node.CountAllChildren(root.Children[0].Children[2].Children[0]));//5
			Assert.AreEqual(0, Node.CountAllChildren(root.Children[0].Children[2].Children[1]));//6
			Assert.AreEqual(0, Node.CountAllChildren(root.Children[1]));//7
			Assert.AreEqual(1, Node.CountAllChildren(root.Children[2]));//8
			Assert.AreEqual(0, Node.CountAllChildren(root.Children[2].Children[0]));//9
		}
	}



	public class Node: Compiler.CodeElements.Node, Generated {
		public Node(string Text = ".", bool GenerateChildren = true) {
			this._lines.Add(new TextLineSnapshot(-1, Text, null));
			this.IsLeaf = GenerateChildren;
		}
		private IList<ITextLine> _lines = new List<ITextLine>();
		public override IEnumerable<ITextLine> Lines {
			get { return new System.Collections.ObjectModel.ReadOnlyCollection<ITextLine>(_lines); }
		}
		public bool IsLeaf { get; private set; }
	}
}