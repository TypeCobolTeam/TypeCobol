using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen {

	[TestClass]
	public class TreeGen {

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void TreeToCode() {
			var root1 = CreateSampleTree();
			root1.GetChildren()[0].GetChildren().Insert(2, new Node("X"));
			root1.GetChildren()[2].GetChildren().Insert(0, new Node("Y"));
			var writer1 = new TreeToCode();
			root1.Accept(writer1);
			System.Console.WriteLine(writer1.Output);
			//new TreeToCode().execute(root);
			var document = Parser.Parse("resources/input/TypeCobol/FUNCTION.cbl", Compiler.DocumentFormat.RDZReferenceFormat);
			var columns = document.Results.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
			var program = document.Results.ProgramClassDocumentSnapshot.Program;
			var root = program.SyntaxTree.Root;
			root.Get("Functions.data-division.working-storage").GetChildren().Insert(1, new Node("codegen", false));
			var writer = new TreeToCode(document.Results.TokensLines);
			root.Accept(writer);
			System.Console.WriteLine(writer.Output);
		}



		public Node CreateSampleTree() {
			//		0
			//     /|\
			//    1 7 8
			//   /|\  |
			//  2 3 4 9
			//     / \
			//    5   6
			var root = new Node("0");
			root.GetChildren().Add(new Node("1"));
			root.GetChildren().Add(new Node("7"));
			root.GetChildren().Add(new Node("8"));
			root.GetChildren()[0].GetChildren().Add(new Node("2"));
			root.GetChildren()[0].GetChildren().Add(new Node("3"));
			root.GetChildren()[0].GetChildren().Add(new Node("4"));
			root.GetChildren()[2].GetChildren().Add(new Node("9"));
			root.GetChildren()[0].GetChildren()[2].GetChildren().Add(new Node("5"));
			root.GetChildren()[0].GetChildren()[2].GetChildren().Add(new Node("6"));
			return root;
		}
		[TestMethod]
		[TestProperty("Time","fast")]
		public void TestSampleTreeCreation() {
			var root = CreateSampleTree();
			// direct children
			Assert.AreEqual(3, root.GetChildren().Count);//0
			Assert.AreEqual(3, root.GetChildren()[0].GetChildren().Count);//1
			Assert.AreEqual(0, root.GetChildren()[0].GetChildren()[0].GetChildren().Count);//2
			Assert.AreEqual(0, root.GetChildren()[0].GetChildren()[1].GetChildren().Count);//3
			Assert.AreEqual(2, root.GetChildren()[0].GetChildren()[2].GetChildren().Count);//4
			Assert.AreEqual(0, root.GetChildren()[0].GetChildren()[2].GetChildren()[0].GetChildren().Count);//5
			Assert.AreEqual(0, root.GetChildren()[0].GetChildren()[2].GetChildren()[1].GetChildren().Count);//6
			Assert.AreEqual(0, root.GetChildren()[1].GetChildren().Count);//7
			Assert.AreEqual(1, root.GetChildren()[2].GetChildren().Count);//8
			Assert.AreEqual(0, root.GetChildren()[2].GetChildren()[0].GetChildren().Count);//9
			// direct & indirect children
			Assert.AreEqual(9, CountAllChildren(root));//0
			Assert.AreEqual(5, CountAllChildren(root.GetChildren()[0]));//1
			Assert.AreEqual(0, CountAllChildren(root.GetChildren()[0].GetChildren()[0]));//2
			Assert.AreEqual(0, CountAllChildren(root.GetChildren()[0].GetChildren()[1]));//3
			Assert.AreEqual(2, CountAllChildren(root.GetChildren()[0].GetChildren()[2]));//4
			Assert.AreEqual(0, CountAllChildren(root.GetChildren()[0].GetChildren()[2].GetChildren()[0]));//5
			Assert.AreEqual(0, CountAllChildren(root.GetChildren()[0].GetChildren()[2].GetChildren()[1]));//6
			Assert.AreEqual(0, CountAllChildren(root.GetChildren()[1]));//7
			Assert.AreEqual(1, CountAllChildren(root.GetChildren()[2]));//8
			Assert.AreEqual(0, CountAllChildren(root.GetChildren()[2].GetChildren()[0]));//9
		}

		public static int CountAllChildren(Node<CodeElement> node) {
			int count = node.GetChildren().Count;
			foreach(var child in node.GetChildren())
				count += CountAllChildren(child);
			return count;
		}
	}



	public class Node: Node<CodeElement>, TypeCobol.Codegen.Nodes.Generated {
		public Node(string Text = ".", bool GenerateChildren = true): base(null) {
			this._lines.Add(new TextLineSnapshot(-1, Text, null));
			this.IsLeaf = GenerateChildren;
		}
		private IList<ITextLine> _lines = new List<ITextLine>();
		public IEnumerable<ITextLine> Lines {
			get { return new System.Collections.ObjectModel.ReadOnlyCollection<ITextLine>(_lines); }
		}
		public bool IsLeaf { get; private set; }
	}
}