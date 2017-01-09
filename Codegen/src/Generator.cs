using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;
namespace TypeCobol.Codegen {

	public class Generator {

		private readonly IReadOnlyList<ICobolTextLine> Input;
		private readonly TextWriter Writer;
        private GeneratorActions Actions = null;

		public Generator(TextWriter destination, IReadOnlyList<ICobolTextLine> source, List<Skeleton> skeletons) {
			Input = source;
			Writer = destination;
            Actions = new GeneratorActions(skeletons);
		}

		/// <summary>Generates code</summary>
		/// <param name="tree">Root of a syntax tree</param>
		/// <param name="table">Table of symbols</param>
		/// <param name="columns">Columns layout</param>
		public void Generate(Root tree, SymbolTable table, ColumnsLayout columns = ColumnsLayout.FreeTextFormat) {
			// STEP 1: modify tree to adapt it to destination language
            Actions.Perform(tree);
			//Console.WriteLine(tree.Root.ToString());

			// STEP 2: convert tree to destination language code
			var converter = new TreeToCode(Input, columns);
			tree.Accept(converter);
			converter.WriteInputLinesUntilEnd();
			Writer.Write(converter.Output.ToString());
			Writer.Flush();
            //Console.WriteLine(converter.Output.ToString());
		}
	}
}
