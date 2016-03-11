using System;
using System.IO;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Generator
{
    // --> see http://stackoverflow.com/questions/20541073/translation-of-pl-sql-code-to-java-using-antlr-4-and-stringtemplate-4

    /// <summary>
    /// Code transformation tool which generates Cobol code from a TypeCobol source program
    /// </summary>
    public class TypeCobolGenerator
    {
        /// <summary>
        /// Source TypeCobol code model
        /// </summary>
        public ProgramClassDocument Input { get; private set; }

        /// <summary>
        /// Output text document where Cobol code will be generated
        /// </summary>
        public ITextDocument OutputDocument { get; private set; }

        /// <summary>
        /// Initialize a TypeCobol to Cobol transformation 
        /// </summary>
        /// <param name="input">Input program</param>
        /// <param name="output">Ouput text document where Cobol code will be generated</param>
        public TypeCobolGenerator(ProgramClassDocument input, ITextDocument output) {
            Input = input;
            OutputDocument = output;
        }
    
        /// <summary>
        /// Generate Cobol program corresponding to the TypeCobol source
        /// </summary>
		public void WriteCobol(TextWriter stream) {
            int line = 1, offset = 0;
            WriteCobol(stream, Input.Program.SyntaxTree.Root, ref line, ref offset);
			stream.Close();
		}

        private void WriteCobol(TextWriter stream, CodeElements.Node node, ref int line, ref int offset) {
			var ce = node.CodeElement as TypeCobol.Generator.CodeGenerator;
            if (ce != null) ce.WriteCode(stream, node.SymbolTable, ref line, ref offset);
            foreach(var child in node.Children) {
                WriteCobol(stream, child, ref line, ref offset);
            }
		}
	}
}
