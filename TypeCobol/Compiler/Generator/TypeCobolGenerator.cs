using System;
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
        public void GenerateCobolText(string filename) {
            if (Input.Program == null) return;
            var str = new System.Text.StringBuilder();
            GenerateCode(str, 0, Input.Program.SyntaxTree.Root);
            System.IO.File.AppendAllText(filename, str.ToString());
        }

        private int GenerateCode(System.Text.StringBuilder str, int line, CodeElements.Node node) {
            if (node.CodeElement != null) {
                int c = 0;
                foreach(var token in node.CodeElement.ConsumedTokens) {
                    line = GenerateCode(str, token, line);
                }
            }
            foreach(var child in node.Children) {
                line = GenerateCode(str, line, child);
            }
            return line;
        }

        private int GenerateCode(System.Text.StringBuilder str, Scanner.Token token, int line) {
            while(line < token.Line) {
                str.AppendLine();
                line++;
            }
            str.Append(token.Text).Append(" ");
            return line;
        }
    }
}
