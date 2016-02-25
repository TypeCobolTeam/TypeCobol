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
            int line = 1, offset = 0;
            GenerateCode(Input.Program.SyntaxTree.Root, str, ref line, ref offset);
            System.IO.File.WriteAllText(filename, str.ToString());
System.Console.WriteLine("RESULT ("+filename+"):\n"+Input.Program.SyntaxTree.ToString());
        }

        private void GenerateCode(CodeElements.Node node, System.Text.StringBuilder str, ref int line, ref int offset) {
			var ce = node.CodeElement as TypeCobol.Compiler.CodeElements.CodeElement;
            if (ce != null) {
                foreach(var token in ce.ConsumedTokens) {
                    GenerateCode(token, str, ref line, ref offset);
                }
            }
            foreach(var child in node.Children) {
                GenerateCode(child, str, ref line, ref offset);
            }
        }

        private void GenerateCode(Scanner.Token token, System.Text.StringBuilder str, ref int line, ref int offset) {
//System.Console.WriteLine(">>>>>>>>>>>>>>>>>>>>>>>>>\nGenerateCode(line="+line+"):");
            while(line < token.Line) {
                str.AppendLine();
                line++;
                offset = 0;
            }
//            System.Console.WriteLine("WRITE TOKEN ("+token.StartIndex+">"+token.StopIndex+":"+token.Length+") \""+token.Text+"\" \""+token.SourceText+"\"");
            for(int c=offset; c<token.StartIndex; c++) str.Append(' ');
//            System.Console.WriteLine("(os old:"+offset+" new:"+token.StartIndex+")");
            offset = token.StartIndex;
            str.Append(token.Text);
            offset += token.Length;
//System.Console.WriteLine("------------------------\n"+str.ToString()+"<<<<<<<<<<<<<<<<<<<<<<<<<");
        }
    }
}
