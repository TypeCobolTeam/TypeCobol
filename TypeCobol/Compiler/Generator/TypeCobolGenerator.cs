using System;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Generator
{
    // --> see http://stackoverflow.com/questions/20541073/translation-of-pl-sql-code-to-java-using-antlr-4-and-stringtemplate-4

    /// <summary>
    /// Code transformation tool which generates Cobol code from a TypeCobol source program
    /// </summary>
    internal class TypeCobolGenerator
    {
        /// <summary>
        /// Source TypeCobol code model
        /// </summary>
        public ProgramClassDocument InputTypeCobolProgram { get; private set; }

        /// <summary>
        /// Output text document where Cobol code will be generated
        /// </summary>
        public ITextDocument OutputCobolTextDocument { get; private set; }

        /// <summary>
        /// Initialize a TypeCobol to Cobol transformation 
        /// </summary>
        /// <param name="sourceCodeModel">Source TypeCobol code model</param>
        /// <param name="generatedTextDocument">Ouput text document where Cobol code will be generated</param>
        public TypeCobolGenerator(ProgramClassDocument inputTypeCobolProgram, ITextDocument outputCobolTextDocument)
        {
            InputTypeCobolProgram = inputTypeCobolProgram;
            OutputCobolTextDocument = outputCobolTextDocument;
        }
    
        /// <summary>
        /// Generatea Cobol program corresponding to the TypeCobol source
        /// </summary>
        public void GenerateCobolText()
        {
 	        // GeneratedTextDocument.Write ...
        }        
    }
}
