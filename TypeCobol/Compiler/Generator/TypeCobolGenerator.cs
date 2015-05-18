using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.TypeChecker;

namespace TypeCobol.Compiler.Generator
{
    // --> see http://stackoverflow.com/questions/20541073/translation-of-pl-sql-code-to-java-using-antlr-4-and-stringtemplate-4

    /// <summary>
    /// Code transformation tool which generates Cobol code from a TypeCobol source program
    /// </summary>
    internal class TypeCobolGenerator : IObserver<CodeModelChangedEvent>
    {
        /// <summary>
        /// Source TypeCobol code model
        /// </summary>
        public SemanticsDocument SourceCodeModel { get; private set; }

        /// <summary>
        /// TextDocument where the target Cobol code will be generated
        /// </summary>
        public ITextDocument GeneratedTextDocument { get; private set; }

        /// <summary>
        /// Initialize a TypeCobol to Cobol transformation 
        /// </summary>
        /// <param name="sourceCodeModel">Source TypeCobol code model</param>
        /// <param name="generatedTextDocument">TextDocument where the target Cobol code will be generated</param>
        public TypeCobolGenerator(SemanticsDocument sourceCodeModel, ITextDocument generatedTextDocument)
        {
            SourceCodeModel = sourceCodeModel;
            GeneratedTextDocument = generatedTextDocument;
        }
    
        public void OnCompleted()
        {
 	        // GeneratedTextDocument.Write ...
        }

        public void OnError(Exception error)
        {
 	        throw new NotImplementedException();
        }

        public void OnNext(CodeModelChangedEvent value)
        {
 	        throw new NotImplementedException();
        }
    }
}
