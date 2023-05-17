#nullable enable

using TypeCobol.Compiler.Directives;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// After the text preprocessing phase, this single token replaces all the tokens
    /// contributing to a compiler directive. 
    /// If the compiler directive spans several text lines, one single CompilerDirectiveToken
    /// will be created on the first line, and generic ContinuationTokenGroups will be created 
    /// on the following lines and will reference this first token.
    /// </summary>
    public class CompilerDirectiveToken : TokensGroup
    {
         /// <summary>
        /// Group all the tokens contributing to a compiler directive on the same line
        /// </summary>
        public CompilerDirectiveToken(CompilerDirective compilerDirective, IList<Token> originalTokens, bool hasError)
            : base((compilerDirective.Type == CompilerDirectiveType.COPY || compilerDirective.Type == CompilerDirectiveType.EXEC_SQL_INCLUDE) ? TokenType.COPY_IMPORT_DIRECTIVE :
                   (compilerDirective.Type == CompilerDirectiveType.REPLACE || compilerDirective.Type == CompilerDirectiveType.REPLACE_OFF) ? TokenType.REPLACE_DIRECTIVE :
                    TokenType.COMPILER_DIRECTIVE, originalTokens)
        {
            CompilerDirective = compilerDirective;
            HasError = hasError;
        }
        
        /// <summary>
        /// Details of the compiler directive represented by this token.
        /// </summary>
        public CompilerDirective CompilerDirective { get; private set; }

        /// <summary>
        /// Debug string : append compiler directive details
        /// </summary>
        public override string ToString()
        {
            return "*** DIRECTIVE " + (HasError?"INVALID SYNTAX -> ":"") + CompilerDirective.ToString() + " (" + base.ToString() + ") ***";
        }
    }
}
