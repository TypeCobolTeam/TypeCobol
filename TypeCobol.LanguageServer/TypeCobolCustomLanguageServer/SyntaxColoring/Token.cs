namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring
{
    /// <summary>
    /// All Token types 
    /// </summary>
    public enum TokenType
    {
        Unknown = 0,
        FormalComment = 1
    }

    /// <summary>
    /// A Token representation
    /// </summary>
    public class Token
    {
        /// <summary>
        /// The Token Type
        /// </summary>
        public TokenType Type { get; set; }

        /// <summary>
        /// Token range
        /// </summary>
        public VsCodeProtocol.Range Range { get; set; }

        /// <summary>
        /// The Token lexeme, can be null
        /// </summary>
        public string Lexeme { get; set; }
    }
}
