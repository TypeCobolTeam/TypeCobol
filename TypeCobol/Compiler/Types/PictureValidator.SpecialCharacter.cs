namespace TypeCobol.Compiler.Types
{
    public partial class PictureValidator
    {
        /// <summary>
        /// Special characters of a picture character string.
        /// </summary>
        internal enum SC
        {
            B,
            ZERO, // 0
            SLASH, // /
            COMMA, // ,
            DOT, // .
            PLUS, // +
            MINUS, // -
            CR,
            DB,
            CS,
            E,
            Z,
            STAR, // *
            NINE, // 9                        
            A,
            X,
            S,
            V,
            P,
            G,
            N
        }
    }
}
