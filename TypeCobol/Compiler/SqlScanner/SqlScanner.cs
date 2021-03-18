using System.Collections;
using System.Collections.Generic;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.SqlScanner
{
    /// <summary>
    /// Poc Man SQL Scanner
    /// </summary>
    public class SqlScanner : TUVienna.CS_CUP.Runtime.Scanner, IEnumerable<TUVienna.CS_CUP.Runtime.Symbol>, IEnumerator<TUVienna.CS_CUP.Runtime.Symbol>
    {
        public string SqlText { get; private set; }

        public List<TokensLine> Lines { get; private set; }
        public List<SqlToken> Tokens { get; private set; }
        private readonly int _startLine;
        private readonly int _columOffset;
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="sqlText">The SQL test to be parsed</param>
        /// <param name="startLine">Thz starting line of the the EXEC SQL statement</param>
        /// <param name="columOffset">Column Offset depending of the Layout format</param>
        public SqlScanner(string sqlText, int startLine, int columOffset)
        {
            Lines = new List<TokensLine>();
            Tokens = new List<SqlToken>();
            _startLine = startLine;
            _columOffset = columOffset;

            System.IO.StringReader sr = new System.IO.StringReader(sqlText);
            string line;
            int lineIndex = _startLine-1;
            while ((line = sr.ReadLine()) != null) {
                ITextLine textLine = new TextLineSnapshot(lineIndex++, new string(' ', _columOffset) + line, null);
                TokensLine tokenLine = new TokensLine(textLine, ColumnsLayout.FreeTextFormat);
                Lines.Add(tokenLine);                
            }
            StreamTokenizer tokenizer = new StreamTokenizer(sqlText.ToCharArray());
            int lineNumber, startColumn, endColumn;
            while (true)
            {
                int token = tokenizer.NextToken(out lineNumber, out startColumn, out endColumn);
                switch (token)
                {
                    case StreamTokenizer.TT_EOF:
                        return;
                    case StreamTokenizer.TT_EOL:
                        break;
                    case StreamTokenizer.TT_WORD:
                        {
                            SqlTokenType sqlTokType;
                            if (System.Enum.TryParse(tokenizer.sval, true, out sqlTokType))
                            {
                                SqlToken sqlToken = new SqlToken(sqlTokType, startColumn + columOffset, endColumn + columOffset, Lines[lineNumber - 1]);
                                sqlToken.LiteralValue = new AlphanumericLiteralTokenValue(tokenizer.sval);
                                AddToken(sqlToken);
                            }
                            else
                            {
                                SqlTokenType sqlKwType = (tokenizer.sval.ToUpper().Equals("END-EXEC")) ? SqlTokenType.END_EXEC : (SqlTokenType)TokenType.UserDefinedWord;
                                if (sqlKwType == SqlTokenType.END_EXEC)
                                {
                                    SqlToken sqlToken = new SqlToken(sqlKwType, startColumn + columOffset, endColumn + columOffset, Lines[lineNumber - 1]);
                                    sqlToken.LiteralValue = new AlphanumericLiteralTokenValue(tokenizer.sval);
                                    AddToken(sqlToken);
                                }
                            }
                        }
                        break;
                    case StreamTokenizer.TT_NUMBER:
                        {
                            SqlToken sqlToken = new SqlToken((SqlTokenType)TokenType.FloatingPointLiteral, startColumn + columOffset, endColumn + columOffset, Lines[lineNumber - 1]);
                            sqlToken.LiteralValue = new DecimalLiteralTokenValue(tokenizer.nval);
                            AddToken(sqlToken);
                        }
                        break;
                    default:
                        {   //Ignore Whitespaces
                            if (!System.Char.IsWhiteSpace((char)token)) {
                                SqlToken sqlToken = new SqlToken((SqlTokenType)TokenType.UserDefinedWord, startColumn + columOffset, endColumn + columOffset, Lines[lineNumber - 1]);
                                sqlToken.LiteralValue = new AlphanumericLiteralTokenValue(((char)token).ToString());
                                AddToken(sqlToken);
                            }
                        }
                        break;     
                }  
            }

            bool IsSqlTokenType(SqlToken sqlToken)
            {
                return (int)sqlToken.TokenType >= (int)SqlTokenType.EXEC && (int)sqlToken.TokenType < (int)SqlTokenType.LastSqlTokenType;
            }
            void AddToken(SqlToken sqlToken)
            {//only Add Keywords for this POC.
                if (IsSqlTokenType(sqlToken))
                {
                    Tokens.Add(sqlToken);
                }
            }
        }
        public SqlScanner(List<SqlToken> tokens)
        {
            Tokens = tokens;
        }

        private int tokenIndex = -1;

        /// <summary>
        /// The EOF symbol
        /// </summary>
        public static TUVienna.CS_CUP.Runtime.Symbol EOF => new TUVienna.CS_CUP.Runtime.Symbol(0, null);

        public TUVienna.CS_CUP.Runtime.Symbol Current =>
           tokenIndex >= 0 && tokenIndex < Tokens.Count 
            ? new TUVienna.CS_CUP.Runtime.Symbol((int)Tokens[tokenIndex].TokenType, Tokens[tokenIndex])
            : EOF;

        object IEnumerator.Current => Current;

        public Symbol next_token()
        {
            if (++tokenIndex < Tokens.Count)
            {
                SqlToken token = Tokens[tokenIndex];
                TUVienna.CS_CUP.Runtime.Symbol symbol = new TUVienna.CS_CUP.Runtime.Symbol((int)token.TokenType
                    - (int)SqlTokenType.EXEC + 2 , token);//+2 to skip EOF and error symbols
                return symbol;
            }
            else
            {
                return EOF;
            }
        }

        public IEnumerator<Symbol> GetEnumerator()
        {
            if ((tokenIndex + 1) < Tokens.Count)
                yield return next_token();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            if ((tokenIndex + 1) < Tokens.Count)
                yield return next_token();
        }

        public void Dispose()
        {            
        }

        public bool MoveNext()
        {
            return ++tokenIndex < Tokens.Count;
        }

        public void Reset()
        {
            tokenIndex = -1;
        }
    }
}
