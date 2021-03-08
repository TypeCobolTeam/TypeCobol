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
        public SqlScanner(string sqlText)
        {
            Lines = new List<TokensLine>();
            Tokens = new List<SqlToken>();

            System.IO.StringReader sr = new System.IO.StringReader(sqlText);
            string line;
            int lineIndex = 0;
            while ((line = sr.ReadLine()) != null) {
                ITextLine textLine = new TextLineSnapshot(++lineIndex, line, null);
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
                                if (sqlTokType == SqlTokenType.END)
                                {//END-SQL
                                    StreamTokenizer.Context ctx = tokenizer.GetCurrentContext();
                                    int l, sc, ec;
                                    int minus = tokenizer.NextToken(out l, out sc, out ec);
                                    if (minus == '-')
                                    {
                                        int exec = tokenizer.NextToken(out l, out sc, out ec);
                                        if (exec == StreamTokenizer.TT_WORD)
                                        {
                                            SqlTokenType sqltok;
                                            if (System.Enum.TryParse(tokenizer.sval, true, out sqltok))
                                            {
                                                if (sqltok == SqlTokenType.EXEC)
                                                {
                                                    SqlToken end_exec = new SqlToken(SqlTokenType.END_EXEC, startColumn, ec, Lines[lineNumber - 1]);
                                                    end_exec.LiteralValue = new AlphanumericLiteralTokenValue(tokenizer.sval);
                                                    AddToken(end_exec);
                                                    continue;
                                                }
                                            }
                                        }
                                    }
                                    tokenizer.SetContext(ctx);
                                }
                                SqlToken sqlToken = new SqlToken(sqlTokType, startColumn, endColumn, Lines[lineNumber - 1]);
                                sqlToken.LiteralValue = new AlphanumericLiteralTokenValue(tokenizer.sval);
                                AddToken(sqlToken);
                            }
                            else
                            {
                                SqlTokenType sqlKwType = (tokenizer.sval.ToUpper().Equals("END-EXEC")) ? SqlTokenType.END_EXEC : (SqlTokenType)TokenType.UserDefinedWord;
                                if (sqlKwType == SqlTokenType.END_EXEC)
                                {
                                    SqlToken sqlToken = new SqlToken(sqlKwType, startColumn, endColumn, Lines[lineNumber - 1]);
                                    sqlToken.LiteralValue = new AlphanumericLiteralTokenValue(tokenizer.sval);
                                    AddToken(sqlToken);
                                }
                            }
                        }
                        break;
                    case StreamTokenizer.TT_NUMBER:
                        {
                            SqlToken sqlToken = new SqlToken((SqlTokenType)TokenType.FloatingPointLiteral, startColumn, endColumn, Lines[lineNumber - 1]);
                            sqlToken.LiteralValue = new DecimalLiteralTokenValue(tokenizer.nval);
                            AddToken(sqlToken);
                        }
                        break;
                    default:
                        {   //Ignore Whitespaces
                            if (!System.Char.IsWhiteSpace((char)token)) {
                                SqlToken sqlToken = new SqlToken((SqlTokenType)TokenType.UserDefinedWord, startColumn, endColumn, Lines[lineNumber - 1]);
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
