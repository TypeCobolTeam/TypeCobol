using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CupCommon
{
    /// <summary>
    /// Basic Cobol Error Strategy based on Cup parser.
    /// </summary>
    public class CupCobolErrorStrategy : ICupParserErrorReporter
    {
        /// <summary>
        /// Stores all encoutered diagnostics
        /// </summary>
        public List<Diagnostic> Diagnostics
        {
            get;
            set;
        }
        /// <summary>
        /// Add a diagnostic
        /// </summary>
        /// <param name="diag">The diagnostic to be added</param>
        private void AddDiagnostic(Diagnostic diag)
        {
            if (Diagnostics == null)
            {
                Diagnostics = new List<Diagnostic>();
            }
            Diagnostics.Add(diag);
        }

        public virtual bool ReportFatalError(lr_parser parser, Stack stack, string message, object info)
        {
            return true;
        }

        public virtual bool ReportError(lr_parser parser, Stack stack, string message, object info)
        {
            return true;
        }

        /// <summary>
        /// Get the first valid Symbol on the parser stack having a value.
        /// </summary>
        /// <param name="parser">The parser stack</param>
        /// <param name="curToken">The current Symbol</param>
        /// <returns>The first valid symbol if any, null otherwise</returns>
        protected virtual Symbol GetParserValidStackSymbol(lr_parser parser, Stack stack, Symbol curToken)
        {
            if (curToken != null && curToken.value != null)
                return curToken;
            //lookback in the stack to find a Symbol having a valid value.
            Symbol lastValid = null;
            foreach (Symbol s in stack)
            {//The first one will be the top of the stack wil be the last symbol encountered
                if (s.value != null)
                {
                    lastValid = s;
                    break;
                }
            }
            return lastValid;
        }

        /// <summary>
        /// Determines if the given token has been consumed. A token is consumed if it is
        /// present on the parser stack.
        /// </summary>
        /// <param name="parser">The parser</param>
        /// <param name="stack">The parser stack</param>
        /// <param name="curToken">The Token to check</param>
        /// <returns>true if the token is consumed, false otherwise</returns>
        public bool IsTokenConsumed(lr_parser parser, Stack stack, Symbol curToken)
        {
            if (curToken?.value == null)
                return false;
            return stack.Contains(curToken);
        }

        /// <summary>
        /// The Last mismatched Token.
        /// </summary>
        protected Symbol LastMismatchedSymbol
        {
            get; set;
        }

        public virtual bool SyntaxError(lr_parser parser, Stack stack, Symbol curToken)
        {
            curToken = GetParserValidStackSymbol(parser, stack, curToken);
            string input = "<unknown input>";
            IToken token = null;
            if (curToken != null && curToken.value != null)
            {
                input = GetTokenErrorDisplay(token = ((IToken)curToken.value));
            }
            else
            {//Look back the stack to find a valid token.
            }
            List<string> expected = ExpectedSymbols(parser, stack, curToken);
            string msg = "";
            if (expected != null && expected.Count >= 1)
            {
                if (IsTokenConsumed(parser, stack, curToken))
                {
                    msg = "missing " +  (expected.Count == 1 ? expected[0] : (" {" + string.Join(", ", expected) + "}"))
                        + " at " + input;
                }
                else
                {
                    LastMismatchedSymbol = curToken;
                    msg = "mismatched input " + input +
                          " expecting " +
                          (expected.Count == 1 ? expected[0] : (" expecting {" + string.Join(", ", expected) + "}"));
                }
            }
            else
            {
                msg = "no viable alternative at input " + input;
            }
            CupParserDiagnostic diag = new CupParserDiagnostic(msg, token,null);
            AddDiagnostic(diag);            
            return true;
        }

        public virtual bool UnrecoveredSyntaxError(lr_parser parser, Stack stack, Symbol curToken)
        {
            return true;
        }

        public static String EscapeWSAndQuote(String s)
        { 
            String result = s; 
            result = s.Replace("\n", "\\n");
            result = s.Replace("\r","\\r");
            result = s.Replace("\t","\\t"); 
            return "'" + result + "'"; 
        }

        protected string GetTokenErrorDisplay(IToken t)
        {
            if (t == null)
            {
                return "<no token>";
            }
            string s = t.Text;
            if (s == null)
            {
                s = "<" + TokenUtils.GetDisplayNameForTokenType((TokenType)t.Type) + ">";
            }
            return EscapeWSAndQuote(s);
        }

        /// <summary>
        /// Get the array of expected symbols on the given symbol current parser state.
        /// </summary>
        /// <param name="parser">The parser</param>
        /// <param name="curToken">The Symbol</param>
        /// <returns>The array of expected symbols</returns>
        protected internal static List<string> ExpectedSymbols(lr_parser parser, Stack stack, Symbol curToken)
        {
            var actionTab = parser.action_table();
            int state = ((Symbol)stack.Peek()).parse_state;
            short[] row = actionTab[state];
            List<string> expected = new List<string>();
            for (int probe = 0; probe < row.Length; probe++)
            {
                int tag = row[probe++];
                if (tag != -1 && tag != parser.error_sym())
                {//symbol tag different of the default symbol.
                    string name = CobolWordsTokenizer.CupTokenToString(tag);
                    if (name != null)
                    {
                        expected.Add(name);
                    }
                }
            }
            return expected;
        }
    }
}
