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

        public virtual bool SyntaxError(lr_parser parser, Stack stack, Symbol curToken)
        {
            string input = "<unknown input>";
            IToken token = null;
            if (curToken != null && curToken.value != null)
            {
                input = (token = ((IToken)curToken.value)).Text;
            }
            else
            {//Look back the stack to find a valid token.
            }
            List<string> expected = ExpectedSymbols(parser, stack, curToken);
            string msg = "";
            if (expected != null && expected.Count == 1)
            {
                msg = "mismatched input " + GetTokenErrorDisplay(curToken != null ? (IToken)curToken.value : null) + 
                    " expecting " + expected[0];
            }
            else
            {
                msg = "no viable alternative at input " + EscapeWSAndQuote(input);
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
