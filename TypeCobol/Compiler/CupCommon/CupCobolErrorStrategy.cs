using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TUVienna.CS_CUP.Runtime;

namespace TypeCobol.Compiler.CupCommon
{
    /// <summary>
    /// Basic Cobol Error Strategy based on Cup parser.
    /// </summary>
    public class CupCobolErrorStrategy : ICupParserErrorReporter
    {
        public virtual bool ReportFatalError(lr_parser parser, Stack stack, string message, object info)
        {
            throw new NotImplementedException();
        }

        public virtual bool ReportError(lr_parser parser, Stack stack, string message, object info)
        {
            throw new NotImplementedException();
        }

        public virtual bool SyntaxError(lr_parser parser, Stack stack, Symbol curToken)
        {
            throw new NotImplementedException();
        }

        public virtual bool UnrecoveredSyntaxError(lr_parser parser, Stack stack, Symbol curToken)
        {
            throw new NotImplementedException();
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
