using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CSCupRuntime;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.CupCommon;

namespace TypeCobol.Compiler.CupPreprocessor
{
    /// <summary>
    /// Specialization of Error Recovery for the preprocessor.
    /// </summary>
    public class CompilerDirectiveErrorStrategy : CupCobolErrorStrategy
    {
        public override bool SyntaxError(lr_parser parser, StackList<Symbol> stack, Symbol curToken)
        {
            LastMismatchedSymbol = null;
            bool bResult = base.SyntaxError(parser, stack, curToken);
            ((CobolWordsTokenizer)parser.getScanner()).EnterStopScanningMode();
            ((CobolWordsTokenizer)parser.getScanner()).RevertLastToken(LastMismatchedSymbol);
            return bResult;
        }
    }
}


