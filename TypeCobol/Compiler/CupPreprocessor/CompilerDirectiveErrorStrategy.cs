using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.CupCommon;

namespace TypeCobol.Compiler.CupPreprocessor
{
    /// <summary>
    /// Specialization of Error Recovery for the preprocessor.
    /// </summary>
    public class CompilerDirectiveErrorStrategy : CupCobolErrorStrategy
    {
        public override bool SyntaxError(lr_parser parser, Stack stack, Symbol curToken)
        {
            ((CobolWordsTokenizer) parser.getScanner()).EnterStopScanningMode();
            ((CobolWordsTokenizer) parser.getScanner()).RevertLastToken();
            return base.SyntaxError(parser, stack, curToken);
        }
    }
}


