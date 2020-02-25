using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CSCupRuntime;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Preprocessor;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// TypeCobol Program Diagnosctic Error Reporter.
    /// </summary>
    public class CupParserTypeCobolProgramDiagnosticErrorReporter : ICupParserErrorReporter
    {
        /// <summary>
        /// Stores all encoutered diagnostics
        /// </summary>
        public List<Diagnostic> Diagnostics
        {
            get;
            private set;
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
        public bool ReportFatalError(lr_parser parser, string message, object info)
        {
            return true;
        }

        public bool ReportError(lr_parser parser, string message, object info)
        {
            return true;
        }

        /// <summary>
        /// Create a new parser using a clone.
        /// </summary>
        /// <param name="parser">The parser to be cloned</param>
        /// <param name="start">The start entry point to use</param>
        /// <param name="firstSymbol">The First Symbol</param>
        /// <param name="trial">If we are creating atrial parser, fals eo therwise</param>
        /// <returns>The new parser</returns>
        public lr_parser CloneParser(lr_parser parser, int start, CodeElement firstSymbol, bool trial)
        {
            TypeCobolProgramParser tcpParser = parser as TypeCobolProgramParser;
            ProgramClassBuilder builder = tcpParser.Builder as ProgramClassBuilder;
            var errorReporter = tcpParser.ErrorReporter;
            var tokenizer = parser.getScanner() as CodeElementTokenizer;
            CodeElementTokenizer newTokenizer = new CodeElementTokenizer(start, firstSymbol);
            TypeCobolProgramParser newParser = new TypeCobolProgramParser(newTokenizer);
            newParser.Builder = builder;
            newParser.ErrorReporter = errorReporter;
            newParser.IsTrial = trial;
            return newParser;
        }

        /// <summary>
        /// Get the first valid Symbol on the parser stack having a value.
        /// </summary>
        /// <param name="parser">The parser stack</param>
        /// <param name="curToken">The current Symbol</param>
        /// <returns>The first valid symbol if any, null otherwise</returns>
        private static Symbol GetParserValidStackSymbol(lr_parser parser, Symbol curToken)
        {
            if (curToken.value != null)
                return curToken;
            //lookback in the stack to find a Symbol having a valid value.
            StackList<Symbol> stack = ((TypeCobolProgramParser)parser).getParserStack();
            Symbol lastValid = null;
            foreach (Symbol s in stack)
            {
                if (s.value != null)
                {
                    lastValid = s;
                }
            }
            return lastValid;
        }

        public bool SyntaxError(lr_parser parser, Symbol curToken)
        {
            TypeCobolProgramParser tcpParser = parser as TypeCobolProgramParser;
            if (tcpParser.IsTrial)
                return true;
            List<string> expected = ExpectedSymbols(parser, curToken);
            string symName = CodeElementTokenizer.CupTokenToString(curToken.sym);
            Symbol validSymbol = GetParserValidStackSymbol(parser, curToken);
            string text = validSymbol != null ? (validSymbol.value as CodeElement).Text : "";
            string msg = string.Format("extraneous input '{0}' expecting {{{1}}}", text, string.Join(", ", expected));
            System.Diagnostics.Debug.WriteLine(msg);
            CupParserDiagnostic diagnostic = new CupParserDiagnostic(msg, validSymbol, null);
            AddDiagnostic(diagnostic);
            //Try to add the last encountered statement in the stack if it is not already entered. 
            StackList<Symbol> stack = tcpParser.getParserStack();
            foreach (var symbol in stack)
            {
                if (symbol.value is StatementElement)
                {
                    lr_parser stmtParser = CloneParser(parser, TypeCobolProgramSymbols.StatementEntryPoint,
                        symbol.value as CodeElement, true);
                    stmtParser.parse();
                    break;
                }
            }
            return true;
        }

        public bool UnrecoveredSyntaxError(lr_parser parser, Symbol curToken)
        {
            return true;
        }

        /// <summary>
        /// Get the array of expected symbols on the given symbol current parser state.
        /// </summary>
        /// <param name="parser">The parser</param>
        /// <param name="curToken">The Symbol</param>
        /// <returns>The array of expected symbols</returns>
        private static List<string> ExpectedSymbols(lr_parser parser, Symbol curToken)
        {
            var actionTab = parser.action_table();
            int state = ((TypeCobolProgramParser)parser).getParserState();
            short[] row = actionTab[state];
            List<string> expected = new List<string>();
            for (int probe = 0; probe < row.Length; probe++)
            {
                int tag = row[probe++];
                if (tag != -1 && tag != parser.error_sym())
                {//symbol tag different of the default symbol.
                    string name = CodeElementTokenizer.CupTokenToString(tag);
                    if (name != null)
                    {
                        expected.Add(name);
                    }
                }
            }
            return expected;
        }
    }

    /// <summary>
    /// Enhanced error message containing additional information about 
    /// the origin of the syntax error in the grammar : offending symbol, rule stack
    /// </summary>
    public class CupParserDiagnostic : Diagnostic
    {
        public CupParserDiagnostic(string message, Symbol offendingSymbol, string ruleStack, MessageCode code = MessageCode.SyntaxErrorInParser, Exception exception = null) :
            base(code, (offendingSymbol == null || offendingSymbol.value == null) ? -1 : (offendingSymbol.value as CodeElement).Column,
                offendingSymbol == null ? -1 : ((offendingSymbol.value as CodeElement).StopIndex < 0 ? -1 : ((offendingSymbol.value as CodeElement).StopIndex + 1)),
                offendingSymbol == null ? -1 : (offendingSymbol.value as CodeElement).Line, message, exception)
        {
            OffendingSymbol = offendingSymbol;
            this.ruleStack = ruleStack;

            CodeElement e = offendingSymbol?.value as CodeElement;
            if (e != null && e.IsInsideCopy())
            {
                var copyToken = e.FirstCopyDirective?.COPYToken;
                if (copyToken != null) Line = copyToken.Line;
            }
        }

        public CupParserDiagnostic(string message, int start, int stop, int line, string ruleStack, MessageCode code = MessageCode.SyntaxErrorInParser, Exception exception = null)
            : base(code, start, stop, line, message, exception)
        {
            this.ruleStack = ruleStack;
        }

        /// <summary>
        /// First token which did not match the current rule in the grammar
        /// </summary>
        public Symbol OffendingSymbol { get; private set; }

        /// <summary>Grammar rules which were being recognized when an incorrect token occured.</summary>
        private readonly string ruleStack;

        public string ToStringWithRuleStack()
        {
            var str = new StringBuilder();
            str.Append(base.ToString());
            if (ruleStack != null) str.Append(" RuleStack=" + ruleStack + ", ");
            if (OffendingSymbol != null)
            {
                str.Append(" OffendingSymbol=").Append(OffendingSymbol.value);
                var importedToken = OffendingSymbol.value as ImportedToken;
                if (importedToken != null)
                {
                    str.Append(" in ").Append(importedToken.CopyDirective);
                }
            }
            return str.ToString();
        }
    }

}
