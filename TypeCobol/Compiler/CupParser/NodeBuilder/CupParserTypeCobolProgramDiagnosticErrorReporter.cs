using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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

        public bool SyntaxError(lr_parser parser, Symbol curToken)
        {
            TypeCobolProgramParser tcpParser = parser as TypeCobolProgramParser;
            if (tcpParser.IsTrial)
                return true;
            List<string> expected = ExpectedSymbols(parser, curToken);
            string symName = CodeElementTokenizer.CupTokenToString(curToken.sym);
            string text = (curToken.value as CodeElement).Text;
            string msg = string.Format("extraneous input '{0}' expecting {{{1}}}", text, string.Join(", ", expected));
            System.Diagnostics.Debug.WriteLine(msg);
            CupParserDiagnostic diagnostic = new CupParserDiagnostic(msg, curToken, null);
            AddDiagnostic(diagnostic);
            //Try to add the last encountered statement in the stack if it is not already entered. 
            System.Collections.Stack stack = tcpParser.getParserStack();
            object[] items = stack.ToArray();
            foreach (var item in items)
            {
                Symbol symbol = item as Symbol;
                if (symbol.value is StatementElement)
                {
                    lr_parser stmtParser = CloneParser(parser, (int)TypeCobolProgramSymbols.StatementEntryPoint,
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
            base(code, offendingSymbol == null ? -1 : (offendingSymbol.value as CodeElement).Column, 
                offendingSymbol == null ? -1 : ((offendingSymbol.value as CodeElement).StopIndex < 0 ? -1 : ((offendingSymbol.value as CodeElement).StopIndex + 1)), 
                offendingSymbol == null ? -1 : (offendingSymbol.value as CodeElement).Line, message, exception)
        {
            OffendingSymbol = offendingSymbol;
            this.ruleStack = ruleStack;

            // TO DO - IMPORTANT : this is the INITIAL line number, and not the CURRENT line number
            // This is enough to pass all unit tests, but will return false informations in real usage !
            // for real line number, use a Snapshot
            if (Line < 0 && OffendingSymbol != null)
            {
                CodeElement e = OffendingSymbol.value as CodeElement;
                if (e != null && e.ConsumedTokens.Count > 0) Line = e.ConsumedTokens[0].Line;
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
