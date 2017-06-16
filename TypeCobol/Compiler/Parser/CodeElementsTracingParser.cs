using Antlr4.Runtime;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Utility class only used to profile Antlr performance
    /// </summary>
    internal class CodeElementsTracingParser : CodeElementsParser
    {
        public CodeElementsTracingParser(ITokenStream input) : base(input)
        {
            Profile = true;
        }

        public override void EnterRule(ParserRuleContext localctx, int state, int ruleIndex)
        {
            CodeElementsParserStep.AntlrPerformanceProfiler.EnterParserRule(localctx, ruleIndex);
            base.EnterRule(localctx, state, ruleIndex);
        }
    }
}