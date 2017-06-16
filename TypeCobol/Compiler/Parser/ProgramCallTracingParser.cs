using Antlr4.Runtime;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Utility class only used to profile Antlr performance
    /// </summary>
    internal class ProgramClassTracingParser : ProgramClassParser
    {
        public ProgramClassTracingParser(ITokenStream input) : base(input)
        {
            Profile = true;
        }

        public override void EnterRule(ParserRuleContext localctx, int state, int ruleIndex)
        {
            ProgramClassParserStep.AntlrPerformanceProfiler.EnterParserRule(localctx, ruleIndex);
            base.EnterRule(localctx, state, ruleIndex);
        }
    }
}