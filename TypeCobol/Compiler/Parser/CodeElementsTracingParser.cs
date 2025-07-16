using Antlr4.Runtime;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Utility class only used to profile Antlr performance
    /// </summary>
    internal class CodeElementsTracingParser : CodeElementsParser
    {
        private readonly AntlrPerformanceProfiler _profiler;

        public CodeElementsTracingParser(ITokenStream input, AntlrPerformanceProfiler profiler)
            : base(input)
        {
            Profile = true;
            _profiler = profiler;
        }

        public override void EnterRule(ParserRuleContext localctx, int state, int ruleIndex)
        {
            _profiler.EnterParserRule(localctx, ruleIndex);
            base.EnterRule(localctx, state, ruleIndex);
        }
    }
}