using Antlr4.Runtime;
using TypeCobol.Compiler.Preprocessor.Generated;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Utility class only used to profile Antlr performance
    /// </summary>
    internal class CobolCompilerDirectivesTracingParser : CobolCompilerDirectivesParser
    {
        public CobolCompilerDirectivesTracingParser(ITokenStream input) : base(input)
        {
            Profile = true;
        }

        public override void EnterRule(ParserRuleContext localctx, int state, int ruleIndex)
        {
            PreprocessorStep.AntlrPerformanceProfiler.EnterParserRule(localctx, ruleIndex);
            base.EnterRule(localctx, state, ruleIndex);
        }
    }
}