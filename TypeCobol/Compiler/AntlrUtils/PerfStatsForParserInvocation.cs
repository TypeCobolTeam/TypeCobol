using System.Diagnostics;

namespace TypeCobol.Compiler.AntlrUtils
{
    public class PerfStatsForParserInvocation
    {
        public PerfStatsForParserInvocation(bool activateDetailedAntlrPofiling)
        {
            ActivateDetailedAntlrPofiling = activateDetailedAntlrPofiling;
        }

        /// <summary>
        /// Set to true to activate very detailed Anltr profiling statistics, which can then be accessed 
        /// through DetailedAntlrProfiling property of this instance
        /// </summary>
        public bool ActivateDetailedAntlrPofiling { get; }

        public AntlrPerformanceProfiler DetailedAntlrProfiling { get; internal set; }

        public int ParsingTime { get; private set; }

        // Details of Antlr parsing time
        // Non zero only when Antlr performance profiling is activated
        public int DecisionTimeMs;
        public int RuleInvocationsCount;

        public int TreeBuildingTime { get; private set; }

        private readonly Stopwatch _stopWatch = new Stopwatch();

        public void OnStartParsing()
        {
            _stopWatch.Restart();
        }

        public void OnStopParsing()
        {
            int decisionTimeMs = DetailedAntlrProfiling != null
                ? (int)DetailedAntlrProfiling.CurrentFileInfo.DecisionTimeMs
                : 0;
            int ruleInvocationsCount = DetailedAntlrProfiling != null
                ? DetailedAntlrProfiling.CurrentFileInfo.RuleInvocations.Sum()
                : 0;
            _stopWatch.Stop();
            ParsingTime += (int)_stopWatch.ElapsedMilliseconds;
            DecisionTimeMs += decisionTimeMs;
            RuleInvocationsCount += ruleInvocationsCount;
        }

        public void OnStartTreeBuilding()
        {
            _stopWatch.Restart();
        }

        public void OnStopTreeBuilding()
        {
            _stopWatch.Stop();
            TreeBuildingTime += (int)_stopWatch.ElapsedMilliseconds;
        }

        internal void Add(PerfStatsForParserInvocation perfStats)
        {
            ParsingTime += perfStats.ParsingTime;
            DecisionTimeMs += perfStats.DecisionTimeMs;
            RuleInvocationsCount += perfStats.RuleInvocationsCount;
            TreeBuildingTime += TreeBuildingTime;

            // Adding detailed stats for ANTLR is not defined. DetailedAntlrProfiling is available only
            // for a single parser invocation. So nothing available on PerfStatsForParsingStep.TotalParsingTime.
            DetailedAntlrProfiling = null;
        }
    }
}