using System;
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
        /// through XxxParserStep.AntlrPerformanceProfiler static properties
        /// </summary>
        public bool ActivateDetailedAntlrPofiling { get; private set; }

        public int ParsingTime { get; set; }

        // Details of Antlr parsing time
        // Non zero only when Antlr performance profiling is activated
        public int DecisionTimeMs;
        public int RuleInvocationsCount;

        public int TreeBuildingTime { get; set; }

        private Stopwatch stopWatch = new Stopwatch();

        public void OnStartParsing()
        {
            stopWatch.Restart();
        }

        public void OnStopParsing(int decisionTimeMs, int ruleInvocationsCount)
        {
            stopWatch.Stop();
            ParsingTime += (int)stopWatch.ElapsedMilliseconds;
            DecisionTimeMs += decisionTimeMs;
            RuleInvocationsCount += ruleInvocationsCount;
        }

        public void OnStartTreeBuilding()
        {
            stopWatch.Restart();
        }

        public void OnStopTreeBuilding()
        {
            stopWatch.Stop();
            TreeBuildingTime += (int)stopWatch.ElapsedMilliseconds;
        }

        internal void Add(PerfStatsForParserInvocation perfStats)
        {
            ParsingTime += perfStats.ParsingTime;
            DecisionTimeMs += perfStats.DecisionTimeMs;
            RuleInvocationsCount += perfStats.RuleInvocationsCount;
            TreeBuildingTime += TreeBuildingTime;
        }
    }
}