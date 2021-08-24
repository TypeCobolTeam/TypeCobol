using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TypeCobol.Analysis.Test
{
    [TestClass]
    [Ignore]
    public class PerformanceWithCfg : TypeCobol.Test.Parser.Performance.Performance
    {
        /// <summary>
        /// AnalyzerProvider for test.
        /// </summary>
        /// <returns></returns>
        protected override IAnalyzerProvider CreateAnalyzerProvider()
        {
            //Add analyzers
            var analyzerProvider = new AnalyzerProvider(str => throw new Exception(str));
            //CFG/DFA
            analyzerProvider.AddActivator((o, t) => CfgDfaAnalyzerFactory.CreateCfgAnalyzer(CfgBuildingMode.Standard, o));
            return analyzerProvider;
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part1_Incremental_Cobol85_NoRedefines()
        {
            base.Part1_Incremental_Cobol85_NoRedefines();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part1_Incremental_TC_BigTypesNoProcedure()
        {
            base.Part1_Incremental_TC_BigTypesNoProcedure();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part1_Incremental_TC_BigTypesWithProcedure()
        {
            base.Part1_Incremental_TC_BigTypesWithProcedure();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part1_Incremental_TC_GlobalStorage()
        {
            base.Part1_Incremental_TC_GlobalStorage();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part2_Incremental_TC_UseALotOfTypes_001Time()
        {
            base.Part2_Incremental_TC_UseALotOfTypes_001Time();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part2_Incremental_TC_UseALotOfTypes_100Times()
        {
            base.Part2_Incremental_TC_UseALotOfTypes_100Times();
        }
        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part2_Incremental_TC_UseALotOfTypes_WithProc_100Times()
        {
            base.Part2_Incremental_TC_UseALotOfTypes_WithProc_100Times();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part3_Incremental_Cobol85_DeepVariables()
        {
            base.Part3_Incremental_Cobol85_DeepVariables();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part3_Incremental_TC_DeepTypes()
        {
            base.Part3_Incremental_TC_DeepTypes();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part1_FullParsing_Cobol85_NoRedefines()
        {
            base.Part1_FullParsing_Cobol85_NoRedefines();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part1_FullParsing_TC_BigTypesNoProcedure()
        {
            base.Part1_FullParsing_TC_BigTypesNoProcedure();
        }
        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part1_FullParsing_TC_BigTypesWithProcedure()
        {
            base.Part1_FullParsing_TC_BigTypesWithProcedure();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part1_FullParsing_TC_GlobalStorage()
        {
            base.Part1_FullParsing_TC_GlobalStorage();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part2_FullParsing_TC_UseALotOfTypes_001Time()
        {
            base.Part2_FullParsing_TC_UseALotOfTypes_001Time();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part2_FullParsing_TC_UseALotOfTypes_100Times()
        {
            base.Part2_FullParsing_TC_UseALotOfTypes_100Times();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part2_FullParsing_TC_UseALotOfTypes_WithProc_100Times()
        {
            base.Part2_FullParsing_TC_UseALotOfTypes_WithProc_100Times();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part3_FullParsing_Cobol85_DeepVariables()
        {
            base.Part3_FullParsing_Cobol85_DeepVariables();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public new void Part3_FullParsing_TC_DeepTypes()
        {
            base.Part3_FullParsing_TC_DeepTypes();
        }

    }
}
