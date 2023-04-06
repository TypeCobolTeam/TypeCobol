using System.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace TypeCobol.Analysis.Test
{
    [TestClass]
    [Ignore]
    public class PerformanceWithCfg
    {
        private readonly TypeCobol.Test.Parser.Performance.Performance _performance;

        public PerformanceWithCfg()
        {
            //AnalyzerProvider for CFG only
            var analyzerProvider = new AnalyzerProviderWrapper(str => Debug.Fail(str));
            analyzerProvider.AddActivator((o, t) => CfgDfaAnalyzerFactory.CreateCfgAnalyzer(CfgBuildingMode.Standard, o));
            _performance = new TypeCobol.Test.Parser.Performance.Performance(analyzerProvider);
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_Incremental_Cobol85_NoRedefines()
        {
            _performance.Part1_Incremental_Cobol85_NoRedefines();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_Incremental_TC_BigTypesNoProcedure()
        {
            _performance.Part1_Incremental_TC_BigTypesNoProcedure();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_Incremental_TC_BigTypesWithProcedure()
        {
            _performance.Part1_Incremental_TC_BigTypesWithProcedure();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_Incremental_TC_GlobalStorage()
        {
            _performance.Part1_Incremental_TC_GlobalStorage();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_Incremental_TC_UseALotOfTypes_001Time()
        {
            _performance.Part2_Incremental_TC_UseALotOfTypes_001Time();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_Incremental_TC_UseALotOfTypes_100Times()
        {
            _performance.Part2_Incremental_TC_UseALotOfTypes_100Times();
        }
        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_Incremental_TC_UseALotOfTypes_WithProc_100Times()
        {
            _performance.Part2_Incremental_TC_UseALotOfTypes_WithProc_100Times();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part3_Incremental_Cobol85_DeepVariables()
        {
            _performance.Part3_Incremental_Cobol85_DeepVariables();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part3_Incremental_TC_DeepTypes()
        {
            _performance.Part3_Incremental_TC_DeepTypes();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_FullParsing_Cobol85_NoRedefines()
        {
            _performance.Part1_FullParsing_Cobol85_NoRedefines();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_FullParsing_TC_BigTypesNoProcedure()
        {
            _performance.Part1_FullParsing_TC_BigTypesNoProcedure();
        }
        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_FullParsing_TC_BigTypesWithProcedure()
        {
            _performance.Part1_FullParsing_TC_BigTypesWithProcedure();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part1_FullParsing_TC_GlobalStorage()
        {
            _performance.Part1_FullParsing_TC_GlobalStorage();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_FullParsing_TC_UseALotOfTypes_001Time()
        {
            _performance.Part2_FullParsing_TC_UseALotOfTypes_001Time();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_FullParsing_TC_UseALotOfTypes_100Times()
        {
            _performance.Part2_FullParsing_TC_UseALotOfTypes_100Times();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part2_FullParsing_TC_UseALotOfTypes_WithProc_100Times()
        {
            _performance.Part2_FullParsing_TC_UseALotOfTypes_WithProc_100Times();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part3_FullParsing_Cobol85_DeepVariables()
        {
            _performance.Part3_FullParsing_Cobol85_DeepVariables();
        }

        [TestMethod]
        [TestCategory("Performance")]
        [TestProperty("Time", "long")]
        //[Ignore]
        public void Part3_FullParsing_TC_DeepTypes()
        {
            _performance.Part3_FullParsing_TC_DeepTypes();
        }
    }
}
