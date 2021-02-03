using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Parser;
using TypeCobol.Test.Utils;

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
        protected override CompositeAnalyzerProvider CreateAnalyzerProvider()
        {
            //Add analyzers
            var analyzerProvider = new CompositeAnalyzerProvider();
            //CFG/DFA
            const string cfgDfaId = "cfg-dfa";
            analyzerProvider.AddActivator((o, t) => CfgDfaAnalyzerFactory.CreateCfgAnalyzer(cfgDfaId, CfgBuildingMode.Standard));
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
        public new void Part1_Incremental_TC_GlobaStorage()
        {
            base.Part1_Incremental_TC_GlobaStorage();
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
