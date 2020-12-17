using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;

using static TypeCobol.Analysis.Test.CfgTestUtils;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These tests check the DFA results of samples located in 'BasicDfaSamples' directory.
    /// </summary>
    [TestClass]
    public class BasicDfaSamplesTest
    {
        [TestMethod]
        public void SampleGotos0()
        {
            string source = Path.Combine(BasicDfaSamples, "SampleGotos0.cbl");
            string expectedDfaResults = Path.Combine(BasicDfaSamples, "SampleGotos0.dfa.csv");
            var dfaResults = ParseCompareDiagnosticsWithDfa(source, null, expectedDfaResults);
            Assert.IsTrue(dfaResults.Graphs.Count == 1);

            string expectedDotResult = Path.Combine(BasicDfaSamples, "SampleGotos0.dot");
            GenDotCfgAndCompare(dfaResults.Graphs[0], source, expectedDotResult, true);
        }
    }
}
