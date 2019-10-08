using TypeCobol.Tools.Options_Config;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
using System.IO;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Cfg;
using TypeCobol.Analysis.Graph;
using TypeCobol.Analysis;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// Test Context for Cfg Dfa tests
    /// </summary>
    public class CfgDfaTestContext : CfgDfaContext
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mode">Testing mode</param>
        public CfgDfaTestContext(Mode mode) : base(mode)
        {
        }

        /// <summary>
        /// Initialize the test session
        /// </summary>
        public void TestInitialize()
        {
            base.Initialize();
        }

        /// <summary>
        /// Cleanup the Test
        /// </summary>
        public void TestCleanup()
        {
            base.Cleanup();
        }
    }
}
