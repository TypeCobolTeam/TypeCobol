using System.IO;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Test
{
    public class CfgTestUtils
    {
        /// <summary>
        /// Generates a Dot CFG file
        /// </summary>
        /// <param name="cfg">The CFG to generate the dot file</param>
        /// <param name="dotFilePath">The Dot File to be generated.</param>
        /// <param name="bFullInstructions">true if full instruction source code must be generated, false if only instruction names.</param>
        public static void GenDotCfgFile(ControlFlowGraph<Node, object> cfg, string dotFilePath, bool bFullInstructions = true)
        {
            //Create a Dot File Generator            
            CfgDotFileForNodeGenerator<object> dotGen = new CfgDotFileForNodeGenerator<object>(cfg);
            dotGen.FullInstruction = bFullInstructions;
            StringWriter writer = new StringWriter();
            dotGen.Filepath = dotFilePath;
            dotGen.Report();
        }

        /// <summary>
        /// Generate the dot corresponding to Cfg and compare it with the expected file.
        /// </summary>
        /// <param name="cfg">The Control Flow Graph instance</param>
        /// <param name="expectedDotFile">The expected dot file</param>
        /// <param name="bFullInstruction">true if full instruction must be displayed, false otherwise</param>
        public static void GenDotCfgAndCompare(ControlFlowGraph<Node, object> cfg, string testPath, string expectedDotFile, bool bFullInstruction = true)
        {
            //Create a Dot File Generator            
            CfgDotFileForNodeGenerator<object> dotGen = new CfgDotFileForNodeGenerator<object>(cfg);
            dotGen.FullInstruction = bFullInstruction;
            StringWriter writer = new StringWriter();
            dotGen.Report(writer);

            // compare with expected result
            string result = writer.ToString();
            string expected = File.ReadAllText(expectedDotFile);
            TypeCobol.Test.TestUtils.compareLines(testPath, result, expected, expectedDotFile);
        }
    }
}
