using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Symbols;
using System.IO;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Nodes;

using static TypeCobol.Analysis.Test.CfgTestUtils;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests for Gen and Kill set building.
    /// </summary>
    [TestClass]
    public class DfaGenAndKillSetTest
    {
        /// <summary>
        /// This test check the GEN set of the sample SampleGotos0.cbl according to its corresponding
        /// dot graph.
        /// </summary>
        //digraph Cfg {
        //    node[
        //    shape = "record"
        //    ]
        //edge[
        //arrowtail = "empty"
        //]
        //            Block0[
        //            label = "{START|}"
        //            ]
        //            Block1[
        //            label = "{L1. Block1|    MOVE 2 TO I\l    MOVE I TO J\l    ADD 1 TO J\l}"
        //            ]
        //            Block2[
        //            label = "{L2. Block2|    MOVE 1 TO I\l    IF J = 999 THEN\l}"
        //            ]
        //            Block3[
        //            label = "{Block3|         GO TO L4\l}"
        //            ]
        //            Block10[
        //            label = "{L4. Block10|    GO TO L2\l}"
        //            ]
        //            Block5[
        //            label = "{Block5|}"
        //            ]
        //            Block6[
        //            label = "{L3. Block6|    ADD 1 TO J\l    IF J = 999 THEN\l}"
        //            ]
        //            Block7[
        //            label = "{Block7|         GO TO L4\l}"
        //            ]
        //            Block9[
        //            label = "{Block9|    SUBTRACT 4 FROM J\l}"
        //            ]
        //            Block0->Block1
        //            Block1->Block2
        //            Block2->Block3
        //            Block2->Block5
        //            Block3->Block10
        //            Block10->Block2
        //            Block5->Block6
        //            Block6->Block7
        //            Block6->Block9
        //            Block7->Block10
        //            Block9->Block10
        //}
        [TestMethod]
        public void GEN_SampleGotos0()
        {
            string path = Path.Combine(BasicDfaSamples, "SampleGotos0.cbl");
            var cfg = ParseCompareDiagnosticsForDfa(path);

            Assert.IsTrue(cfg.Count == 1);

            //Resolve variable I
            QualifiedName qi = new URI("I");
            var namesI = cfg[0].ProcedureDivisionNode.SymbolTable.GetVariablesExplicitWithQualifiedName(qi);
            Assert.AreEqual(1, namesI.Count);
            Symbol I = namesI[0].Value.SemanticData;

            //Resolve variable J
            QualifiedName qj = new URI("J");
            var namesJ = cfg[0].ProcedureDivisionNode.SymbolTable.GetVariablesExplicitWithQualifiedName(qj);
            Assert.AreEqual(1, namesJ.Count);
            Symbol J = namesJ[0].Value.SemanticData;

            DefaultDataFlowGraphBuilder dfaBuilder = new DefaultDataFlowGraphBuilder(cfg[0]);
            dfaBuilder.ComputeGenSet();

            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[0].Data.Gen);            
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[3].Data.Gen);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[4].Data.Gen);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[5].Data.Gen);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[7].Data.Gen);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[8].Data.Gen);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[10].Data.Gen);

            //----------------------------------------------------------
            //checked that GEN(Block(1)) = "101" : bit 0 and 2 are set
            //----------------------------------------------------------
            Assert.IsNotNull(dfaBuilder.Cfg.AllBlocks[1].Data.Gen);            
            Assert.AreEqual("{0, 2}", dfaBuilder.Cfg.AllBlocks[1].Data.Gen.ToString());
            //MOVE 2 TO I
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[1].Data.Gen.Get(0));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.MoveStatement, dfaBuilder.DefList[0].Instruction.CodeElement.Type);
            Assert.AreEqual(I, dfaBuilder.DefList[0].Variable);
            //MOCE I TO J
            Assert.IsFalse(dfaBuilder.Cfg.AllBlocks[1].Data.Gen.Get(1));

            //ADD 1 TO J
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[1].Data.Gen.Get(2));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.AddStatement, dfaBuilder.DefList[2].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[2].Variable);

            //---------------------------------------------------------
            //checked that GEN(Block(2)) = "0001" : bit 3 is set
            //---------------------------------------------------------
            Assert.IsNotNull(dfaBuilder.Cfg.AllBlocks[2].Data.Gen);            
            Assert.AreEqual("{3}", dfaBuilder.Cfg.AllBlocks[2].Data.Gen.ToString());
            //MOVE 1 TO I
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[2].Data.Gen.Get(3));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.MoveStatement, dfaBuilder.DefList[3].Instruction.CodeElement.Type);
            Assert.AreEqual(I, dfaBuilder.DefList[3].Variable);

            //---------------------------------------------------------
            //checked that GEN(Block(6)) = "00001" : bit 4 is set
            //---------------------------------------------------------
            Assert.IsNotNull(dfaBuilder.Cfg.AllBlocks[6].Data.Gen);            
            Assert.AreEqual("{4}", dfaBuilder.Cfg.AllBlocks[6].Data.Gen.ToString());
            //ADD 1 TO J
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[6].Data.Gen.Get(4));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.AddStatement, dfaBuilder.DefList[4].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[4].Variable);

            //---------------------------------------------------------
            //checked that GEN(Block(9)) = "000001" : bit 5 is set
            //---------------------------------------------------------
            Assert.IsNotNull(dfaBuilder.Cfg.AllBlocks[9].Data.Gen);            
            Assert.AreEqual("{5}", dfaBuilder.Cfg.AllBlocks[9].Data.Gen.ToString());
            //SUBSTRACT 4 FROM J
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[9].Data.Gen.Get(5));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.SubtractStatement, dfaBuilder.DefList[5].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[5].Variable);
        }

        [TestMethod]
        public void KILL_SampleGotos0()
        {
            string path = Path.Combine(BasicDfaSamples, "SampleGotos0.cbl");
            var cfg = ParseCompareDiagnosticsForDfa(path);

            Assert.IsTrue(cfg.Count == 1);

            //Resolve variable I
            QualifiedName qi = new URI("I");
            var namesI = cfg[0].ProcedureDivisionNode.SymbolTable.GetVariablesExplicitWithQualifiedName(qi);
            Assert.AreEqual(1, namesI.Count);
            Symbol I = namesI[0].Value.SemanticData;

            //Resolve variable J
            QualifiedName qj = new URI("J");
            var namesJ = cfg[0].ProcedureDivisionNode.SymbolTable.GetVariablesExplicitWithQualifiedName(qj);
            Assert.AreEqual(1, namesJ.Count);
            Symbol J = namesJ[0].Value.SemanticData;

            DefaultDataFlowGraphBuilder dfaBuilder = new DefaultDataFlowGraphBuilder(cfg[0]);
            dfaBuilder.ComputeKillSet();

            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[0].Data.Kill);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[3].Data.Kill);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[4].Data.Kill);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[5].Data.Kill);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[7].Data.Kill);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[8].Data.Kill);
            Assert.IsNull(dfaBuilder.Cfg.AllBlocks[10].Data.Kill);

            //----------------------------------------------------------------
            //checked that KILL(Block(1)) = "000111" : 
            //----------------------------------------------------------------
            Assert.IsNotNull(dfaBuilder.Cfg.AllBlocks[1].Data.Kill);            
            Assert.AreEqual("{3, 4, 5}", dfaBuilder.Cfg.AllBlocks[1].Data.Kill.ToString());
            //ADD 1 TO I : From Block3
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[1].Data.Kill.Get(3));            
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.MoveStatement, dfaBuilder.DefList[3].Instruction.CodeElement.Type);
            Assert.AreEqual(I, dfaBuilder.DefList[3].Variable);

            //ADD 1 TO J : From Block 6
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[1].Data.Kill.Get(4));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.AddStatement, dfaBuilder.DefList[4].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[4].Variable);

            //SUBSTRACT 4 FROM J : From block 9
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[1].Data.Kill.Get(5));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.SubtractStatement, dfaBuilder.DefList[5].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[5].Variable);

            //----------------------------------------------------------------
            //checked that KILL(Block(2)) = "1" : 
            //----------------------------------------------------------------
            Assert.IsNotNull(dfaBuilder.Cfg.AllBlocks[2].Data.Kill);
            //MOVE I TO J : From Block1
            Assert.AreEqual("{0}", dfaBuilder.Cfg.AllBlocks[2].Data.Kill.ToString());
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[2].Data.Kill.Get(0));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.MoveStatement, dfaBuilder.DefList[0].Instruction.CodeElement.Type);
            Assert.AreEqual(I, dfaBuilder.DefList[0].Variable);

            //----------------------------------------------------------------
            //checked that KILL(Block(6)) = "011001" : 
            //----------------------------------------------------------------
            Assert.IsNotNull(dfaBuilder.Cfg.AllBlocks[6].Data.Kill);            
            Assert.AreEqual("{1, 2, 5}", dfaBuilder.Cfg.AllBlocks[6].Data.Kill.ToString());
            //MOVE I TO J : From Block1
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[6].Data.Kill.Get(1));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.MoveStatement, dfaBuilder.DefList[1].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[1].Variable);

            //ADD 1 TO J : From Block1
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[6].Data.Kill.Get(2));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.AddStatement, dfaBuilder.DefList[2].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[2].Variable);

            //SUBSTRACT 4 FROM J : From block 9
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[6].Data.Kill.Get(5));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.SubtractStatement, dfaBuilder.DefList[5].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[5].Variable);

            //----------------------------------------------------------------
            //checked that KILL(Block(9)) = "01101" : 
            //----------------------------------------------------------------
            Assert.IsNotNull(dfaBuilder.Cfg.AllBlocks[9].Data.Kill);
            Assert.AreEqual("{1, 2, 4}", dfaBuilder.Cfg.AllBlocks[9].Data.Kill.ToString());
            //MOVE I TO J : From Block1
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[9].Data.Kill.Get(1));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.MoveStatement, dfaBuilder.DefList[1].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[1].Variable);

            //ADD 1 TO J : From Block1
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[9].Data.Kill.Get(2));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.AddStatement, dfaBuilder.DefList[2].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[2].Variable);

            //ADD 1 TO J : From Block6
            Assert.IsTrue(dfaBuilder.Cfg.AllBlocks[9].Data.Kill.Get(4));
            Assert.AreEqual(Compiler.CodeElements.CodeElementType.AddStatement, dfaBuilder.DefList[4].Instruction.CodeElement.Type);
            Assert.AreEqual(J, dfaBuilder.DefList[4].Variable);
        }

    }
}
