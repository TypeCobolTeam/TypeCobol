﻿using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Symbols;
using System.IO;
using TypeCobol.Compiler.CodeElements.Expressions;

using static TypeCobol.Analysis.Test.CfgTestUtils;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests for UseDef set building.
    /// </summary>
    [TestClass]
    public class DfaUseDefSetTest
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
        public void USE_DEF_SET_SampleGotos0()
        {
            string path = Path.Combine(BasicDfaSamples, "SampleGotos0.cbl");
            var dfaResults = ParseCompareDiagnosticsWithDfa(path);
            Assert.IsTrue(dfaResults.Graphs.Count == 1);

            var cfg = dfaResults.Graphs[0];
            //------------------------------------
            // All definitions
            // d(0) : MOVE 2 TO I
            // d(1) : MOVE 1 TO J
            // d(2) : ADD 1 TO J
            // d(3) : MOVE 1 TO I
            // d(4) : ADD 1 TO J
            // d(5) : SUBTRACT 4 FROM J
            //-----------------------------------

            //Resolve variable I
            QualifiedName qi = new URI("I");
            var namesI = cfg.ProcedureDivisionNode.SymbolTable.GetVariablesExplicitWithQualifiedName(qi);
            Assert.AreEqual(1, namesI.Count);
            Assert.IsNotNull(namesI[0]);
            Assert.IsNotNull(namesI[0].Value);
            Assert.IsNotNull(namesI[0].Value.SemanticData);
            Assert.IsTrue(namesI[0].Value.SemanticData is VariableSymbol);
            var I = (VariableSymbol) namesI[0].Value.SemanticData;

            //Resolve variable J
            QualifiedName qj = new URI("J");
            var namesJ = cfg.ProcedureDivisionNode.SymbolTable.GetVariablesExplicitWithQualifiedName(qj);
            Assert.AreEqual(1, namesJ.Count);
            Assert.IsNotNull(namesJ[0]);
            Assert.IsNotNull(namesJ[0].Value);
            Assert.IsNotNull(namesJ[0].Value.SemanticData);
            Assert.IsTrue(namesJ[0].Value.SemanticData is VariableSymbol);
            var J = (VariableSymbol) namesJ[0].Value.SemanticData;

            var variableDefMap = dfaResults.GetVariableDefMap(cfg);
            //Check Var I definitions
            Assert.AreEqual("{0, 3}", variableDefMap[I].ToString());
            //Check Var J definitions
            Assert.AreEqual("{1, 2, 4, 5}", variableDefMap[J].ToString());

            var useList = dfaResults.GetUseList(cfg);
            //--------------------------
            // VAR : I
            // Block(1)
            //--------------------------
            Assert.AreEqual(1, useList[0].BlockIndex);
            Assert.AreEqual(I, useList[0].Variable);
            Assert.AreEqual("{0}", useList[0].UseDef.ToString());

            //--------------------------
            // VAR : J
            // Block(2)
            //--------------------------
            Assert.AreEqual(2, useList[1].BlockIndex);
            Assert.AreEqual(J, useList[1].Variable);
            Assert.AreEqual("{2, 4, 5}", useList[1].UseDef.ToString());

            //--------------------------
            // VAR : J
            // Block(6)
            //--------------------------
            Assert.AreEqual(6, useList[2].BlockIndex);
            Assert.AreEqual(J, useList[2].Variable);
            Assert.AreEqual("{4}", useList[2].UseDef.ToString());
        }
    }
}
