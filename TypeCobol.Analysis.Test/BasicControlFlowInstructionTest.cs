using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using System.IO;
using TypeCobol.Analysis.Cfg;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests for control from instructions.
    /// </summary>
    [TestClass]
    public class BasicControlFlowInstructionTest
    {
        private NodeListenerFactory _cfgBuilderNodeListenerFactory;

        private DefaultControlFlowGraphBuilder _cfgBuilder;

        [TestInitialize]
        public void TestInitialize()
        {
            //Allocate a static Default Control Flow Graph Builder
            _cfgBuilderNodeListenerFactory = () =>
            {
                _cfgBuilder = new DefaultControlFlowGraphBuilder();
                return _cfgBuilder;
            };
            NodeDispatcher.RegisterStaticNodeListenerFactory(_cfgBuilderNodeListenerFactory);
        }

        [TestCleanup]
        public void TestCleanup()
        {
            if (_cfgBuilderNodeListenerFactory != null)
            {
                NodeDispatcher.RemoveStaticNodeListenerFactory(_cfgBuilderNodeListenerFactory);
            }
        }

        /// <summary>
        /// This is a simple test to ensure that given a Cobol program,
        /// all staked and nested program are captured.
        /// </summary>
        [TestMethod]
        public void MixedStackedNestedPgms()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "MixedStackedNestedPgms.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 6);
            Assert.IsNull(_cfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[0] == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder);
        }

        /// <summary>
        /// This is a simple test to ensure that given a Cobol program,
        /// all staked and nested program are captured.
        /// </summary>
        [TestMethod]
        public void MixedStackedNestedProcsPgms()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "MixedStackedNestedProcsPgms.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested functions and programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 8);
            Assert.IsNull(_cfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[0] == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);

            //Proc0
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[1].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].Cfg.ProgramNode.Name.Equals("Proc0"));

            //Proc1
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[2].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].Cfg.ProgramNode.Name.Equals("Proc1"));

            //Nested0
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[3].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].Cfg.ProgramNode is Program);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].Cfg.ProgramNode.Name.Equals("Nested0"));
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc0"));

            //Nested1
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[4].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].Cfg.ProgramNode is Program);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].Cfg.ProgramNode.Name.Equals("Nested1"));
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc1"));

            //Nested2
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[5].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].Cfg.ProgramNode is Program);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].Cfg.ProgramNode.Name.Equals("Nested2"));
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc2"));

            //Stacked0
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[6].ParentProgramCfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[6].AllCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[6].Cfg.ProgramNode is Program);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[6].Cfg.ProgramNode.Name.Equals("Stacke0"));
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[6].AllCfgBuilder.Count == 1);
            //StackedNestedProc0
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[6].AllCfgBuilder[0].Cfg);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[6].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[6].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("StackedNestedProc0"));

            //Stacked1
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[7].ParentProgramCfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[7].AllCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[7].Cfg.ProgramNode is Program);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[7].Cfg.ProgramNode.Name.Equals("Stacke1"));
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[7].AllCfgBuilder.Count == 1);
            //StackedNestedProc1
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[7].AllCfgBuilder[0].Cfg);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[7].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[7].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("StackedNestedProc1"));
        }

        [TestMethod]
        public void IfThen0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThen0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThen0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }
        [TestMethod]
        public void IfThenAfter0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenAfter0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenAfter0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElse0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElse0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElse0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNextSentence0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenNextSentence0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenNextSentence0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElseNextSentence1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNextSentence1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNextSentence1.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElseNextSentence2()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNextSentence2.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNextSentence2.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElseNextSentence0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNextSentence0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNextSentence0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNested0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenNested0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenNested0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNested1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenNested1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenNested1.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNested2()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenNested2.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenNested2.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElseNested0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNested0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNested0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElseNested1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNested1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNested1.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }
        
        [TestMethod]
        public void IfThenElseCascade0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseCascade0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseCascade0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }


        [TestMethod]
        public void SimpleGotoPara0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "SimpleGotoPara0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "SimpleGotoPara0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void ComplexGotoPara0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "ComplexGotoPara0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "ComplexGotoPara0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void Evaluate0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "Evaluate0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Evaluate0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void EvaluateNoOther0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "EvaluateNoOther0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "EvaluateNoOther0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void EvaluateMultiWhen0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "EvaluateMultiWhen0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "EvaluateMultiWhen0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void Alter0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "Alter0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Alter0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void Alter1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "Alter1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Alter1.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        /// <summary>
        /// This ALTER test include qualified paragraph names.
        /// </summary>
        [TestMethod]
        public void Alter2()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "Alter2.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Alter2.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void GoBack0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "GoBack0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "GoBack0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void GoBack1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "GoBack1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "GoBack1.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void Perform0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "Perform0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Perform0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void PerformUntil0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformUntil0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformUntil0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void PerformUntil1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformUntil1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformUntil1.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }
        
        [TestMethod]
        public void PerformTime0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformTime0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformTime0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void PerformVarying0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformVarying0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformVarying0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void PerformProcedure0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformProcedure0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformProcedure0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void PerformProcedure1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformProcedure1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformProcedure1.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void PerformNested0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformNested0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformNested0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void Search0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "Search0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Search0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void SearchNextSentence0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "SearchNextSentence0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "SearchNextSentence0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void SearchCond0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "SearchCond0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "SearchCond0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void Declaratives0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "Declaratives0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Declaratives0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void Declaratives1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "Declaratives1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Declaratives1.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void PerformThru0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformThru0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformThru0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void PerformThru1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformThru1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformThru1.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void MixPerformEvaluateIf0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "MixPerformEvaluateIf0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "MixPerformEvaluateIf0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void PerformProcRecursive0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformProcRecursive0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformProcRecursive0.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void CfgInNestedPrg0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "CfgInNestedPrg0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 6);
            Assert.IsNull(_cfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[0] == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder);

            //We have taken the same CFG than for IfThenElseCascade0    
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseCascade0.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[2].Cfg, path, expectedPath);
        }

        [TestMethod]
        public void CfgInNestedPrg1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "CfgInNestedPrg1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 6);
            Assert.IsNull(_cfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[0] == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder);

            //We have taken the same CFG than for PerformProcedure0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformProcedure0.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[3].Cfg, path, expectedPath);
        }

        [TestMethod]
        public void CfgInNestedPrg2()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "CfgInNestedPrg2.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 6);
            Assert.IsNull(_cfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[0] == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder);

            //We have taken the same CFG than for MixPerformEvaluateIf0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "MixPerformEvaluateIf0.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[1].Cfg, path, expectedPath);
        }


        [TestMethod]
        public void CfgInStackedPrg0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "CfgInStackedPrg0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 6);
            Assert.IsNull(_cfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[0] == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder);

            //We have taken the same CFG than for PerformProcedure0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformProcedure0.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[4].Cfg, path, expectedPath);
        }


        [TestMethod]
        public void CfgInStackedPrg1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "CfgInStackedPrg1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 6);
            Assert.IsNull(_cfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[0] == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder);

            //We have taken the same CFG than for MixPerformEvaluateIf0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "MixPerformEvaluateIf0.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[5].Cfg, path, expectedPath);
        }

        [TestMethod]
        public void CfgInProcedure0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "CfgInProcedure0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested functions and programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 8);
            Assert.IsNull(_cfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[0] == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);

            //Proc0
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[1].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[1].Cfg.ProgramNode.Name.Equals("Proc0"));

            //We have taken the same CFG than for IfThenElseCascade0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseCascade0.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[1].Cfg, path, expectedPath);
        }

        [TestMethod]
        public void CfgInProcedure1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "CfgInProcedure1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            //Here we have the main program, followed by stacked programs.
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested functions and programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 8);
            Assert.IsNull(_cfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[0] == _cfgBuilder);
            Assert.IsNull(_cfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);

            //Proc1
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[2].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[2].Cfg.ProgramNode.Name.Equals("Proc1"));

            //We have taken the same CFG than for ComplexGotoPara0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "ComplexGotoPara0.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[2].Cfg, path, expectedPath);
        }


        [TestMethod]
        public void CfgInNestedProcedure0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "CfgInNestedProcedure0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            //Nested0
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[3].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].Cfg.ProgramNode is Program);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].Cfg.ProgramNode.Name.Equals("Nested0"));
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc0"));

            //Nested1
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[4].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].Cfg.ProgramNode is Program);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].Cfg.ProgramNode.Name.Equals("Nested1"));
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc1"));

            //Nested2
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder == _cfgBuilder);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[5].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].Cfg.ProgramNode is Program);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].Cfg.ProgramNode.Name.Equals("Nested2"));
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc2"));

            //We have taken the same CFG than for ComplexGotoPara0  
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "ComplexGotoPara0.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg, path, expectedPath);

            //We have taken the same CFG than for IfThenElseCascade0  
            expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseCascade0.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg, path, expectedPath);

            //We have taken the same CFG than for PerformThru1  
            expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "PerformThru1.dot");
            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg, path, expectedPath);
        }

        /// <summary>
        /// "dot.exe" -Tpng CGM110.dot -o CGM110.png
        /// "dot.exe" -Tsvg CGM110.dot -o CGM110.svg
        /// </summary>
        [TestMethod]
        public void OneThirdPartyCGM110()
        {
            string pwd = Directory.GetCurrentDirectory();
            string solutionPath = Directory.GetParent(pwd)?.Parent?.FullName;
            DirectoryInfo solDir = new DirectoryInfo(solutionPath);
            DirectoryInfo rootSolDir = solDir.Parent;

            string samples = @"ThirdParty" + Path.DirectorySeparatorChar + "CNAF" + Path.DirectorySeparatorChar + "Batch";

            string path = Path.Combine(solDir.FullName, "TypeCobol.Test", samples, "CGM110.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "CGM110.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath, false);
        }

        /// <summary>
        /// This test contains a PERFORM instruction to a PARAGRAPH that contains a GOTO to another
        /// Paragraph, there is an Diagnostic which is Raised.
        /// </summary>
        [TestMethod]
        [Ignore] //Result file needs update
        public void OneThirdPartyIX105A()
        {
            string pwd = Directory.GetCurrentDirectory();
            string solutionPath = Directory.GetParent(pwd)?.Parent?.FullName;
            DirectoryInfo solDir = new DirectoryInfo(solutionPath);
            DirectoryInfo rootSolDir = solDir.Parent;

            string samples = @"ThirdParty" + Path.DirectorySeparatorChar + "Nist";

            string path = Path.Combine(solDir.FullName, "TypeCobol.Test", samples, "IX105A.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Nist", "IX105A.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath, true);
        }

        /// <summary>
        /// This test contains a PERFORM instruction to a PARAGRAPH that contains a GOTO to another
        /// Paragraph, with Block recursion detection, there is an Diagnostic which is Raised.
        /// </summary>
        [TestMethod]
        [Ignore] //Result file needs update
        public void OneThirdPartySG102A()
        {
            string pwd = Directory.GetCurrentDirectory();
            string solutionPath = Directory.GetParent(pwd)?.Parent?.FullName;
            DirectoryInfo solDir = new DirectoryInfo(solutionPath);
            DirectoryInfo rootSolDir = solDir.Parent;

            string samples = @"ThirdParty" + Path.DirectorySeparatorChar + "Nist";

            string path = Path.Combine(solDir.FullName, "TypeCobol.Test", samples, "SG102A.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Nist", "SG102A.dot");

            Assert.IsTrue(_cfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(_cfgBuilder.Cfg, path, expectedPath, true);
        }

        /// <summary>
        /// This Test is only used to generate all .dot files corresponding to the Nist source samples.
        /// This dot files contains full instructions source code
        /// </summary>
        [TestMethod]
        [Ignore] //Long execution time
        public void GenAllNistSrcDots()
        {
            string pwd = Directory.GetCurrentDirectory();
            string solutionPath = Directory.GetParent(pwd)?.Parent?.FullName;
            DirectoryInfo solDir = new DirectoryInfo(solutionPath);
            DirectoryInfo rootSolDir = solDir.Parent;

            string samples = @"ThirdParty" + Path.DirectorySeparatorChar + "Nist";

            string path = Path.Combine(solDir.FullName, "TypeCobol.Test", samples);
            string[] files = Directory.GetFiles(path, "*.cbl", SearchOption.AllDirectories);
            foreach(string f in files)
            {
                string dotFile = f.Substring(0, f.LastIndexOf('.')) + ".dot";
                FileInfo fofi = new FileInfo(dotFile);
                string dotName = fofi.Name;

                Directory.CreateDirectory(Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Nist"));
                string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Nist", dotName);

                var document = TypeCobol.Parser.Parse(f, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                    false, /*copies*/ null);

                Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

                CfgTestUtils.GenDotCfgFile(_cfgBuilder.Cfg, expectedPath);

                TestCleanup();
                TestInitialize();
            }
        }

        /// <summary>
        /// This Test is only used to generate all .dot files corresponding to the Nist source samples.
        /// This dot files contains instructions names only.
        /// </summary>
        [TestMethod]
        [Ignore] //Long execution time
        public void GenAllNistDots()
        {
            string pwd = Directory.GetCurrentDirectory();
            string solutionPath = Directory.GetParent(pwd)?.Parent?.FullName;
            DirectoryInfo solDir = new DirectoryInfo(solutionPath);
            DirectoryInfo rootSolDir = solDir.Parent;

            string samples = @"ThirdParty" + Path.DirectorySeparatorChar + "Nist";

            string path = Path.Combine(solDir.FullName, "TypeCobol.Test", samples);
            string[] files = Directory.GetFiles(path, "*.cbl", SearchOption.AllDirectories);
            foreach (string f in files)
            {
                string dotFile = f.Substring(0, f.LastIndexOf('.')) + ".dot";
                FileInfo fofi = new FileInfo(dotFile);
                string dotName = "_" + fofi.Name;

                Directory.CreateDirectory(Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Nist"));
                string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Nist", dotName);

                var document = TypeCobol.Parser.Parse(f, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                    false, /*copies*/ null);

                Assert.IsNotNull(_cfgBuilder.AllCfgBuilder);

                CfgTestUtils.GenDotCfgFile(_cfgBuilder.Cfg, expectedPath, false);

                TestCleanup();
                TestInitialize();
            }
        }
    }
}
