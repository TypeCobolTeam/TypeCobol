using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Tools.Options_Config;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
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
        public static TypeCobolConfiguration DefaultConfig = null;
        public static ProgramSymbolTableBuilder Builder = null;
        public static NodeListenerFactory BuilderNodeListenerFactory = null;
        public static string DefaultIntrinsicPath = null;

        public static DefaultControlFlowGraphBuilder CfgBuilder;

        [TestInitialize]
        public void TestInitialize()
        {
            //Create a default configurations for options
            DefaultConfig = new TypeCobolConfiguration();
            if (File.Exists(DefaultIntrinsicPath))
            {
                DefaultConfig.Copies.Add(DefaultIntrinsicPath);
            }

            //DefaultConfig.Dependencies.Add(Path.Combine(Directory.GetCurrentDirectory(), "resources", "dependencies"));
            SymbolTableBuilder.Config = DefaultConfig;

            //Force the creation of the Global Symbol Table
            var global = SymbolTableBuilder.Root;

            //Allocate a static Program Symbol Table Builder
            BuilderNodeListenerFactory = () =>
            {
                Builder = new ProgramSymbolTableBuilder();
                return Builder;
            };
            NodeDispatcher.RegisterStaticNodeListenerFactory(BuilderNodeListenerFactory);

            //Alocate a static Default Control Flow Graph Builder
            BuilderNodeListenerFactory = () =>
            {
                CfgBuilder = new DefaultControlFlowGraphBuilder();
                return CfgBuilder;
            };
            NodeDispatcher.RegisterStaticNodeListenerFactory(BuilderNodeListenerFactory);
        }

        private static void RemovePrograms(ProgramSymbol prog)
        {
            foreach (var nestPrg in prog.Programs)
            {
                SymbolTableBuilder.Root.RemoveProgram(prog);
                RemovePrograms(nestPrg);
            }
            SymbolTableBuilder.Root.RemoveProgram(prog);
        }

        [TestCleanup]
        public void TestCleanup()
        {
            if (BuilderNodeListenerFactory != null)
            {
                NodeDispatcher.RemoveStaticNodeListenerFactory(BuilderNodeListenerFactory);
                if (Builder.Programs.Count != 0)
                {
                    foreach (var prog in Builder.Programs)
                    {
                        RemovePrograms(prog);
                    }
                }
            }
        }


        /// <summary>
        /// This is a simple test to ensure that given a Cobol program,
        /// all staked and nested program are capured.
        /// </summary>
        [TestMethod]
        public void MixedStackedNestedPgmsTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "MixedStackedNestedPgms.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Here we have the main program, followed by stacked programs.
            Assert.IsTrue(Builder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 6);
            Assert.IsNull(CfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[0] == CfgBuilder);
            Assert.IsNull(CfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNull(CfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder);
            Assert.IsNull(CfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder);
        }

        /// <summary>
        /// This is a simple test to ensure that given a Cobol program,
        /// all staked and nested program are capured.
        /// </summary>
        [TestMethod]
        public void MixedStackedNestedProcsPgmsTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "MixedStackedNestedProcsPgms.tcbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            //Here we have the main program, followed by stacked programs.
            Assert.IsTrue(Builder.Programs.Count == 3);
            //var currentProgram = Builder.Programs[0];
            //var mainProgram = document.Results.ProgramClassDocumentSnapshot.Root.MainProgram;
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);
            //In this case we have first the Main program cfg, followed by the nested functions and programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 8);
            Assert.IsNull(CfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[0] == CfgBuilder);
            Assert.IsNull(CfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder);

            //Proc0
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[1].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[1].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[1].Cfg.ProgramNode.Name.Equals("Proc0"));

            //Proc1
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[2].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].Cfg.ProgramNode.Name.Equals("Proc1"));

            //Nested0
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[3].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].Cfg.ProgramNode.Name.Equals("Nested0"));
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc0"));

            //Nested1
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[4].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].Cfg.ProgramNode.Name.Equals("Nested1"));
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc1"));

            //Nested2
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[5].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].Cfg.ProgramNode.Name.Equals("Nested2"));
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc2"));

            //Stacked0
            Assert.IsNull(CfgBuilder.AllCfgBuilder[6].ParentProgramCfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[6].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[6].Cfg.ProgramNode.Name.Equals("Stacke0"));
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder.Count == 1);
            //StackedNestedProc0
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder[0].Cfg);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("StackedNestedProc0"));

            //Stacked1
            Assert.IsNull(CfgBuilder.AllCfgBuilder[7].ParentProgramCfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[7].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[7].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[7].Cfg.ProgramNode.Name.Equals("Stacke1"));
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[7].AllCfgBuilder.Count == 1);
            //StackedNestedProc1
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[7].AllCfgBuilder[0].Cfg);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[7].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[7].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("StackedNestedProc1"));
        }

        [TestMethod]
        public void IfThenTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThen0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThen0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }
        [TestMethod]
        public void IfThenAfterTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenAfter0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenAfter0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElseTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElse0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElse0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNextSentence()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenNextSentence0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenNextSentence0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNextSentence1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNextSentence1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNextSentence1.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNextSentence2()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNextSentence2.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNextSentence2.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElseNextSentence()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNextSentence0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNextSentence0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNested0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenNested0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenNested0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNested1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenNested1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenNested1.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenNested2()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenNested2.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenNested2.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElseNested0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNested0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNested0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void IfThenElseNested1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseNested1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseNested1.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }
        
        [TestMethod]
        public void IfThenElseCascade0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThenElseCascade0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThenElseCascade0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }


        [TestMethod]
        public void SimpleGotoPara0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "SimpleGotoPara0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "SimpleGotoPara0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void ComplexGotoPara0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "ComplexGotoPara0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "ComplexGotoPara0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void Evaluate0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "Evaluate0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "Evaluate0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void EvaluateNoOther0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "EvaluateNoOther0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "EvaluateNoOther0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void EvaluateMultiWhen0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "EvaluateMultiWhen0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "EvaluateMultiWhen0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void GoBack0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "GoBack0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "GoBack0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

        [TestMethod]
        public void GoBack1()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "GoBack1.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "GoBack1.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath);
        }

    }
}

