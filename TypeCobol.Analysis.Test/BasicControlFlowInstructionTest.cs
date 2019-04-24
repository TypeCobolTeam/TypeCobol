using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Tools.Options_Config;
using TypeCobol.Compiler.Parser;
using TypeCobol.Tools.Options_Config;
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
using TypeCobol.Compiler.Types;
using Type = TypeCobol.Compiler.Types.Type;
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
            //In this case we have firts the nested programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 5);
            Assert.IsNull(CfgBuilder.ParentProgramCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNull(CfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder);
            Assert.IsNull(CfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder);
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
            //In this case we have firts the nested functions and programs cfg builders of the main program, followed by stacked program cfg builders.
            //Stack programs cfg builders have no parent cfg builders.
            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 7);
            Assert.IsNull(CfgBuilder.ParentProgramCfgBuilder);

            //Proc0
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[0].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[0].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("Proc0"));

            //Proc1
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[1].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[1].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[1].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[1].Cfg.ProgramNode.Name.Equals("Proc1"));

            //Nested0
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[2].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].Cfg.ProgramNode.Name.Equals("Nested0"));
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[2].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[2].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[2].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc0"));

            //Nested1
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[3].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].Cfg.ProgramNode.Name.Equals("Nested1"));
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[3].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc1"));

            //Nested2
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].ParentProgramCfgBuilder == CfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[4].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].Cfg.ProgramNode.Name.Equals("Nested2"));
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[4].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("NestedProc2"));

            //Stacked0
            Assert.IsNull(CfgBuilder.AllCfgBuilder[5].ParentProgramCfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].Cfg.ProgramNode.Name.Equals("Stacke0"));
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder.Count == 1);
            //StackedNestedProc0
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[5].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("StackedNestedProc0"));

            //Stacked1
            Assert.IsNull(CfgBuilder.AllCfgBuilder[6].ParentProgramCfgBuilder);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[6].Cfg.ProgramNode is Program);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[6].Cfg.ProgramNode.Name.Equals("Stacke1"));
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder.Count == 1);
            //StackedNestedProc1
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder[0].Cfg);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder[0].Cfg.ProgramNode is FunctionDeclaration);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder[6].AllCfgBuilder[0].Cfg.ProgramNode.Name.Equals("StackedNestedProc1"));
        }
    }
}
