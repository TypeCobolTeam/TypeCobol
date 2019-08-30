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

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// Test Context for Cfg Dfa tests
    /// </summary>
    public class CfgDfaTestContext
    {
        public TypeCobolConfiguration DefaultConfig = null;
        public ProgramSymbolTableBuilder Builder = null;
        public NodeListenerFactory BuilderNodeListenerFactory = null;
        public NodeListenerFactory CfgBuilderNodeListenerFactory = null;
        public string DefaultIntrinsicPath = null;
        //A Cfg for Control Flow Analysis on Node
        public DefaultControlFlowGraphBuilder<object> CfgBuilder;
        //A Cfg for Data Flow Analysis
        public DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>> CfgDfaBuilder;

        public enum Mode
        {
            Cfg = 1,    //Only building Cfg in normale mode without block expansion
            CfgExpand,  //Only building Cfg with block expansion 
            Dfa         //Dfa Test mode
        }

        /// <summary>
        /// Testing mode
        /// </summary>
        private Mode _Mode
        {
            get;
            set;
        }
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mode">Testing mode</param>
        public CfgDfaTestContext(Mode mode)
        {
            _Mode = mode;
        }

        /// <summary>
        /// Initialize the test session
        /// </summary>
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
            switch(_Mode)
            {
                case Mode.Cfg:
                case Mode.CfgExpand:
                    //Alocate a static Default Control Flow Graph Builder
                    CfgBuilderNodeListenerFactory = () =>
                    {
                        CfgBuilder = new DefaultControlFlowGraphBuilder<object>();
                        CfgBuilder.Mode = _Mode == Mode.Cfg ? ControlFlowGraphBuilder<object>.CfgMode.Normal : ControlFlowGraphBuilder<object>.CfgMode.Extended;
                        return CfgBuilder;
                    };
                    break;
                case Mode.Dfa:
                    CfgBuilderNodeListenerFactory = () =>
                    {
                        CfgDfaBuilder = new DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>>();
                        CfgDfaBuilder.Mode = ControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>>.CfgMode.Extended;
                        return CfgDfaBuilder;
                    };
                    break;
            }
            NodeDispatcher.RegisterStaticNodeListenerFactory(CfgBuilderNodeListenerFactory);
        }

        /// <summary>
        /// Remove all programs.
        /// </summary>
        /// <param name="prog">The main program to be removed</param>
        private static void RemovePrograms(ProgramSymbol prog)
        {
            foreach (var nestPrg in prog.Programs)
            {
                SymbolTableBuilder.Root.RemoveProgram(prog);
                RemovePrograms(nestPrg);
            }
            SymbolTableBuilder.Root.RemoveProgram(prog);
        }

        /// <summary>
        /// Cleanup the Test
        /// </summary>
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
            if (CfgBuilderNodeListenerFactory != null)
            {
                NodeDispatcher.RemoveStaticNodeListenerFactory(CfgBuilderNodeListenerFactory);
            }
        }
    }
}
