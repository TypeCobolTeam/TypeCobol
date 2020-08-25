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


namespace TypeCobol.Analysis
{
    /// <summary>
    /// A Context for Cfg/Dfa construction.
    /// </summary>
    public class CfgDfaContext
    {
        /// <summary>
        /// TypeCobolConfiguration to use
        /// </summary>
        public TypeCobolConfiguration DefaultConfig
        {
            get;
            private set;
        }
        public NodeListenerFactory CfgBuilderNodeListenerFactory = null;
        //A Cfg for Control Flow Analysis on Node
        public DefaultControlFlowGraphBuilder<object> CfgBuilder;
        //A Cfg for Data Flow Analysis
        public DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>> CfgDfaBuilder;

        /// <summary>
        /// Context mode
        /// </summary>
        public enum Mode
        {
            None = 0,//No Cfg or Dfa only Program Symol Table Builder
            Cfg,    //Only building Cfg in normale mode without block expansion
            CfgExpand,  //Only building Cfg with block expansion 
            CfgExplicit,  //Iteration link are explicitly created. 
            Dfa         //Dfa mode
        }

        /// <summary>
        /// Testing mode
        /// </summary>
        public Mode _Mode
        {
            get;
            set;
        }
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="mode">Testing mode</param>
        public CfgDfaContext(Mode mode)
        {
            _Mode = mode;
        }

        /// <summary>
        /// Initialize the context
        /// </summary>
        /// <param name="config">The TypeCobol Configuration to be used</param>
        public void Initialize(TypeCobolConfiguration config = null)
        {
            //Alocate a static Default Control Flow Graph Builder
            switch (_Mode)
            {
                case Mode.Cfg:
                case Mode.CfgExpand:
                case Mode.CfgExplicit:
                    //Alocate a static Default Control Flow Graph Builder
                    CfgBuilderNodeListenerFactory = () =>
                    {
                        CfgBuilder = new DefaultControlFlowGraphBuilder<object>();
                        CfgBuilder.Mode = _Mode == Mode.Cfg 
                            ? ControlFlowGraphBuilder<object>.CfgMode.Normal 
                            : (_Mode == Mode.CfgExplicit ?  ControlFlowGraphBuilder<object>.CfgMode.Explicit : ControlFlowGraphBuilder<object>.CfgMode.Extended) ;
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
        /// Cleanup the context
        /// </summary>
        public void Cleanup()
        {
            if (CfgBuilderNodeListenerFactory != null)
            {
                NodeDispatcher.RemoveStaticNodeListenerFactory(CfgBuilderNodeListenerFactory);
            }
        }
    }
}
