using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// BasicBlock which instruction are Nodes.
        /// </summary>
        /// <typeparam name="D"></typeparam>
        public class BasicBlockForNode : BasicBlock<Node, D>
        {
	        /// <summary>
            /// Set whether full instruction must be generated are not.
            /// If not only the instruction name will be generated.
            /// </summary>
            public bool FullInstruction
            {
                get;
                set;
            }

            protected override string InstructionToString(Node instruction)
            {
                return (instruction == null || instruction.CodeElement == null) ? "<null>" :
                    FullInstruction ? instruction.CodeElement.SourceText :
                    System.Enum.GetName(typeof(CodeElementType), instruction.CodeElement.Type);
            }
        }
    }
}