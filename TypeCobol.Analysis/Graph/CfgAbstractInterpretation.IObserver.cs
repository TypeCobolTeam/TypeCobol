using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    public partial class CfgAbstractInterpretation<N,D>
    {
        /// <summary>
        /// Abstract Interpretation Observer
        /// </summary>
        public interface IObserver
        {
            /// <summary>
            /// Called when a block is entered
            /// </summary>
            /// <param name="block"></param>
            /// <param name="env">The current execution environment</param>
            void EnterBlock(BasicBlock<N, D> block, Environment env);

            /// <summary>
            /// Called whena block is leaved
            /// </summary>
            /// <param name="block"></param>
            /// <param name="env">The current execution environment</param>
            void LeaveBlock(BasicBlock<N, D> block, Environment env);

            /// <summary>
            /// Called on an instruction inside a block
            /// </summary>
            /// <param name="instr">The instruction</param>
            /// <param name="block">The parent block</param>
            /// <param name="env">The current execution environment</param>
            void OnInstruction(N instr, BasicBlock<N, D> block, Environment env);
        }
    }
}
