using System.Collections.Generic;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// Declarative context class.
        /// </summary>
        private class DeclarativesContext
        {
            /// <summary>
            /// Current Block before all declaratives sections and paragraphs.
            /// </summary>
            private BasicBlockForNode _originBlock;

            /// <summary>
            /// All sections inside this Declaratives.
            /// </summary>
            private LinkedList<ControlFlowGraph<Node, D>.Section> _sections;

            /// <summary>
            /// The related CFG builder
            /// </summary>
            private ControlFlowGraphBuilder<D> _builder;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="currentProgramCfgBuilder">The related CFG Builder</param>
            internal DeclarativesContext(ControlFlowGraphBuilder<D> currentProgramCfgBuilder)
            {
                _sections = new LinkedList<ControlFlowGraph<Node, D>.Section>();
                _builder = currentProgramCfgBuilder;
            }

            /// <summary>
            /// Starts a newly created DeclarativesContext.
            /// </summary>
            /// <param name="originBlock">The starting block for this context.</param>
            internal void Start(BasicBlockForNode originBlock)
            {
                _originBlock = originBlock;
            }

            public void AddSection(ControlFlowGraph<Node, D>.Section section)
            {
                _sections.AddLast(section);
            }

            /// <summary>
            /// End the declaratives sections.
            /// Each Section becomes a branch from the current block.
            /// </summary>
            internal void End(BasicBlockForNode nextBlock)
            {
                System.Diagnostics.Debug.Assert(_builder != null);
                System.Diagnostics.Debug.Assert(_originBlock != null);
                System.Diagnostics.Debug.Assert(nextBlock != null);

                //First, origin block is linked to the next block.
                int nbIndex = _builder.Cfg.SuccessorEdges.Count;
                _builder.Cfg.SuccessorEdges.Add(nextBlock);
                _originBlock.SuccessorEdges.Add(nbIndex);

                //For each section, link the origin block to the first block of the section.
                bool bFirstsection = true;
                foreach (var section in _sections)
                {
                    foreach (var sentence in section)
                    {
                        //Ensure that every first block of the section is linked.
                        System.Diagnostics.Debug.Assert(sentence.FirstBlockIndex.HasValue);
                        int firstBlockIndex = sentence.FirstBlockIndex.Value;
                        if (bFirstsection)
                        {//The first block of the first section, should have been already linked to the origin Block.
                            System.Diagnostics.Debug.Assert(_originBlock.SuccessorEdges.Contains(firstBlockIndex));
                            if (!_originBlock.SuccessorEdges.Contains(firstBlockIndex))
                            {
                                _originBlock.SuccessorEdges.Add(firstBlockIndex);
                            }
                            bFirstsection = false;
                        }
                        else
                        {
                            _originBlock.SuccessorEdges.Add(firstBlockIndex);
                        }
                        break;//We only need the first sentence.
                    }
                }
            }
        }
    }
}