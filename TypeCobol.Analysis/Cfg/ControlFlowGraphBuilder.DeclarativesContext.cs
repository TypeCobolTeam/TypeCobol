using System.Collections.Generic;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// Declarative context class.
        /// </summary>
        internal class DeclarativesContext
        {
            /// <summary>
            /// Current Block before all declaratives sections and paragraphs.
            /// </summary>
            internal BasicBlockForNode CurrentBlock;

            /// <summary>
            /// All sections inside this Declaratives.
            /// </summary>
            internal LinkedList<CfgSectionSymbol> Sections;

            /// <summary>
            /// The related CFG builder
            /// </summary>
            internal ControlFlowGraphBuilder<D> Builder;

            /// <summary>
            /// Constructor
            /// </summary>
            /// <param name="currentProgramCfgBuilder">The related CFG Builder</param>
            internal DeclarativesContext(ControlFlowGraphBuilder<D> currentProgramCfgBuilder)
            {
                Sections = new LinkedList<CfgSectionSymbol>();
                Builder = currentProgramCfgBuilder;
            }

            /// <summary>
            /// 
            /// </summary>
            /// <param name="currentBlock"></param>
            internal void Start(BasicBlockForNode currentBlock)
            {
                CurrentBlock = currentBlock;
            }
            internal void AddSection(CfgSectionSymbol section)
            {
                Sections.AddLast(section);
            }

            /// <summary>
            /// End the declaratives sections.
            /// Each Section becomes a branch from the current block.
            /// </summary>
            internal void End(BasicBlockForNode nextBlock)
            {
                System.Diagnostics.Debug.Assert(Builder != null);
                System.Diagnostics.Debug.Assert(CurrentBlock != null);
                System.Diagnostics.Debug.Assert(nextBlock != null);

                //First Current block is linked to the next block.
                int nbIndex = Builder.Cfg.SuccessorEdges.Count;
                Builder.Cfg.SuccessorEdges.Add(nextBlock);
                CurrentBlock.SuccessorEdges.Add(nbIndex);

                //For each section, link the current block to the first block of the section.
                bool bFirstsection = true;
                foreach (var section in Sections)
                {
                    var sentences = Builder.YieldSectionOrParagraphSentences(section);
                    foreach (var sentence in sentences)
                    {
                        //Ensure that every first block of the section is linked.
                        System.Diagnostics.Debug.Assert(sentence.BlockIndex >= 0);
                        if (sentence.BlockIndex >= 0)
                        {
                            var block = sentence.Block;
                            if (bFirstsection)
                            {//The first block of the first section, should have been already linked to the Current Block.
                                System.Diagnostics.Debug.Assert(CurrentBlock.SuccessorEdges.Contains(sentence.BlockIndex));
                                if (!CurrentBlock.SuccessorEdges.Contains(sentence.BlockIndex))
                                {
                                    CurrentBlock.SuccessorEdges.Add(sentence.BlockIndex);
                                }
                                bFirstsection = false;
                            }
                            else
                            {
                                CurrentBlock.SuccessorEdges.Add(sentence.BlockIndex);
                            }
                        }
                        break;//We only need the first sentence.
                    }
                }
            }
        }
    }
}