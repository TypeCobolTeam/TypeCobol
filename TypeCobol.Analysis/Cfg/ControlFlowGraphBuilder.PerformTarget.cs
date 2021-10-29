using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using SectionNode = TypeCobol.Compiler.Nodes.Section;

namespace TypeCobol.Analysis.Cfg
{
    public partial class ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// PerfomrTarget class to capture target sentences and procedures from a PERFORM instruction.
        /// </summary>
        private class PerformTarget
        {
            /// <summary>
            /// All target sentences
            /// </summary>
            internal List<Sentence> Sentences { get; private set; }
            /// <summary>
            /// All target procedures.
            /// </summary>
            internal List<Procedure> Procedures { get; private set; }

            public PerformTarget(List<Sentence> sentences, List<Procedure> procedures)
            {
                this.Sentences = sentences;
                this.Procedures = procedures;
            }
        }

        /// <summary>
        /// Cache of PerformTarget instances already calculated for a Perform procedure 
        /// </summary>
        private Dictionary<PerformProcedure, PerformTarget> _performTargetCache;
        private PerformTarget ComputePerformTarget(PerformProcedure p, SectionNode sectionNode)
        {
            if (_performTargetCache == null)
                _performTargetCache = new Dictionary<PerformProcedure, PerformTarget>();
            if (_performTargetCache.TryGetValue(p, out var target)) {
                return target;
            }

            SymbolReference procedureReference = p.CodeElement.Procedure;
            SymbolReference throughProcedureReference = p.CodeElement.ThroughProcedure;

            Node procedureNode = ResolveProcedure(p, sectionNode, procedureReference);
            if (procedureNode == null)
                return null;

            Procedure procedure = _nodeToProcedure[procedureNode];
            List<Sentence> sentences = new List<Sentence>();
            List<Procedure> procedures = new List<Procedure>();
            if (throughProcedureReference != null)
            {
                Node throughProcedureNode = ResolveProcedure(p, sectionNode, throughProcedureReference);
                if (throughProcedureNode == null)
                    return null;
                
                Procedure throughProcedure = _nodeToProcedure[throughProcedureNode];
                if (procedure.Number > throughProcedure.Number)
                {
                    // the second procedure name is declared before the first one.
                    this.Cfg.AddWrongOrderPerformThru(p, procedureNode, throughProcedureNode);
                    return null;
                }

                //Accumulate sentences located between the two procedures
                int n = 0;
                int currentProcedureNumber = procedure.Number;
                while (currentProcedureNumber <= throughProcedure.Number)
                {
                    var currentProcedure = this.CurrentProgramCfgBuilder.AllProcedures[currentProcedureNumber];
                    currentProcedure.AccumulateSentencesThrough(sentences, throughProcedure, out var lastProcedure);
                    currentProcedureNumber = lastProcedure.Number + 1;
                    procedures.Add(currentProcedure);
                    while (n < sentences.Count)
                    {
                        sentences[n++].Procedure = currentProcedure;
                    }
                }
            } else 
            {
                procedures.Add(procedure);
                foreach (var sentence in procedure)
                {
                    sentences.Add(sentence);
                    sentence.Procedure = procedure;                    
                }
            }
            target = new PerformTarget(sentences, procedures);
            _performTargetCache[p] = target;
            return target;
        }
    }
}
