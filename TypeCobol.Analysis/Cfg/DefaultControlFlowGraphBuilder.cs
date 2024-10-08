﻿using TypeCobol.Compiler.Directives;

namespace TypeCobol.Analysis.Cfg
{
    /// <summary>
    /// Default Control Flow Graph Builder with any object has Data.
    /// </summary>
    public class DefaultControlFlowGraphBuilder<D> : ControlFlowGraphBuilder<D>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="identifier">String identifier of this analyzer-builder.</param>
        /// <param name="compilerOptions">Compiler options.</param>
        /// <param name="extendPerformTargets">True to extend the blocks targeted by PERFORM statements.</param>
        /// <param name="useEvaluateCascade">True to convert EVALUATE statements into cascaded-IFs.</param>
        /// <param name="useSearchCascade">True to convert SEARCH statements into cascaded-IFs.</param>
        public DefaultControlFlowGraphBuilder(string identifier, TypeCobolOptions compilerOptions, bool extendPerformTargets = false, bool useEvaluateCascade = true, bool useSearchCascade = true)
            : base(identifier, compilerOptions, extendPerformTargets, useEvaluateCascade, useSearchCascade)
        {

        }
    }
}
