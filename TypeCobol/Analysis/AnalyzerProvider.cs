using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// IAnalyzerProvider default implementation. Can be used directly by adding activation delegates
    /// with the appropriate AddActivator method or by subclassing it.
    /// </summary>
    public class AnalyzerProvider : IAnalyzerProvider
    {
        private List<Func<TypeCobolOptions, TextSourceInfo, ISyntaxDrivenAnalyzer>> _sdaActivators;
        private List<Func<TypeCobolOptions, IASTAnalyzer>> _astaActivators;

        public virtual ISyntaxDrivenAnalyzer[] CreateSyntaxDrivenAnalyzers(TypeCobolOptions options, TextSourceInfo textSourceInfo)
        {
            return _sdaActivators?
                .Select(sdaActivator => sdaActivator(options, textSourceInfo))
                .Where(sda => sda != null)
                .ToArray();
        }

        public virtual IASTAnalyzer[] CreateASTAnalyzers(TypeCobolOptions options)
        {
            return _astaActivators?
                .Select(astaActivator => astaActivator(options))
                .Where(asta => asta != null)
                .ToArray();
        }

        /// <summary>
        /// Add an activation delegate to produce a new instance of ISyntaxDrivenAnalyzer.
        /// </summary>
        /// <param name="activator">Non-null Func delegate to create a new ISyntaxDrivenAnalyzer.</param>
        public void AddActivator([NotNull] Func<TypeCobolOptions, TextSourceInfo, ISyntaxDrivenAnalyzer> activator)
        {
            if (_sdaActivators == null)
            {
                _sdaActivators = new List<Func<TypeCobolOptions, TextSourceInfo, ISyntaxDrivenAnalyzer>>();
            }
            _sdaActivators.Add(activator);
        }

        /// <summary>
        /// Add an activation delegate to produce a new instance of IASTAnalyzer.
        /// </summary>
        /// <param name="activator">Non-null Func delegate to create a new IASTAnalyzer.</param>
        public void AddActivator([NotNull] Func<TypeCobolOptions, IASTAnalyzer> activator)
        {
            if (_astaActivators == null)
            {
                _astaActivators = new List<Func<TypeCobolOptions, IASTAnalyzer>>();
            }
            _astaActivators.Add(activator);
        }
    }
}
