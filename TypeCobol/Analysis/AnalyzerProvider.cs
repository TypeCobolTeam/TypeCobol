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

        public virtual ISyntaxDrivenAnalyzer[] CreateSyntaxDrivenAnalyzers(TypeCobolOptions options, TextSourceInfo textSourceInfo)
        {
            return _sdaActivators?.Select(sdaActivator => sdaActivator(options, textSourceInfo)).ToArray();
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
    }
}
