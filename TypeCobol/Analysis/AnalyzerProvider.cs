using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// IAnalyzerProvider default implementation. Can be used directly by adding activation delegates
    /// with the appropriate AddActivator method or by subclassing it.
    /// It can also be used to allow composition of multiple providers into one.
    /// This ensures that the creation of IAnalyzers won't interrupt the parsing.
    /// </summary>
    public class AnalyzerProviderWrapper : IAnalyzerProvider
    {
        private Action<string> _logger;
        protected Action<string> Logger
        {
            get => _logger ?? DoNothing;
            set => _logger = value;
        }
        private List<Func<TypeCobolOptions, TextSourceInfo, ISyntaxDrivenAnalyzer>> _sdaActivators;
        private List<Func<TypeCobolOptions, IQualityAnalyzer>> _qaActivators;
        private List<IAnalyzerProvider> _providers;

        public AnalyzerProviderWrapper(Action<string> logger = null)
        {
            _logger = logger;
        }

        /// <summary>
        /// Create an analyzer with the given method and catch potential exceptions thrown.
        /// </summary>
        /// <typeparam name="TAnalyzer">Type of IAnalyzer created.</typeparam>
        /// <param name="createAnalyzer">Function without parameter and returning an analyzer.</param>
        /// <returns>An analyzer if the creation is successful, null otherwise.</returns>
        private TAnalyzer SecureCreateAnalyzer<TAnalyzer>(Func<TAnalyzer> createAnalyzer) where TAnalyzer : IAnalyzer
        {
            try
            {
                return createAnalyzer();
            }
            catch (Exception exception)
            {
                Logger($"Failed to create analyzer of type {typeof(TAnalyzer).FullName}.{Environment.NewLine}{exception.GetType().FullName} has been thrown.{Environment.NewLine}{exception.Message}");
            }
            return default; // null
        }

        /// <summary>
        /// Create analyzers of the given type from providers and catch exceptions thrown.
        /// <remarks>If the creation of an analyzer fails, it will fail the creation of all analyzers for this particular provider but not for the others.</remarks>
        /// </summary>
        /// <typeparam name="TAnalyzer">Type of IAnalyzer created.</typeparam>
        /// <param name="createAnalyzers">Function to create analyzers from a provider.</param>
        /// <returns>Non-null list of created analyzers. Might be empty and can contain null values.</returns>
        private List<TAnalyzer> SecureCreateAnalyzersFromProviders<TAnalyzer>(Func<IAnalyzerProvider, TAnalyzer[]> createAnalyzers) where TAnalyzer : IAnalyzer
        {
            if (_providers == null) return null;

            var analyzers = new List<TAnalyzer>();
            foreach (var analyzerProvider in _providers)
            {
                Debug.Assert(analyzerProvider != null); // Because the adding method has NotNull
                // Either the whole array is created, or the whole array fails and there is no analyzers of TAnalyzer type from this provider
                try
                {
                    analyzers.AddRange(createAnalyzers(analyzerProvider));
                }
                catch (Exception exception)
                {
                    Logger($"Failed to create analyzers from analyzer provider {analyzerProvider.GetType().FullName}.{Environment.NewLine}{exception.GetType().FullName} has been thrown.{Environment.NewLine}{exception.Message}");
                }
            }
            return analyzers.Count == 0 ? null : analyzers;
        }

        /// <summary>
        /// Merge the results of the creation of analyzers.
        /// </summary>
        /// <typeparam name="TAnalyzer">Type of IAnalyzer to merge lists.</typeparam>
        /// <param name="list1">List of analyzers as base of the merge, can be null.</param>
        /// <param name="list2">Enumeration of analyzers, can be null.</param>
        /// <returns>Merged array of analyzers, may be empty, can be null if both parameters are null.</returns>
        private TAnalyzer[] MergeAnalyzers<TAnalyzer>(List<TAnalyzer> list1, IEnumerable<TAnalyzer> list2) where TAnalyzer : IAnalyzer
        {
            if (list1 == null && list2 == null)
            {
                return null;
            }

            if (list1 != null && list2 != null)
            {
                list1.AddRange(list2);
                return list1.ToArray();
            }

            if (list1 != null && list2 == null)
            {
                return list1.ToArray();
            }
            
            return list2.ToArray();
        }

        public IQualityAnalyzer[] CreateQualityAnalyzers([NotNull] TypeCobolOptions options)
        {
            var qaFromProviders = SecureCreateAnalyzersFromProviders(analyzerProvider => analyzerProvider.CreateQualityAnalyzers(options));

            var qaFromActivators = _qaActivators?
                .Select(qaActivator => SecureCreateAnalyzer(() => qaActivator(options)))
                .Where(qaAnalyzer => qaAnalyzer != null);

            return MergeAnalyzers(qaFromProviders, qaFromActivators);
        }

        public ISyntaxDrivenAnalyzer[] CreateSyntaxDrivenAnalyzers([NotNull] TypeCobolOptions options, [NotNull] TextSourceInfo textSourceInfo)
        {
            var sdaFromProviders = SecureCreateAnalyzersFromProviders(analyzerProvider => analyzerProvider.CreateSyntaxDrivenAnalyzers(options, textSourceInfo));

            var sdaFromActivators = _sdaActivators?
                .Select(sdaActivator => SecureCreateAnalyzer(() => sdaActivator(options, textSourceInfo)))
                .Where(sdaAnalyzer => sdaAnalyzer != null);

            return MergeAnalyzers(sdaFromProviders, sdaFromActivators);
        }

        /// <summary>
        /// Add an activation delegate to produce a new instance of ISyntaxDrivenAnalyzer.
        /// </summary>
        /// <param name="sdaActivator">Non-null Func delegate to create a new ISyntaxDrivenAnalyzer.</param>
        public void AddActivator([NotNull] Func<TypeCobolOptions, TextSourceInfo, ISyntaxDrivenAnalyzer> sdaActivator)
        {
            if (_sdaActivators == null)
            {
                _sdaActivators = new List<Func<TypeCobolOptions, TextSourceInfo, ISyntaxDrivenAnalyzer>>();
            }
            _sdaActivators.Add(sdaActivator);
        }

        /// <summary>
        /// Add an activation delegate to produce a new instance of IQualityAnalyzer.
        /// </summary>
        /// <param name="qaActivator">Non-null Func delegate to create a new IQualityAnalyzer.</param>
        public void AddActivator([NotNull] Func<TypeCobolOptions, IQualityAnalyzer> qaActivator)
        {
            if (_qaActivators == null)
            {
                _qaActivators = new List<Func<TypeCobolOptions, IQualityAnalyzer>>();
            }
            _qaActivators.Add(qaActivator);
        }

        /// <summary>
        /// Add an analyzer provider to this one.
        /// </summary>
        /// <param name="provider">Instance of IAnalyzerProvider.</param>
        public void AddProvider([NotNull] IAnalyzerProvider provider)
        {
            Debug.Assert(provider != null);
            if (_providers == null)
            {
                _providers = new List<IAnalyzerProvider>();
            }
            _providers.Add(provider);
        }

        /// <summary>
        /// Does nothing.
        /// </summary>
        /// <param name="str">Parameter to do nothing with.</param>
        private static void DoNothing(string str) { }
    }
}
