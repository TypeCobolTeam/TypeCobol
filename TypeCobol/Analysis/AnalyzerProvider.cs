using System;
using System.Collections.Generic;
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
        /// <returns>Non-null array of created analyzers. Might be empty and can contain null values.</returns>
        private TAnalyzer[] SecureCreateAnalyzersFromProviders<TAnalyzer>(Func<IAnalyzerProvider, TAnalyzer[]> createAnalyzers) where TAnalyzer : IAnalyzer
        {
            var analyzers = new List<TAnalyzer>();
            foreach (var analyzerProvider in _providers)
            {
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
            return analyzers.ToArray();
        }

        public IQualityAnalyzer[] CreateQualityAnalyzers([NotNull] TypeCobolOptions options)
        {
            var qaFromProviders = SecureCreateAnalyzersFromProviders(analyzerProvider => analyzerProvider.CreateQualityAnalyzers(options));

            return _qaActivators
                .Select(qaActivator => SecureCreateAnalyzer(() => qaActivator(options)))
                .Concat(qaFromProviders) // Add analyzers from providers
                .Where(qaAnalyzer => qaAnalyzer != null)
                .ToArray();
        }

        public ISyntaxDrivenAnalyzer[] CreateSyntaxDrivenAnalyzers([NotNull] TypeCobolOptions options, [NotNull] TextSourceInfo textSourceInfo)
        {
            var sdaFromProviders = SecureCreateAnalyzersFromProviders(analyzerProvider => analyzerProvider.CreateSyntaxDrivenAnalyzers(options, textSourceInfo));

            return _sdaActivators
                .Select(sdaActivator => SecureCreateAnalyzer(() => sdaActivator(options, textSourceInfo)))
                .Concat(sdaFromProviders) // Add analyzers from providers
                .Where(sdaAnalyzer => sdaAnalyzer != null)
                .ToArray();
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
