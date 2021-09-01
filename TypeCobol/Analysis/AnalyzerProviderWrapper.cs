using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

using SyntaxDrivenAnalyzerActivator = System.Func<TypeCobol.Compiler.Directives.TypeCobolOptions, TypeCobol.Compiler.Text.TextSourceInfo, TypeCobol.Analysis.ISyntaxDrivenAnalyzer>;
using QualityAnalyzerActivator = System.Func<TypeCobol.Compiler.Directives.TypeCobolOptions, TypeCobol.Analysis.IQualityAnalyzer>;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// IAnalyzerProvider default implementation. Can be used directly by adding activation delegates
    /// with the appropriate AddActivator method or by subclassing it.
    /// It can also be used to allow composition of multiple providers into one.
    /// This class also ensures that the creation of IAnalyzers won't interrupt the parsing.
    /// </summary>
    public class AnalyzerProviderWrapper : IAnalyzerProvider
    {
        private static void DoNothing(string message) { /*Empty default logging method*/ }

        /// <summary>
        /// Method used to log exceptions.
        /// <remarks>Should not be used directly, use the accessor Logger instead.</remarks>
        /// </summary>
        private Action<string> _logger;

        /// <summary>
        /// Method used to log exceptions.
        /// </summary>
        protected Action<string> Logger
        {
            [NotNull]
            get => _logger;
            private set => _logger = (value ?? DoNothing);
        }

        private List<SyntaxDrivenAnalyzerActivator> _sdaActivators;
        private List<QualityAnalyzerActivator> _qaActivators;
        private List<IAnalyzerProvider> _providers;

        public AnalyzerProviderWrapper(Action<string> logger = null)
        {
            Logger = logger;
        }

        [NotNull]
        [ItemNotNull]
        private TAnalyzer[] SafeCreateAnalyzers<TAnalyzer>([NotNull] Func<IAnalyzerProvider, TAnalyzer[]> createFromProvider, [CanBeNull] IEnumerable<Func<TAnalyzer>> createFromActivators)
            where TAnalyzer : IAnalyzer
        {
            List<TAnalyzer> result = new List<TAnalyzer>();

            if (_providers != null)
            {
                foreach (var provider in _providers)
                {
                    TAnalyzer[] analyzers = null;
                    try
                    {
                        analyzers = createFromProvider(provider);
                    }
                    catch (Exception exception)
                    {
                        Logger($"Failed to create analyzers from provider {provider.GetType().FullName}.{Environment.NewLine}{exception.GetType().FullName} has been thrown.{Environment.NewLine}{exception.Message}");
                    }

                    if (analyzers != null)
                    {
                        result.AddRange(analyzers.Where(a => a != null));
                    }
                }
            }

            if (createFromActivators != null)
            {
                foreach (var createFromActivator in createFromActivators)
                {
                    try
                    {
                        var analyzer = createFromActivator();
                        if (analyzer != null) result.Add(analyzer);
                    }
                    catch (Exception exception)
                    {
                        Logger($"Failed to create analyzer of type {typeof(TAnalyzer).FullName}.{Environment.NewLine}{exception.GetType().FullName} has been thrown.{Environment.NewLine}{exception.Message}");
                    }
                }
            }

            return result.ToArray();
        }

        public ISyntaxDrivenAnalyzer[] CreateSyntaxDrivenAnalyzers([NotNull] TypeCobolOptions options, [NotNull] TextSourceInfo textSourceInfo)
        {
            var createFromActivators = _sdaActivators?.Select<SyntaxDrivenAnalyzerActivator, Func<ISyntaxDrivenAnalyzer>>(sdaActivator => () => sdaActivator(options, textSourceInfo));
            return SafeCreateAnalyzers(CreateFromProvider, createFromActivators);

            ISyntaxDrivenAnalyzer[] CreateFromProvider(IAnalyzerProvider provider) => provider.CreateSyntaxDrivenAnalyzers(options, textSourceInfo);
        }

        public IQualityAnalyzer[] CreateQualityAnalyzers([NotNull] TypeCobolOptions options)
        {
            var createFromActivators = _qaActivators?.Select<QualityAnalyzerActivator, Func<IQualityAnalyzer>>(qaActivator => () => qaActivator(options));
            return SafeCreateAnalyzers(CreateFromProvider, createFromActivators);

            IQualityAnalyzer[] CreateFromProvider(IAnalyzerProvider provider) => provider.CreateQualityAnalyzers(options);
        }

        /// <summary>
        /// Add an activation delegate to produce a new instance of ISyntaxDrivenAnalyzer.
        /// </summary>
        /// <param name="sdaActivator">Non-null Func delegate to create a new ISyntaxDrivenAnalyzer.</param>
        public void AddActivator([NotNull] SyntaxDrivenAnalyzerActivator sdaActivator)
        {
            if (_sdaActivators == null)
            {
                _sdaActivators = new List<SyntaxDrivenAnalyzerActivator>();
            }
            _sdaActivators.Add(sdaActivator);
        }

        /// <summary>
        /// Add an activation delegate to produce a new instance of IQualityAnalyzer.
        /// </summary>
        /// <param name="qaActivator">Non-null Func delegate to create a new IQualityAnalyzer.</param>
        public void AddActivator([NotNull] QualityAnalyzerActivator qaActivator)
        {
            if (_qaActivators == null)
            {
                _qaActivators = new List<QualityAnalyzerActivator>();
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
    }
}
