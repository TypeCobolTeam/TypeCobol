﻿using JetBrains.Annotations;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Describes a factory of analyzers.
    /// </summary>
    public interface IAnalyzerProvider
    {
        /// <summary>
        /// Triggers the creation of syntax-driven analyzers.
        /// </summary>
        /// <param name="options">Current TypeCobol compilation options.</param>
        /// <param name="textSourceInfo">Current source file being compiled.</param>
        /// <returns>An array of syntax-driven analyzers.</returns>
        /// <remarks>Called each time a file is being parsed. See <see cref="CompilationUnit.ProduceTemporarySemanticDocument"/> method.</remarks>
        ISyntaxDrivenAnalyzer[] CreateSyntaxDrivenAnalyzers([NotNull] TypeCobolOptions options, [NotNull] TextSourceInfo textSourceInfo);

        /// <summary>
        /// Triggers the creation of quality analyzers.
        /// </summary>
        /// <param name="options">Current TypeCobol compilation options.</param>
        /// <returns>An array of quality analyzers.</returns>
        /// <remarks>Called each time a quality check is requested. See <see cref="CompilationUnit.RefreshCodeAnalysisDocumentSnapshot"/> method.</remarks>
        IQualityAnalyzer[] CreateQualityAnalyzers([NotNull] TypeCobolOptions options);
    }
}
