using JetBrains.Annotations;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
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
        /// <remarks>Called each time a file is being parsed. See <see cref="ProgramClassParserStep.CupParseProgramOrClass"/> method.</remarks>
        ISyntaxDrivenAnalyzer[] CreateSyntaxDrivenAnalyzers([NotNull] TypeCobolOptions options, [NotNull] TextSourceInfo textSourceInfo);
    }
}
