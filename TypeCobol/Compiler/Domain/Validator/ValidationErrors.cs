using System;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Compiler.Domain.Validator
{
    /// <summary>
    /// Interface describing an external semantic error reporter.
    /// </summary>
    public interface IValidationErrorReporter
    {
        /// <summary>
        /// This method is meant to be called while operating on types or symbols to signal an error.
        /// </summary>
        /// <param name="invalidSymbol">Symbol concerned by the error.</param>
        /// <param name="message">Error message.</param>
        /// <param name="exception">Optional exception object detailing the error.</param>
        void Report(Symbol invalidSymbol, string message, Exception exception);
    }
}
