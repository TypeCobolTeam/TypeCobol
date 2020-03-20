using System;
using JetBrains.Annotations;
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
        /// <param name="validationError">Instance of <see cref="ValidationError" /> to report.</param>
        void Report(ValidationError validationError);
    }

    /// <summary>
    /// Describes an error that occured while manipulating the semantic domain.
    /// </summary>
    public class ValidationError
    {
        /// <summary>
        /// Symbol concerned by the error.
        /// </summary>
        [NotNull]
        public Symbol InvalidSymbol { get; }

        /// <summary>
        /// Error message.
        /// </summary>
        [NotNull]
        public string Message { get; }

        /// <summary>
        /// Optional exception object detailing the error. 
        /// </summary>
        public Exception Exception { get; }

        public ValidationError([NotNull] Symbol invalidSymbol, [NotNull] string message, Exception exception = null)
        {
            System.Diagnostics.Debug.Assert(invalidSymbol != null);
            System.Diagnostics.Debug.Assert(message != null);
            InvalidSymbol = invalidSymbol;
            Message = message;
            Exception = exception;
        }
    }
}
