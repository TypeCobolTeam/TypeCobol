namespace TypeCobol.LanguageServer.Commands.InsertVariableDisplay
{
    /// <summary>
    /// Abstraction for objects that generate Cobol code.
    /// </summary>
    internal interface ICobolCodeProvider
    {
        /// <summary>
        /// True when this provider has content to write, False when empty.
        /// </summary>
        bool HasContent { get; }

        /// <summary>
        /// Write the entirety of generated code to the given builder.
        /// </summary>
        /// <param name="cobolStringBuilder">Non-null CobolStringBuilder instance.</param>
        void WriteCobolCode(CobolStringBuilder cobolStringBuilder);
    }
}
