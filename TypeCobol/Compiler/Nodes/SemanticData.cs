namespace TypeCobol.Compiler.Nodes
{
    /// <summary>
    /// Enumeration on the kind of semantic data hold by the SemanticData property if not null.
    /// </summary>
    public enum SemanticKinds
    {
        Type,
        Symbol
    }

    /// <summary>
    /// Interface of any semantic data
    /// </summary>
    public interface ISemanticData
    {
        /// <summary>
        /// Get The Kind of Semantic data
        /// </summary>
        SemanticKinds SemanticKind { get; }
    }
}
