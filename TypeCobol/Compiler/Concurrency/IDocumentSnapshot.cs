namespace TypeCobol.Compiler.Concurrency
{
    /// <summary>
    /// Represents an immutable snapshot of a specific version of a document.
    /// </summary>
    /// <remarks>
    /// Documents snapshots enable one thread to consume information concurrently produced by another thread.
    /// </remarks>
    public interface IDocumentSnapshot<T>
    {
        /// <summary>
        /// Document version identifier for this snapshot
        /// </summary>
        DocumentVersion<T> CurrentVersion { get; }

        /// <summary>
        /// Previous document version for this snapshot
        /// </summary>
        DocumentVersion<T> PreviousVersion { get; }

        /// <summary>
        /// Immutable list of document lines
        /// </summary>
        ISearchableReadOnlyList<T> Lines { get; }
    }

    /// <summary>
    /// Represents an immutable snapshot for the first document in the compilation pipeline.
    /// </summary>
    public interface IFirstStepDocumentSnapshot<TSourceLines, TCurrentStep> : IDocumentSnapshot<TCurrentStep>
    {
        /// <summary>
        /// Document version identifier for the list of source lines used as a basis to compute the current step
        /// </summary>
        DocumentVersion<TSourceLines> SourceLinesVersion { get; }
    }

    public interface INextStepDocumentSnapshot<T>
    { 
        /// <summary>
        /// Snapshot of the document at the previous step used as a basis to compute the current step
        /// </summary>
        IDocumentSnapshot<T> PreviousStepSnapshot { get; }
    }

    /// <summary>
    /// Represents an immutable snapshot for the following documents in the compilation pipeline.
    /// </summary>
    public interface ICompilerStepDocumentSnapshot<TPreviousStep,TCurrentStep> : IDocumentSnapshot<TCurrentStep>, INextStepDocumentSnapshot<TPreviousStep>
    { }    
}
