namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Represents a collection of [completion items](#CompletionItem) to be
    /// presented in the editor.
    /// </summary>
    public class CompletionList
    {
        /// <summary>
        /// This list is not complete. Further typing should result in recomputing
        /// this list.
        ///
        /// Recomputed lists have all their items replaced (not appended) in the
        /// incomplete completion sessions.
        /// </summary>
        public bool isIncomplete { get; set; }

        /// <summary>
        /// The completion items
        /// </summary>
        public List<CompletionItem> items { get; set; }
    }
}
