namespace TypeCobol.LanguageServer
{
    [Flags]
    public enum DataLayoutNodeFlags
    {
        None = 0,
        IsRedefines = 1,
        Displayable = 2 // Can be used in DISPLAY statement
    }

    /// <summary>
    /// Node gathering information for GetDataLayoutRequest with TREE outputType
    /// </summary>
    public class DataLayoutNode
    {
        // Logical level: +1 on parent one
        public int logicalLevel { get; set; }

        // Line number (starting at 1)
        public int line { get; set; }

        // Level number
        public long physicalLevel { get; set; }

        // Name
        public string name { get; set; }

        // Declaration (Picture, Usage, REDEFINES, OCCURS, ...)
        public string declaration { get; set; }

        // +1 on parent one if OCCURS
        public int occursDimension { get; set; }

        // Start position
        public long start { get; set; }

        // Physical length
        public long length { get; set; }

        // Copy directive (possibly null)
        public string copy { get; set; }

        // Position in parent node's chidren
        public int index { get; set; }

        // Flags IsRedefines, Displayable, ...
        public DataLayoutNodeFlags flags { get; set; }

        public DataLayoutNode[] children { get; set; }
    }
}