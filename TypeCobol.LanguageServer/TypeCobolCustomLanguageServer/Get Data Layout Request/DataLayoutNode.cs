using Newtonsoft.Json;

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
        /// <summary>
        /// UNDEFINED value possibly set for <c>index</c> field
        /// </summary>
        public const int UNDEFINED = -1;

        /// <summary>
        /// Logical level: +1 on parent one
        /// </summary>
        public int logicalLevel { get; set; }

        /// <summary>
        /// Line number (starting at 1)
        /// </summary>        
        public int line { get; set; }

        /// <summary>
        /// Level number
        /// </summary>        
        public long physicalLevel { get; set; }

        /// <summary>
        /// Name
        /// </summary>
        public string name { get; set; }

        /// <summary>
        /// Declaration (Picture, Usage, REDEFINES, OCCURS, ...)
        /// </summary>
        public string declaration { get; set; }

        /// <summary>
        /// +1 on parent one if OCCURS
        /// </summary>
        public int occursDimension { get; set; }

        /// <summary>
        /// Start position
        /// </summary>
        public long start { get; set; }

        /// <summary>
        /// Physical length
        /// </summary>
        public long length { get; set; }

        /// <summary>
        /// Copy directive (possibly null)
        /// </summary>
        public string copy { get; set; }

        /// <summary>
        /// Position in parent node's children (possibly set to UNDEFINED if not relevant)
        /// </summary>
        public int index { get; set; }

        /// <summary>
        /// Flags IsRedefines, Displayable, ...
        /// </summary>
        public DataLayoutNodeFlags flags { get; set; }

        /// <summary>
        /// Children nodes
        /// </summary>
        public List<DataLayoutNode> children { get; set; }

        /// <summary>
        /// Flag indicating whether the max OCCURS exceeds the max capacity of an index declared as PIC 9(4) COMP-5
        /// </summary>
        [JsonIgnore]
        public bool ExceedsMaxIndexCapacity { get; set; }

        /// <summary>
        /// Flag indicating whether the data is addressable in a DISPLAY statement (i.e is named or having at least one named parent)
        /// </summary>
        [JsonIgnore]
        public bool IsAdressable { get; set; }
    }
}