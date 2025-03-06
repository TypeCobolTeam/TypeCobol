using Newtonsoft.Json;

namespace TypeCobol.LanguageServer
{
    [Flags]
    public enum DataLayoutNodeFlags
    {
        None = 0,
        IsRedefines = 1,
        Displayable = 2, // Can be used in DISPLAY statement
        Generated = 4 // Generated 01 = virtual node built by the parser
    }

    /// <summary>
    /// Node gathering information for GetDataLayoutRequest with TREE outputType
    /// </summary>
    public class DataLayoutNode
    {
        // Matching data value / index
        private const int INDEX_LOGICAL_LEVEL = 0;
        private const int INDEX_LINE = 1;
        private const int INDEX_PHYSICAL_LEVEL = 2;
        private const int INDEX_NAME = 3;
        private const int INDEX_DECLARATION = 4;
        private const int INDEX_OCCURS_DIMENSION = 5;
        private const int INDEX_START = 6;
        private const int INDEX_LENGTH = 7;
        private const int INDEX_COPY = 8;
        private const int INDEX_INDEX = 9;
        private const int INDEX_FLAGS = 10;

        /// <summary>
        /// UNDEFINED value possibly set for <c>index</c> field
        /// </summary>
        private const int UNDEFINED = -1;

        public DataLayoutNode()
        {
            dataValues = new object[INDEX_FLAGS + 1];
            LogicalLevel = 0;
            Line = 0;
            PhysicalLevel = 0;
            OccursDimension = 0;
            Start = 0;
            Length = 0;
            Index = UNDEFINED;
            Flags = DataLayoutNodeFlags.None;
        }

        /// <summary>
        /// Logical level: +1 on parent one
        /// </summary>
        [JsonIgnore]
        public int LogicalLevel { get => (int)dataValues[INDEX_LOGICAL_LEVEL]; set => dataValues[INDEX_LOGICAL_LEVEL] = value; }

        /// <summary>
        /// Line number (starting at 1)
        /// </summary>        
        [JsonIgnore]
        public int Line { get => (int)dataValues[INDEX_LINE]; set => dataValues[INDEX_LINE] = value; }

        /// <summary>
        /// Level number
        /// </summary>        
        [JsonIgnore]
        public long PhysicalLevel { get => (long)dataValues[INDEX_PHYSICAL_LEVEL]; set => dataValues[INDEX_PHYSICAL_LEVEL] = value; }

        /// <summary>
        /// Name
        /// </summary>
        [JsonIgnore]
        public string Name { get => (string)dataValues[INDEX_NAME]; set => dataValues[INDEX_NAME] = value; }

        /// <summary>
        /// Declaration (Picture, Usage, REDEFINES, OCCURS, ...)
        /// </summary>
        [JsonIgnore]
        public string Declaration { get => (string)dataValues[INDEX_DECLARATION]; set => dataValues[INDEX_DECLARATION] = value; }

        /// <summary>
        /// +1 on parent one if OCCURS
        /// </summary>
        [JsonIgnore]
        public int OccursDimension { get => (int)dataValues[INDEX_OCCURS_DIMENSION]; set => dataValues[INDEX_OCCURS_DIMENSION] = value; }

        /// <summary>
        /// Start position
        /// </summary>
        [JsonIgnore]
        public long Start { get => (long)dataValues[INDEX_START]; set => dataValues[INDEX_START] = value; }

        /// <summary>
        /// Physical length
        /// </summary>
        [JsonIgnore]
        public long Length { get => (long)dataValues[INDEX_LENGTH]; set => dataValues[INDEX_LENGTH] = value; }

        /// <summary>
        /// Copy directive (possibly null)
        /// </summary>
        [JsonIgnore] 
        public string Copy { get => (string)dataValues[INDEX_COPY]; set => dataValues[INDEX_COPY] = value; }

        /// <summary>
        /// Position in parent node's children (possibly set to UNDEFINED if not relevant)
        /// </summary>
        [JsonIgnore]
        public int Index { get => (int)dataValues[INDEX_INDEX]; set => dataValues[INDEX_INDEX] = value; }

        /// <summary>
        /// Flags IsRedefines, Displayable, ...
        /// </summary>
        [JsonIgnore]
        public DataLayoutNodeFlags Flags { get => (DataLayoutNodeFlags)dataValues[INDEX_FLAGS]; set => dataValues[INDEX_FLAGS] = value; }

        /// <summary>
        /// Flag indicating whether the max OCCURS exceeds the max capacity of an index declared as PIC 9(4) COMP-5
        /// </summary>
        [JsonIgnore]
        public bool ExceedsMaxIndexCapacity { get; set; }

        /// <summary>
        /// Flag indicating whether the data is addressable in a DISPLAY statement (i.e is named or having at least one named parent)
        /// </summary>
        [JsonIgnore]
        public bool IsAddressable { get; set; }

        /// <summary>
        /// Array gathering all data values (to make Json more compact).
        /// Here is the matching between the array index and the data value:
        /// 0 = LogicalLevel
        /// 1 = Line
        /// 2 = PhysicalLevel
        /// 3 = Name
        /// 4 = Declaration
        /// 5 = OccursDimension
        /// 6 = Start
        /// 7 = Length
        /// 8 = Copy
        /// 9 = Index
        /// 10 = Flags
        /// </summary>
        public object[] dataValues { get; set; }

        /// <summary>
        /// Children nodes
        /// </summary>
        public List<DataLayoutNode> children { get; set; }
    }
}