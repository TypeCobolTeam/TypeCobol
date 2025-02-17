using System.Diagnostics;
using System.Text;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Nodes {

    public class DataDivision: GenericNode<DataDivisionHeader>, Parent<DataSection> {

        public const string NODE_ID = "data-division";

        /// <summary>
        /// Lists indices for data sections
        /// </summary>
        private enum SectionIndex
        {
            FileSection = 0,
            GlobalStorageSection,
            WorkingStorageSection,
            LocalStorageSection,
            LinkageSection
        }
        // The 5 (optional) data sections
        private readonly DataSection[] _sections = new DataSection[5];
        public FileSection FileSection => (FileSection) _sections[(int)SectionIndex.FileSection];
        public GlobalStorageSection GlobalStorageSection => (GlobalStorageSection)_sections[(int)SectionIndex.GlobalStorageSection];
        public WorkingStorageSection WorkingStorageSection => (WorkingStorageSection)_sections[(int)SectionIndex.WorkingStorageSection];
        public LocalStorageSection LocalStorageSection => (LocalStorageSection)_sections[(int)SectionIndex.LocalStorageSection];
        public LinkageSection LinkageSection => (LinkageSection)_sections[(int)SectionIndex.LinkageSection];

        public DataDivision(DataDivisionHeader header): base(header) { }
        public override string ID { get { return NODE_ID; } }

        public override void Add(Node child, int index = -1)
        {
            var maxSectionIndex = UpdateSection(child);
            if (index <= 0) index = WhereShouldIAdd(maxSectionIndex);

            base.Add(child,index);
        }

        private int UpdateSection(Node node)
        {
            int result = node switch
            {
                Nodes.FileSection => (int)SectionIndex.FileSection,
                Nodes.GlobalStorageSection => (int)SectionIndex.GlobalStorageSection,
                Nodes.WorkingStorageSection => (int)SectionIndex.WorkingStorageSection,
                Nodes.LocalStorageSection => (int)SectionIndex.LocalStorageSection,
                Nodes.LinkageSection => (int)SectionIndex.LinkageSection,
                _ => -1
            };

            if (result != -1)
            {
                _sections[result] = (DataSection)node;
            }

            return result;
        }

        private int WhereShouldIAdd(int maxSectionIndex)
        {
            // If maxSectionIndex = -1, _sections is not enumerated and Takes returns an empty IEnumerable so Count is 0
            return _sections.Take(maxSectionIndex).Count(s => s != null);
        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

        public abstract class DataSection: GenericNode<DataSectionHeader>, Child<DataDivision>{
        protected DataSection(DataSectionHeader header): base(header) { }
        public virtual bool IsShared { get { return false; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class FileSection: DataSection {
        public FileSection(FileSectionHeader header): base(header) { }
        public override string ID { get { return "file"; } }
        public override bool IsShared { get { return true; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }

    public class FileDescription : DataDefinition {
        public FileDescription(FileDescriptionEntry entry): base(entry) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
   } 

    public class GlobalStorageSection : DataSection, Parent<DataDefinition>
    {
        public GlobalStorageSection(GlobalStorageSectionHeader header) : base(header) { }
        public override string ID { get { return "global-storage"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }


    public class WorkingStorageSection: DataSection, Parent<DataDefinition>
    {
        public WorkingStorageSection(WorkingStorageSectionHeader header) : base(header) { }

        public override IEnumerable<ITextLine> Lines
        {
            get
            {
                List<ITextLine> lines = new List<ITextLine>();
                lines.Add(new TextLineSnapshot(-1, CodeElement.SourceText, null));

                if (IsFlagSet(Flag.InsideProcedure))
                {
                    var declare = Parent?.Parent as FunctionDeclaration;
                    if (declare != null)
                    {
                        lines.Add(new TextLineSnapshot(-1,
                            string.Format("*{0}.{1} {2}", declare.Root.MainProgram.Name, declare.Name,
                                declare.Profile.Parameters.Count != 0 ? "- Params :" : " - No Params"), null));
                        lines.AddRange(declare.Profile.GetSignatureForComment());
                    }
                }

                return lines;
            }
        }
        public override string ID { get { return "working-storage"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    public class LocalStorageSection: DataSection, Parent<DataDefinition>
        {
        public LocalStorageSection(LocalStorageSectionHeader header): base(header) { }

        public override IEnumerable<ITextLine> Lines
        {
            get
            {
                List<ITextLine> lines = new List<ITextLine>();
                lines.Add(new TextLineSnapshot(-1, CodeElement.SourceText, null));

                if (IsFlagSet(Flag.InsideProcedure))
                {
                    var declare = Parent?.Parent as FunctionDeclaration;
                    if (declare != null)
                    {
                        lines.Add(new TextLineSnapshot(-1,
                            string.Format("*{0}.{1} {2}", declare.Root.MainProgram.Name, declare.Name,
                                declare.Profile.Parameters.Count != 0 ? "- Params :" : " - No Params"), null));
                        lines.AddRange(declare.Profile.GetSignatureForComment());
                    }
                }

                return lines;
            }
        }
        public override string ID { get { return "local-storage"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    public class LinkageSection: DataSection, Parent<DataDefinition>
    {
        public LinkageSection(LinkageSectionHeader header): base(header) { }

        public override IEnumerable<ITextLine> Lines
        {
            get
            {
                List<ITextLine> lines = new List<ITextLine>();
                lines.Add(new TextLineSnapshot(-1, CodeElement.SourceText, null));

                if (IsFlagSet(Flag.InsideProcedure))
                {
                    var declare = Parent?.Parent as FunctionDeclaration;
                    if (declare != null)
                    {
                        lines.Add(new TextLineSnapshot(-1,
                            string.Format("*{0}.{1} {2}", declare.Root.MainProgram.Name, declare.Name,
                                declare.Profile.Parameters.Count != 0 ? "- Params :" : " - No Params"), null));
                        lines.AddRange(declare.Profile.GetSignatureForComment());
                    }
                }

                return lines;
            }
        }
        public override string ID { get { return "linkage"; } }
        public override bool IsShared { get { return true; } }

        public override bool VisitNode(IASTVisitor astVisitor) {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }

    /// <summary>
    /// Node                            -> CodeElement
    /// ------------------------------------------------------------------------------
    /// DataDefinition                  -> DataDefinitionEntry
    ///   IndexDefinition               -> DataDefinitionEntry
    ///   GeneratedDefinition           -> DataDefinitionEntry
    ///                                         CommonDataDescriptionAndDataRedefines
    ///   DataDescription 01-49 & 77    ->          DataDescriptionEntry
    ///      ParameterDescription       ->          ParameterDescriptionEntry
    ///      TypedDataNode              ->          DataDescriptionEntry
    ///   TypeDefinition                ->          DataTypeDescriptionEntry
    ///   DataRedefines                 ->          DataRedefinesEntry
    ///   DataCondition   88            ->      DataConditionEntry
    ///   DataRenames     66            ->      DataRenamesEntry
    ///   FileDescriptionEntryNode      ->      FileDescriptionEntry
    ///     
    /// 
    /// Implementation note:
    /// DataDefinition doesn't inherits from GenericNode. See explanation on property InternalDataDefinitionEntry
    /// </summary>
    public abstract class DataDefinition: Node, Parent<DataDefinition>
    {
        protected DataDefinition([CanBeNull] DataDefinitionEntry dataDefinitionEntry)
        {
            this.CodeElement = dataDefinitionEntry;
        }

        [CanBeNull]
        private CommonDataDescriptionAndDataRedefines _CommonDataDesc { get { return this.CodeElement as CommonDataDescriptionAndDataRedefines; } }


        protected override CodeElement InternalCodeElement => CodeElement;

        /// <summary>
        /// IndexDefinition and GeneratedDefinition don't have a CodeElement.
        /// Otherwise all others DataDefinition must have a CodeElement
        /// </summary>
        [CanBeNull]
        public new DataDefinitionEntry CodeElement { get; }



        public override string ID { get { return "data-definition"; } }

        public override string Name=> this.CodeElement.Name;

        private Dictionary<StorageArea, Node> _References;

        public void AddReferences(StorageArea storageArea, Node node)
        {
            if(_References == null)
                _References = new Dictionary<StorageArea, Node>();

            if (!_References.ContainsKey(storageArea))
                _References.Add(storageArea, node);
        }

        public Dictionary<StorageArea, Node> GetReferences()
        {
            return _References ?? (_References = new Dictionary<StorageArea, Node>());
        }

        private TypeDefinition _typeDefinition;

        /// <summary>
        /// Get the TypeDefinition node associated to this Node
        /// </summary>
        public TypeDefinition TypeDefinition
        {
            get { return _typeDefinition; }
            set
            {
                //Implementation note : Only TypeCobolLinker should set this value
                if (_typeDefinition == null)
                    _typeDefinition = value;
            }
        }

        private IList<DataRedefines> _dataRedefines;

        public IEnumerable<DataRedefines> DataRedefinitions => _dataRedefines;

        public void AddDataRedefinition(DataRedefines dataRedefines)
        {
            if (_dataRedefines == null)
            {
                _dataRedefines = new List<DataRedefines>();
            }

            _dataRedefines.Add(dataRedefines);
        }

        public DataRedefines GetBiggestRedefines()
        {
            //Order the list so the redefines that take the highest amout of memory is the top
            var redefines = _dataRedefines?.OrderByDescending(dr => dr.PhysicalLength).First();

            //Recurse if this redefines a redefine
            return redefines?.GetBiggestRedefines() ?? redefines;
        }

        public override bool VisitNode(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }

        private DataType _dataType;
        public virtual DataType DataType
        {
            get
            {
                if (_dataType != null) return _dataType;

                _dataType = this.CodeElement != null ?  this.CodeElement.DataType : DataType.Unknown;
                return _dataType;
            }
        }

        [CanBeNull]
        private DataType _primitiveDataType;

        [CanBeNull]
        public virtual DataType PrimitiveDataType
        {
            get
            {
                if (_primitiveDataType != null) return _primitiveDataType;
                //If it's not a Typedef
                if (_CommonDataDesc?.DataType.CobolLanguageLevel == CobolLanguageLevel.Cobol85)
                {
                    _primitiveDataType = _CommonDataDesc.DataType;
                }
                else //It's a Typedef, its DataType doesn't reflect its picture or its usage but the Typedef
                {
                    if (_CommonDataDesc?.PictureValidationResult != null)
                    {
                        _primitiveDataType = DataType.Create(_CommonDataDesc.PictureValidationResult);
                    }
                    else
                    {
                        var dataUsage = Usage;
                        if (dataUsage.HasValue)
                            _primitiveDataType = DataType.Create(dataUsage.Value);
                    }
                }
                
                return _primitiveDataType;
            }
        }

        /// <summary>
        /// PhysicalLength is the size taken by a DataDefinition and its children in memory
        /// </summary>
        private long _physicalLength = -1;
        public virtual long PhysicalLength
        {
            get
            {
                if (_physicalLength != -1)
                {
                    return _physicalLength;
                }

                if (children != null)
                {
                    if(Picture != null || (Usage != null && Usage != DataUsage.None && Children.Count == 0))
                    {
                        _physicalLength = GetPhysicalLength();
                    }
                    else if (CodeElement?.Type == CodeElementType.DataConditionEntry)
                    {
                        //Exception case if this is a level 88
                        _physicalLength = 0;
                    }
                    else if (DataType == DataType.Boolean)
                    {
                        //exception case if this is a type bool
                        _physicalLength = 1;
                    }
                    else if (TypeDefinition != null)
                    {
                        _physicalLength = TypeDefinition.PhysicalLength;
                    }
                    else
                    {
                        if (_physicalLength == -1)
                            _physicalLength = 0;

                        //Sum up the physical lengths of the children of the current node.
                        foreach (var node in children)
                        {
                            var dataDefinition = (DataDefinition)node;
                            
                            //The highest amount of memory taken between a node and its redefinition is always the size taken in memory
                            if (dataDefinition is DataRedefines == false)
                            {
                                var redefines = dataDefinition.GetBiggestRedefines();
                                _physicalLength += Math.Max(redefines?.PhysicalLength + redefines?.SlackBytes ?? 0, dataDefinition.PhysicalLength + dataDefinition.SlackBytes);
                            }
                        }
                    }
                    
                }

                if (MaxOccurencesCount > 1)
                {
                    _physicalLength = _physicalLength * MaxOccurencesCount;
                }

                return _physicalLength != -1 ? _physicalLength : 1;
            }
        }

        /// <summary>
        /// Gets the actual size of one DataDefinition
        /// </summary>
        /// <returns></returns>
        private long GetPhysicalLength()
        {
            var usage = Usage.HasValue ? Types.Type.DataUsage2UsageFormat(Usage.Value) : Types.Type.UsageFormat.None;
            
            if (Picture == null)
            {
                if (Usage != null && Usage.Value != DataUsage.None)
                {
                    return new Types.Type(Types.Type.Tags.Usage, usage).Length;
                }
                return 1;
            }

            System.Diagnostics.Debug.Assert(PictureValidationResult != null);
            if (PictureValidationResult.IsValid)
            {
                PictureType type = new PictureType(PictureValidationResult, SignIsSeparate);
                type.Usage = usage;
                return type.Length;
            }
            
            return 1;
        }

        /// <summary>
        /// A SlackByte is a unit that is used to synchronize DataDefinitions in memory.
        /// One or more will be present only if the keyword SYNC is placed on a DataDefinition
        /// To calculated the number of slackbytes present :
        /// - Calculate the size of all previous DataDefinitions to the current one
        /// - The number of SlackBytes inserted will be determined by the formula m - (occupiedMemory % m) where m is determined by the usage of the DataDefinition
        /// - Whether it will be put before or after the size of the DataDefinition is determined by the keyword LEFT or RIGHT after the keyword SYNC (not implemented yet)
        /// </summary>
        private long? _slackBytes = null;
        public long SlackBytes
        {
            get
            {
                if (_slackBytes != null)
                {
                    return _slackBytes.Value;
                }

                int index = Parent.ChildIndex(this);
                long occupiedMemory = 0;
                _slackBytes = 0;
                DataDefinition redefinedDataDefinition = null;

                if (Synchronized != null && Usage != null && Usage != DataUsage.None && Parent is DataDefinition parent)
                {
                    //Analyse all DataDefinition that preceed the current node
                    while (parent != null && parent.Type != CodeElementType.SectionHeader)
                    {
                        DataRedefines redefines = parent.Children[index] as DataRedefines;
                        //Get the original redefined node
                        if (redefines != null)
                        {
                            redefinedDataDefinition = redefines.RedefinedVariable;
                        }
                        
                        //Sum up all physical lengths except these from DataRedefines and the node that is redefined by the current node (if he is a DataRedefines)
                        for (int i = 0; i < index; i++)
                        {
                            var child = (DataDefinition) parent.Children[i];
                            if (child is DataRedefines == false && (redefinedDataDefinition == null || !child.Equals(redefinedDataDefinition)))
                            {
                                var dataRedefinition = child.GetBiggestRedefines();
                                occupiedMemory += Math.Max(dataRedefinition?.PhysicalLength + dataRedefinition?.SlackBytes ?? 0, child.PhysicalLength + child.SlackBytes);
                            }
                        }

                        index = parent.Parent.ChildIndex(parent);
                        parent = parent.Parent as DataDefinition;
                    }

                    
                    int m = 1;

                    switch (Usage.Value)
                    {
                        case DataUsage.Binary:
                            if (PictureValidationResult?.Digits <= 4)
                            {
                                m = 2;
                            }
                            else
                            {
                                m = 4;
                            }
                            break;
                        case DataUsage.Index:
                        case DataUsage.Pointer:
                        case DataUsage.Pointer32:
                        case DataUsage.ProcedurePointer:
                        case DataUsage.ObjectReference:
                        case DataUsage.FunctionPointer:
                        case DataUsage.PackedDecimal:
                            m = 4; 
                            break;
                        case DataUsage.FloatingPoint:
                            m = 8;
                            break;
                    }

                    if (occupiedMemory % m > 0)
                    {
                        _slackBytes = m - (occupiedMemory % m);
                    }


                }

                return _slackBytes.Value;

            }
        }

        private long? _startPosition = null;
        public virtual long StartPosition
        {
            get
            {
                if (!_startPosition.HasValue)
                {
                    ComputeStartPosition();
                }

                System.Diagnostics.Debug.Assert(_startPosition.HasValue);
                return _startPosition.Value;

                void ComputeStartPosition()
                {
                    if (this is DataRedefines node)
                    {
                        //Get the start position from the node it redefines.
                        var result = node.RedefinedVariable;
                        if (result != null)
                        {
                            _startPosition = result.StartPosition + SlackBytes;
                            return;
                        }

                        // Redefined variable does not exist -> handle node as a DataDescription
                    }

                    if (Parent is DataSection)
                    {
                        _startPosition = 1;
                        return;
                    }

                    //Searching for the first sibling with specified physical position, preceding the current node.
                    int siblingIndex = Parent.ChildIndex(this) - 1;
                    if (siblingIndex >= 0)
                    {
                        Node sibling = Parent.Children[siblingIndex];

                        //Looks further up if the first position encountered is from a DataRedefines node with an existing redefined variable.
                        while (sibling is DataRedefines dataRedefines && dataRedefines.RedefinedVariable != null)
                        {
                            sibling = Parent.Children[--siblingIndex];
                        }

                        DataDefinition siblingDefinition = (DataDefinition)sibling;
                        //Add 1 for the next free Byte in memory
                        _startPosition = Math.Max(siblingDefinition.GetBiggestRedefines()?.PhysicalPosition ?? 0, siblingDefinition.PhysicalPosition) + 1 + SlackBytes;
                        return;
                    }

                    // No previous data at same level, use position of parent
                    // Default to 0, but it means we could not compute the actual start position
                    _startPosition = (Parent as DataDefinition)?.StartPosition ?? 0;
                }
            }
        }

        /// PhysicalPosition is the position of the last Byte used by a DataDefinition in memory
        /// Minus 1 is due to PhysicalLength, which is calculated from 0. 
        public virtual long PhysicalPosition => StartPosition + PhysicalLength - 1;

        /// <summary>If this node a subordinate of a TYPEDEF entry?</summary>
        public virtual bool IsPartOfATypeDef { get { return _ParentTypeDefinition != null; } }
    
        private TypeDefinition _ParentTypeDefinition;
        /// <summary>
        /// Return Parent TypeDefinition if this node is under it
        /// Otherwise return null
        /// </summary>
        [CanBeNull]
        public TypeDefinition ParentTypeDefinition {
            get { return _ParentTypeDefinition; }
            set
            {
                if (_ParentTypeDefinition == null)
                    _ParentTypeDefinition = value;
            }
        }

        public TypeDefinition GetParentTypeDefinitionWithPath([NotNull] List<string> qualifiedPath)
        {
            Node currentNode = this;
            while (currentNode != null)
            {
                var typedNode = currentNode as TypeDefinition;
                if (typedNode != null) return typedNode;
                else
                    qualifiedPath.Add(currentNode.Name); //Store the path and ignore Type Name

                //Stop if we reach a Parent which is not a DataDefinion (Working storage section for example)
                if (!(currentNode is DataDefinition)) return null;

                currentNode = currentNode.Parent;
                
            }
            return null;
        }

        public bool IsStronglyTyped
        {
            get
            {
                if (DataType.RestrictionLevel == RestrictionLevel.STRONG) return true;
                var parent = Parent as DataDefinition;
                return parent != null && parent.IsStronglyTyped;
            }
        }

        public bool IsStrictlyTyped
        {
            get
            {
                if (DataType.RestrictionLevel == RestrictionLevel.STRICT) return true;
                var parent = Parent as DataDefinition;
                return parent != null && parent.IsStrictlyTyped;
            }
        }

        public bool IsTableIndex { get; internal set; }
        public string Hash
        {
            get
            {
                var hash = new StringBuilder();
                hash.Append(Name);
                return Tools.Hash.CreateCOBOLNameHash(hash.ToString(), 8);
            }
        }

        #region TypeProperties

        [CanBeNull]
        public AlphanumericValue Picture => _CommonDataDesc?.Picture;

        [CanBeNull]
        internal PictureValidator.Result PictureValidationResult => _CommonDataDesc?.PictureValidationResult;

        public bool IsJustified { get {  if(_CommonDataDesc != null && _CommonDataDesc.IsJustified != null) return _CommonDataDesc.IsJustified.Value; else return false; } }
        public virtual DataUsage? Usage
        {
            get
            {
                if (_CommonDataDesc != null && _CommonDataDesc.Usage != null)
                    return _CommonDataDesc.Usage.Value;

                if (CodeElement?.LevelNumber?.Value > 50)
                {
                    return null;
                }

                DataDefinition parent = Parent as DataDefinition;
                if (parent != null && parent.GroupUsage.HasValue)
                    return parent.GroupUsage.Value;

                return parent?.Usage;
            }
        }

        public bool IsGroupUsageNational => GroupUsage.HasValue && GroupUsage.Value == DataUsage.National;

        public bool IsGroupUsageUTF8 => GroupUsage.HasValue && GroupUsage.Value == DataUsage.UTF8;

        public DataUsage? GroupUsage
        {
            get
            {
                if (_CommonDataDesc?.GroupUsage != null)
                    return _CommonDataDesc.GroupUsage.Value;

                if (Parent is DataDefinition parent)
                    return parent.GroupUsage;

                return null;
            }
        }

        public long MinOccurencesCount { get { if (_CommonDataDesc != null && _CommonDataDesc.MinOccurencesCount != null) return _CommonDataDesc.MinOccurencesCount.Value; else return 1; } }
        public long MaxOccurencesCount { get { return _CommonDataDesc != null && _CommonDataDesc.MaxOccurencesCount != null ? _CommonDataDesc.MaxOccurencesCount.Value : 1; } }


        public NumericVariable OccursDependingOn { get { return _CommonDataDesc != null ? _CommonDataDesc.OccursDependingOn : null; } }
        public bool HasUnboundedNumberOfOccurences { get { if (_CommonDataDesc != null && _CommonDataDesc.HasUnboundedNumberOfOccurences != null) return _CommonDataDesc.HasUnboundedNumberOfOccurences.Value; else return false; } }
        public bool IsTableOccurence { get { if (_CommonDataDesc != null) return _CommonDataDesc.IsTableOccurence; else return false; } }
        public CodeElementType? Type { get { if (_CommonDataDesc != null) return _CommonDataDesc.Type; else return null; } }
        public bool SignIsSeparate { get { if (_CommonDataDesc != null && _CommonDataDesc.SignIsSeparate != null) return _CommonDataDesc.SignIsSeparate.Value; else return false; } }
        public SignPosition? SignPosition { get { if (_CommonDataDesc != null && _CommonDataDesc.SignPosition != null) return _CommonDataDesc.SignPosition.Value; else return null; } }

        public SyncAlignment? Synchronized
        {
            get
            {
                if (_CommonDataDesc?.Synchronized != null)
                    return _CommonDataDesc.Synchronized.Value;

                else if (Parent is DataDefinition parent)
                    return parent.Synchronized;

                else return null;
            }
        }
        public SymbolReference ObjectReferenceClass { get { if (_CommonDataDesc != null) return _CommonDataDesc.ObjectReferenceClass; else return null; } }
        #endregion
    }

    public class DataDescription: DataDefinition, Parent<DataDescription>{

        public DataDescription([NotNull] DataDescriptionEntry entry): base(entry) { }

        [NotNull]
        public new DataDescriptionEntry CodeElement => (DataDescriptionEntry) base.CodeElement;

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }

        /// <summary>
        /// A Dictonary that gives for a Token that appears in a qualified name its subtitution.
        /// </summary>
        public Dictionary<Token, string> QualifiedTokenSubsitutionMap;

        
    }
    public class DataCondition: DataDefinition 
    {
        public DataCondition([NotNull] DataConditionEntry entry): base(entry) { }

        [NotNull]
        public new DataConditionEntry CodeElement => (DataConditionEntry) base.CodeElement;

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    public class DataRedefines: DataDefinition {
        public DataRedefines([NotNull] DataRedefinesEntry entry)
            : base(entry)
        {
            _redefinedVariable = new Lazy<DataDefinition>(() => SymbolTable.GetRedefinedVariable(this));
        }

        [NotNull]
        public new DataRedefinesEntry CodeElement => (DataRedefinesEntry) base.CodeElement;

        private readonly Lazy<DataDefinition> _redefinedVariable;

        public DataDefinition RedefinedVariable => _redefinedVariable.Value;

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    public class DataRenames: DataDefinition {

        public DataRenames([NotNull] DataRenamesEntry entry): base(entry) { }

        [NotNull]
        public new DataRenamesEntry CodeElement => (DataRenamesEntry) base.CodeElement;

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    // [COBOL 2002]
    public class TypeDefinition: DataDefinition, Parent<DataDescription>, IDocumentable, IEquatable<TypeDefinition>
    {
        public TypeDefinition([NotNull] DataTypeDescriptionEntry entry) : base(entry)
        {
            TypedChildren = new List<DataDefinition>();
        }

        [NotNull]
        public new DataTypeDescriptionEntry CodeElement => (DataTypeDescriptionEntry) base.CodeElement;

        public RestrictionLevel RestrictionLevel { get { return this.CodeElement.RestrictionLevel; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }

        /// <summary>
        /// List of all children that reference a type.
        /// Element of this list can be null if :
        ///  - the child reference an unknown type, it'll be set to null in this list.
        ///  - We detect a circular reference between type. To avoid infinite loop one link of the circular reference will be set to null.
        ///
        /// ProgramClassBuilder to initialize this list.
        /// Only TypeCobolLinker can check the link and set items to null.
        /// </summary>
        [NotNull][ItemCanBeNull]
        public List<DataDefinition>  TypedChildren { get;  }

        public override bool IsPartOfATypeDef => true;

        public override bool Equals(object obj)
        {
            if (obj is TypeDefinition)
            {
                return Equals(obj as TypeDefinition);
            }

            var generatedDataType = (obj as GeneratedDefinition);
            if (generatedDataType != null &&
                !(generatedDataType.DataType == DataType.Alphabetic ||
                  generatedDataType.DataType == DataType.Alphanumeric)) //Remove these two check on Alpha.. to allow move "fezf" TO alphatypedVar
            {
                if (this.PrimitiveDataType != null)
                    return this.PrimitiveDataType == generatedDataType.DataType;
                else
                    return this.DataType == generatedDataType.DataType;
            }
            return false;
        }

        public bool Equals(TypeDefinition compareTypeDef)
        {
            if (Object.ReferenceEquals(this, compareTypeDef)) return true;
            if (Object.ReferenceEquals(null, compareTypeDef)) return false;

            return compareTypeDef.DataType == this.DataType &&
                   compareTypeDef.QualifiedName.ToString() == this.QualifiedName.ToString();
        }
        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = 13;
                hashCode = (hashCode * 397) ^ DataType.GetHashCode();
                hashCode = (hashCode * 397) ^ QualifiedName.ToString().GetHashCode();

                return hashCode;
            }
        }

        public override string ToString()
        {
            var writer = new StringWriter();
            Write(writer);
            return writer.ToString();
        }

        public void Write(TextWriter writer)
        {
            var lines = SelfAndChildrenLines.ToArray();
            int i = 0;
            while (i < lines.Length)
            {
                if (lines[i] is CodeElementsLine line)
                {
                    if (line.ScanState.InsideFormalizedComment)
                    {
                        while ((lines[i] as CodeElementsLine)?.ScanState.InsideFormalizedComment == true)
                            i++;
                    }
                    else if (line.IndicatorChar != '*')
                    {
                        writer.WriteLine(line.Text.Remove(0, 7));
                    }
                }
                i++;
            }

            var doc = CodeElement.FormalizedCommentDocumentation;
            if (doc != null)
            {
                writer.WriteLine();
                doc.Write(writer);
            }
        }
    }
    // [/COBOL 2002]

    // [TYPECOBOL]
    public class ParameterDescription: TypeCobol.Compiler.Nodes.DataDescription, Parent<ParametersProfileNode> {
        public ParameterDescription([NotNull] ParameterDescriptionEntry entry): base(entry) {  }

        [NotNull]
        public new ParameterDescriptionEntry CodeElement => (ParameterDescriptionEntry)base.CodeElement;

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }

        public new DataType DataType {
            get
            {
                return CodeElement.DataType;
            }
        }

        public PassingTypes PassingType { get; set; }
        public IntegerValue LevelNumber { get { return CodeElement.LevelNumber; } }
        public SymbolDefinition DataName { get { return CodeElement.DataName; } }

        public bool IsOmittable { get { return CodeElement.IsOmittable; } }

        public enum PassingTypes
        {
            Input,
            Output,
            InOut,
            Returning
        }
    }
    // [/TYPECOBOL]

    public class IndexDefinition : DataDefinition
    {
        public IndexDefinition(SymbolDefinition symbolDefinition) : base(null)
        {
            _SymbolDefinition = symbolDefinition;
            IsTableIndex = true;
        }

        private SymbolDefinition _SymbolDefinition;

        public override string Name
        {
            get { return _SymbolDefinition.Name; }
        }

        public override DataType DataType
        {
            get { return DataType.Numeric; }
        }

        public override DataUsage? Usage
        {
            get { return DataUsage.Index; }
        }

        public override long PhysicalLength
        {
            //Table indexes are not physically stored into their parent DATA section.
            //Their size and actual location in memory depends on the compiler implementation.
            get { return 0; }
        }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    /// <summary>
    /// Allow to generate DataDefinition which can take any desired form/type. 
    /// Give access to GeneratedDefinition of Numeric/Alphanumeric/Boolean/... DataType
    /// </summary>
    public class GeneratedDefinition : DataDefinition, IEquatable<GeneratedDefinition>
    {
        private string _Name;
        private readonly DataType _DataType;

        public static GeneratedDefinition NumericGeneratedDefinition =       new GeneratedDefinition("Numeric", DataType.Numeric);
        public static GeneratedDefinition AlphanumericGeneratedDefinition =  new GeneratedDefinition("Alphanumeric", DataType.Alphanumeric);
        public static GeneratedDefinition AlphabeticGeneratedDefinition =    new GeneratedDefinition("Alphabetic", DataType.Alphabetic);
        public static GeneratedDefinition BooleanGeneratedDefinition =       new GeneratedDefinition("Boolean", DataType.Boolean);
        public static GeneratedDefinition DBCSGeneratedDefinition =          new GeneratedDefinition("DBCS", DataType.DBCS);
        public static GeneratedDefinition DateGeneratedDefinition =          new GeneratedDefinition("Date", DataType.Date);
        public static GeneratedDefinition CurrencyGeneratedDefinition =      new GeneratedDefinition("Currency", DataType.Currency);
        public static GeneratedDefinition FloatingPointGeneratedDefinition = new GeneratedDefinition("FloatingPoint", DataType.FloatingPoint);
        public static GeneratedDefinition OccursGeneratedDefinition =        new GeneratedDefinition("Occurs", DataType.Occurs);
        public static GeneratedDefinition StringGeneratedDefinition =        new GeneratedDefinition("String", DataType.String);
        public GeneratedDefinition(string name, DataType dataType) : base(null)
        {
            _Name = name;
            _DataType = dataType;
        }

        public override string Name
        {
            get { return _Name; }
        }

        public override DataType DataType
        {
            get { return _DataType; }
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as GeneratedDefinition);
        }

        public bool Equals(GeneratedDefinition generatedDefinition)
        {
            if (Object.ReferenceEquals(this, generatedDefinition)) return true;
            if (Object.ReferenceEquals(null, generatedDefinition)) return false;
            return generatedDefinition.DataType == _DataType;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = 13;
                hashCode = (hashCode * 397) ^ _DataType.GetHashCode();

                return hashCode;
            }
        }
    }

    /// <summary>
    /// Helper class for manipulating table definitions (DataDescription or DataRedefines having an OCCURS clause).
    /// </summary>
    public static class TableDefinitionExtensions
    {
        /// <summary>
        /// Collect all parent table definitions of the given data-item.
        /// If the given DataDefinition is a table itself, it is included in the result.
        /// Results are ordered from the given data item to its highest parent table.
        /// </summary>
        /// <param name="dataDefinition">Data definition to start from.</param>
        /// <returns>List of parent tables. Maybe null if the data-item is part of a TC typedef,
        /// empty if the data-item does not belong to any table.</returns>
        /// <remarks>Returns empty list for a null starting DataDefinition.</remarks>
        [CanBeNull]
        public static List<DataDefinition> GetParentTableDefinitions([CanBeNull] this DataDefinition dataDefinition)
        {
            var result = new List<DataDefinition>();
            var tableDefinition = dataDefinition;
            while (tableDefinition != null)
            {
                //TODO SemanticDomain: use symbols and type expansion to get all the OCCURS including those coming from a typedef
                if (tableDefinition.IsPartOfATypeDef) return null;

                if (tableDefinition.IsTableOccurence) result.Add(tableDefinition);
                tableDefinition = tableDefinition.Parent as DataDefinition;
            }

            return result;
        }

        /// <summary>
        /// Get the sorting keys of the given table.
        /// </summary>
        /// <param name="tableDefinition">Non-null DataDefinition instance, property IsTableOccurence must be true.</param>
        /// <returns>Array of TableSortingKey, maybe null.</returns>
        [CanBeNull]
        public static TableSortingKey[] GetTableSortingKeys([NotNull] this DataDefinition tableDefinition)
        {
            System.Diagnostics.Debug.Assert(tableDefinition.CodeElement != null);
            System.Diagnostics.Debug.Assert(tableDefinition.IsTableOccurence);
            return ((CommonDataDescriptionAndDataRedefines) tableDefinition.CodeElement).TableSortingKeys;
        }

        /// <summary>
        /// Get the indexes of the given table.
        /// </summary>
        /// <param name="tableDefinition">Non-null DataDefinition instance, property IsTableOccurence must be true.</param>
        /// <returns>Array of SymbolDefinition, maybe null.</returns>
        [CanBeNull]
        public static SymbolDefinition[] GetIndexes([NotNull] this DataDefinition tableDefinition)
        {
            System.Diagnostics.Debug.Assert(tableDefinition.CodeElement != null);
            System.Diagnostics.Debug.Assert(tableDefinition.IsTableOccurence);
            return ((CommonDataDescriptionAndDataRedefines) tableDefinition.CodeElement).Indexes;
        }
    }

    public static class DataDefinitionExtensions
    {
        /// <summary>
        /// Indicates whether the given DataDefinition is a functional FILLER or not.
        /// See <see cref="CodeElementExtensions.IsFiller"/> for details on functional FILLERs.
        /// </summary>
        /// <param name="dataDefinition">DataDefinition instance, can be null.</param>
        /// <returns>True when the definition is a functional FILLER, False otherwise.</returns>
        public static bool IsFiller([CanBeNull] this DataDefinition dataDefinition)
        {
            return dataDefinition != null
                   && dataDefinition.ChildrenCount == 0 // No children
                   && dataDefinition.CodeElement.IsFiller(); // Anonymous or FILLER (by using keyword or FILLER-like name)
        }

        /// <summary>
        /// Test if the received DataDefinition has other children than DataConditionEntry or DataRenamesEntry
        /// </summary>
        /// <param name="dataDefinition">Non-null item to check</param>
        /// <returns>True if there are only DataConditionEntry or DataRenamesEntry children</returns>
        public static bool HasChildrenThatDeclareData([NotNull] this DataDefinition dataDefinition)
        {
            //We only need to check the last children:
            //DataConditionEntry is a level 88, DataRenamesEntry is level 66, both cannot have children
            //DataDescription and DataRedefines are level between 1 and 49 inclusive.
            //As the level number drives the positioning of Node inside the Children:
            //- if last child is a DataConditionEntry, it means all children are DataConditionEntry and no new data is declared
            //- if last child is a DataRenamesEntry, some data may be declared before the RENAMES
            if (dataDefinition.ChildrenCount > 0)
            {
                var lastChild = dataDefinition.Children[^1];

                if (lastChild.CodeElement == null)
                {
                    Debug.Assert(lastChild is IndexDefinition);
                    //Last child is an Index in an OCCURS: it is not a declaration
                    return false;
                }

                if (lastChild.CodeElement.Type == CodeElementType.DataRenamesEntry)
                {
                    //Last child is a DataRenamesEntry: we need to loop on the other children to find a possible DataDescription before
                    return dataDefinition.Children.Any(c => c is DataDescription);
                }

                return lastChild.CodeElement.Type != CodeElementType.DataConditionEntry;
            }

            return false;
        }

        /// <summary>
        /// Test the given data to check whether its PICTURE is among the given Picture categories.
        /// </summary>
        /// <param name="dataDefinition">Non-null data to test.</param>
        /// <param name="pictureCategories">Enumeration of Picture Categories to test, passing no categories will simply test whether
        /// the data has a PICTURE type or not.</param>
        /// <returns>True when the actual picture category of the data is among the given categories.</returns>
        public static bool HasPictureCategory([NotNull] this DataDefinition dataDefinition, params PictureCategory[] pictureCategories)
        {
            var type = dataDefinition.SemanticData?.Type;
            bool hasPicture = type?.Tag == Types.Type.Tags.Picture;
            if (hasPicture)
            {
                if (pictureCategories.Length == 0)
                    return true; // No categories to test, the method will just indicate that the data has a PICTURE

                var pictureType = (PictureType)type;
                return pictureCategories.Contains(pictureType.Category); // We could use HashSet, but we're assuming a small array of PictureCategory without duplicates
            }

            // No PICTURE
            return false;
        }

        public static bool IsNationalOrNationalEdited([NotNull] this DataDefinition dataDefinition)
            => dataDefinition.HasPictureCategory(PictureCategory.National, PictureCategory.NationalEdited);
    }

} // end of namespace TypeCobol.Compiler.Nodes
