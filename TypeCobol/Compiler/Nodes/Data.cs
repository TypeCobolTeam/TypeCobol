
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using JetBrains.Annotations;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.Nodes {

    using System;
    using System.Collections.Generic;
    using CodeElements.Expressions;
    using Scanner;
    using TypeCobol.Compiler.CodeElements;



    public class DataDivision: GenericNode<DataDivisionHeader>, Parent<DataSection> {

        public const string NODE_ID = "data-division";
	    public DataDivision(DataDivisionHeader header): base(header) { }
	    public override string ID { get { return NODE_ID; } }

	    public override void Add(Node child, int index = -1) {
		    if (index <= 0) index = WhereShouldIAdd(child.GetType());
		    base.Add(child,index);
	    }
        private int WhereShouldIAdd(System.Type section) {
            if (Tools.Reflection.IsTypeOf(section, typeof(FileSection))) return 0;
            int ifile = -2;
            int iworking = -2;
            int ilocal = -2;
            int ilinkage = -2;
            int c = 0;
            foreach(var child in this.Children()) {
                if (Tools.Reflection.IsTypeOf(child.GetType(), typeof(FileSection))) ifile = c;
                else
                if (Tools.Reflection.IsTypeOf(child.GetType(), typeof(WorkingStorageSection))) iworking = c;
                else
                if (Tools.Reflection.IsTypeOf(child.GetType(), typeof(LocalStorageSection))) ilocal = c;
                else
                if (Tools.Reflection.IsTypeOf(child.GetType(), typeof(LinkageSection))) ilinkage = c;
                c++;
            }
            if (Tools.Reflection.IsTypeOf(section, typeof(WorkingStorageSection))) return Math.Max(0,ifile+1);
            if (Tools.Reflection.IsTypeOf(section, typeof(LocalStorageSection))) return Math.Max(0,Math.Max(ifile+1,iworking+1));
            if (Tools.Reflection.IsTypeOf(section, typeof(LinkageSection))) return Math.Max(0,Math.Max(ifile+1,Math.Max(iworking+1,ilocal+1)));
            return 0;
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

    public class FileDescriptionEntryNode : GenericNode<FileDescriptionEntry> {
        public FileDescriptionEntryNode(FileDescriptionEntry entry): base(entry) { }
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
    ///   DataDescription               ->          DataDescriptionEntry
    ///      ParameterDescription       ->          ParameterDescriptionEntry
    ///      TypedDataNode              ->          DataDescriptionEntry
    ///   TypeDefinition                ->          DataTypeDescriptionEntry
    ///   DataRedefines                 ->          DataRedefinesEntry
    ///   DataCondition                 ->      DataConditionEntry
    ///   DataRenames                   ->      DataRenamesEntry
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

        private CommonDataDescriptionAndDataRedefines _ComonDataDesc { get { return this.CodeElement as CommonDataDescriptionAndDataRedefines; } }


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

        private DataType _primitiveDataType;
        public virtual DataType PrimitiveDataType
        {
            get
            {
                if (_primitiveDataType != null) return _primitiveDataType;
                if (this.Picture != null) //Get DataType based on Picture clause
                    _primitiveDataType = DataType.Create(this.Picture.Value);
                else if (this.Usage.HasValue) //Get DataType based on Usage clause
                    _primitiveDataType = DataType.Create(this.Usage.Value);
                else
                    return null;

                return _primitiveDataType;
            }
        }

        private PictureValidator _pictureValidator;
        public PictureValidator PictureValidator
        {
            get
            {
                if (_pictureValidator != null) return _pictureValidator;

                _pictureValidator = new PictureValidator(Picture.Value, SignIsSeparate);

                return _pictureValidator;
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
                    else if (CodeElement?.LevelNumber?.Value == 88)
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
            TypeCobolType.UsageFormat usage = TypeCobolType.UsageFormat.None;
            if (Usage != null)
            {
                switch (Usage.Value)
                {
                    case DataUsage.Binary:
                    case DataUsage.NativeBinary:
                        usage = TypeCobolType.UsageFormat.Binary;
                        break;
                    case DataUsage.FloatingPoint:
                        usage = TypeCobolType.UsageFormat.Comp1;
                        break;
                    case DataUsage.Display:
                        usage = TypeCobolType.UsageFormat.Display;
                        break;
                    case DataUsage.FunctionPointer:
                        usage = TypeCobolType.UsageFormat.FunctionPointer;
                        break;
                    case DataUsage.Index:
                        usage = TypeCobolType.UsageFormat.Index;
                        break;
                    case DataUsage.National:
                        usage = TypeCobolType.UsageFormat.National;
                        break;
                    case DataUsage.None:
                        usage = TypeCobolType.UsageFormat.None;
                        break;
                    case DataUsage.ObjectReference:
                        usage = TypeCobolType.UsageFormat.ObjectReference;
                        break;
                    case DataUsage.PackedDecimal:
                        usage = TypeCobolType.UsageFormat.PackedDecimal;
                        break;
                    case DataUsage.Pointer:
                        usage = TypeCobolType.UsageFormat.Pointer;
                        break;
                    case DataUsage.ProcedurePointer:
                        usage = TypeCobolType.UsageFormat.ProcedurePointer;
                        break;
                    case DataUsage.LongFloatingPoint:
                        usage = TypeCobolType.UsageFormat.Comp2;
                        break;
                    case DataUsage.DBCS:
                        usage = TypeCobolType.UsageFormat.Display1;
                        break;
                }
            }
            
            if (Picture == null)
            {
                if (Usage != null && Usage.Value != DataUsage.None)
                {
                    return new TypeCobolType(TypeCobolType.Tags.Usage, usage).Length;
                }
                return 1;
            }
            if (PictureValidator.IsValid())
            {
                PictureType type = new PictureType(PictureValidator);
                type.Usage = usage;
                return type.Length;
            }
            else
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

                if (IsSynchronized && Usage != null && Usage != DataUsage.None && Parent is DataDefinition parent)
                {
                    //Analyse all DataDefinition that preceed the current node
                    while (parent != null && parent.Type != CodeElementType.SectionHeader)
                    {
                        DataRedefines redefines = parent.Children[index] as DataRedefines;
                        //Get the original redefined node
                        while (redefines != null)
                        {
                            SymbolReference redefined = redefines.CodeElement.RedefinesDataName;
                            redefinedDataDefinition = redefines.SymbolTable.GetRedefinedVariable(redefines, redefined);

                            redefines = redefinedDataDefinition as DataRedefines;
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
                            if (PictureValidator.ValidationContext.Digits <= 4)
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
                if (_startPosition.HasValue)
                {
                    return _startPosition.Value;
                }

                if (this is DataRedefines node)
                {
                    //Get the start position from the node it redefines.
                    SymbolReference redefined = node.CodeElement.RedefinesDataName;
                    var result = SymbolTable.GetRedefinedVariable(node, redefined);

                    _startPosition = result.StartPosition + SlackBytes;
                    return _startPosition.Value;
                    
                }

                if (Parent is DataSection)
                {
                    _startPosition = 1;
                }
                else
                {
                    //Searching for the first sibling with specified physical position, preceeding the current node.
                    for (int i = 0; i < Parent.Children.Count; i++)
                    {
                        Node sibling = Parent.Children[i];

                        if (i == Parent.ChildIndex(this) - 1)
                        {
                            int siblingIndex = i;
                            //Looks further up if the first position encountered is from a DataRedefines node.
                            while (sibling is DataRedefines)
                            {
                                sibling = Parent.Children[siblingIndex - 1];

                                siblingIndex--;

                            }

                            DataDefinition siblingDefinition = (DataDefinition)sibling;
                            //Add 1 for the next free Byte in memory
                            _startPosition = Math.Max(siblingDefinition.GetBiggestRedefines()?.PhysicalPosition ?? 0, siblingDefinition.PhysicalPosition) + 1 + SlackBytes;
                            
                        }

                    }
                    if (_startPosition == null)
                    {
                        _startPosition = (Parent as DataDefinition)?.StartPosition;
                    }
                }

                return _startPosition ?? 0;
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

        public bool IsIndex { get; internal set; }
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
        public AlphanumericValue Picture { get {return _ComonDataDesc != null ? _ComonDataDesc.Picture : null;}}
        public bool IsJustified { get {  if(_ComonDataDesc != null && _ComonDataDesc.IsJustified != null) return _ComonDataDesc.IsJustified.Value; else return false; } }
        public DataUsage? Usage
        {
            get
            {
                if (_ComonDataDesc != null && _ComonDataDesc.Usage != null)
                    return _ComonDataDesc.Usage.Value;

                if (CodeElement?.LevelNumber?.Value > 50)
                {
                    return null;
                }

                DataDefinition parent = Parent as DataDefinition;
                if (parent != null && parent.IsGroupUsageNational)
                    return DataUsage.National;

                return parent?.Usage;
            }
        }

        public bool IsGroupUsageNational
        {
            get
            {
                if (_ComonDataDesc?.IsGroupUsageNational != null)
                    return _ComonDataDesc.IsGroupUsageNational.Value;

                else if (Parent is DataDefinition parent)
                    return parent.IsGroupUsageNational;

                else return false;
            }
        }
        public long MinOccurencesCount { get { if (_ComonDataDesc != null && _ComonDataDesc.MinOccurencesCount != null) return _ComonDataDesc.MinOccurencesCount.Value; else return 1; } }
        public long MaxOccurencesCount { get { return _ComonDataDesc != null && _ComonDataDesc.MaxOccurencesCount != null ? _ComonDataDesc.MaxOccurencesCount.Value : 1; } }


        public NumericVariable OccursDependingOn { get { return _ComonDataDesc != null ? _ComonDataDesc.OccursDependingOn : null; } }
        public bool HasUnboundedNumberOfOccurences { get { if (_ComonDataDesc != null && _ComonDataDesc.HasUnboundedNumberOfOccurences != null) return _ComonDataDesc.HasUnboundedNumberOfOccurences.Value; else return false; } }
        public bool IsTableOccurence { get { if (_ComonDataDesc != null) return _ComonDataDesc.IsTableOccurence; else return false; } }
        public CodeElementType? Type { get { if (_ComonDataDesc != null) return _ComonDataDesc.Type; else return null; } }
        public bool SignIsSeparate { get { if (_ComonDataDesc != null && _ComonDataDesc.SignIsSeparate != null) return _ComonDataDesc.SignIsSeparate.Value; else return false; } }
        public SignPosition? SignPosition { get { if (_ComonDataDesc != null && _ComonDataDesc.SignPosition != null) return _ComonDataDesc.SignPosition.Value; else return null; } }

        public bool IsSynchronized
        {
            get
            {
                if (_ComonDataDesc?.IsSynchronized != null)
                    return _ComonDataDesc.IsSynchronized.Value;

                else if (Parent is DataDefinition parent)
                    return parent.IsSynchronized;

                else return false;
            }
        }
        public SymbolReference ObjectReferenceClass { get { if (_ComonDataDesc != null) return _ComonDataDesc.ObjectReferenceClass; else return null; } }
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
        public DataRedefines([NotNull] DataRedefinesEntry entry) : base(entry) { }

        [NotNull]
        public new DataRedefinesEntry CodeElement => (DataRedefinesEntry) base.CodeElement;

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
    public class TypeDefinition: DataDefinition, Parent<DataDescription>, IDocumentable
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
            if ((obj as TypeDefinition) != null)
            {
                var compareTypeDef = (TypeDefinition) obj;
                return compareTypeDef.DataType == this.DataType &&
                       //compareTypeDef.PrimitiveDataType == this.PrimitiveDataType &&
                       compareTypeDef.QualifiedName.ToString() == this.QualifiedName.ToString();
            }

            var generatedDataType = (obj as GeneratedDefinition);
            if (generatedDataType  != null && 
                !(generatedDataType.DataType == DataType.Alphabetic ||
                  generatedDataType .DataType == DataType.Alphanumeric)) //Remove these two check on Alpha.. to allow move "fezf" TO alphatypedVar
            {
                if (this.PrimitiveDataType != null)
                    return this.PrimitiveDataType == generatedDataType.DataType;
                else
                    return this.DataType == generatedDataType.DataType;
            }
            return false;
        }

        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            FormalizedCommentDocumentation doc = this.CodeElement.FormalizedCommentDocumentation;

            int i = 0;

            while (i < SelfAndChildrenLines.Count())
            {
                if (SelfAndChildrenLines.ElementAt(i) is CodeElementsLine line)
                {
                    if (line.ScanState.InsideFormalizedComment)
                    {
                        while ((SelfAndChildrenLines.ElementAt(i) as CodeElementsLine)?.ScanState.InsideFormalizedComment == true)
                            i++;
                    }
                    else if (line.IndicatorChar != '*')
                    {
                        sb.AppendLine(line.Text);
                    }
                }
                i++;
            }

            if (doc != null)
            {
                sb.AppendLine();
                sb.Append(doc);
            }

            return sb.ToString();
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
            InOut
        }
    }
    // [/TYPECOBOL]

    public class IndexDefinition : DataDefinition
    {
        public IndexDefinition(SymbolDefinition symbolDefinition) : base(null)
        {
            _SymbolDefinition = symbolDefinition;
            IsIndex = true;
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

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    /// <summary>
    /// Allow to generate DataDefinition which can take any desired form/type. 
    /// Give access to GeneratedDefinition of Numeric/Alphanumeric/Boolean/... DataType
    /// </summary>
    public class GeneratedDefinition : DataDefinition
    {
        private string _Name;
        private DataType _DataType;

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
            //In this case we can only compare the DataType
            if((obj as DataDefinition) != null)
                return ((DataDefinition) obj).DataType == _DataType;
            return false;
        }
    }

} // end of namespace TypeCobol.Compiler.Nodes
