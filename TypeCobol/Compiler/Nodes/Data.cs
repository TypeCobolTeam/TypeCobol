
using JetBrains.Annotations;

namespace TypeCobol.Compiler.Nodes {

    using System;
    using System.Collections.Generic;
    using CodeElements.Expressions;
    using Scanner;
    using TypeCobol.Compiler.CodeElements;



    public class DataDivision: Node, CodeElementHolder<DataDivisionHeader>, Parent<DataSection> {

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

        public abstract class DataSection: Node, CodeElementHolder<DataSectionHeader>, Child<DataDivision>{
	    protected DataSection(DataSectionHeader header): base(header) { }
	    public virtual bool IsShared { get { return false; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class FileSection: DataSection, CodeElementHolder<FileSectionHeader>{
	    public FileSection(FileSectionHeader header): base(header) { }
	    public override string ID { get { return "file"; } }
	    public override bool IsShared { get { return true; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }

    public class FileDescriptionEntryNode : Node, CodeElementHolder<FileDescriptionEntry> {
	    public FileDescriptionEntryNode(FileDescriptionEntry entry): base(entry) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }


    public class WorkingStorageSection: DataSection, CodeElementHolder<WorkingStorageSectionHeader>, Parent<DataDefinition>
        {
	    public WorkingStorageSection(WorkingStorageSectionHeader header): base(header) { }
	    public override string ID { get { return "working-storage"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    public class LocalStorageSection: DataSection, CodeElementHolder<LocalStorageSectionHeader>, Parent<DataDefinition>
        {
	    public LocalStorageSection(LocalStorageSectionHeader header): base(header) { }
	    public override string ID { get { return "local-storage"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    public class LinkageSection: DataSection, CodeElementHolder<LinkageSectionHeader>, Parent<DataDefinition>
    {
	    public LinkageSection(LinkageSectionHeader header): base(header) { }
	    public override string ID { get { return "linkage"; } }
	    public override bool IsShared { get { return true; } }

        public override bool VisitNode(IASTVisitor astVisitor) {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }

    /// <summary>
    /// DataDefinition
    ///   DataDescription
    ///      ParameterDescription
    ///      TypedDataNode
    ///   DataCondition
    ///   DataRedefines
    ///   DataRenames
    ///   TypeDefinition
    ///   IndexDefinition
    ///   GeneratedDefinition
    /// </summary>
    public abstract class DataDefinition: Node, Parent<DataDefinition>, ITypedNode {

        private CommonDataDescriptionAndDataRedefines _ComonDataDesc { get { return this.CodeElement as CommonDataDescriptionAndDataRedefines; } }

        protected DataDefinition(DataDefinitionEntry entry) : base(entry) { }
        public override string ID { get { return "data-definition"; } }
        public override string Name { get { return ((DataDefinitionEntry)this.CodeElement).Name; } }

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

        public override bool VisitNode(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }

        public virtual DataType DataType
        {
            get
            {
                if (this.CodeElement != null)
                {
                    return ((DataDefinitionEntry)this.CodeElement).DataType;
                }
                return DataType.Unknown;
            }
        }

        //public virtual DataType PrimitiveDataType
        //{
        //    get
        //    {
        //        if (this.Picture != null) //Get DataType based on Picture clause
        //            return DataType.Create(this.Picture.Value);
        //        else if (this.Usage.HasValue) //Get DataType based on Usage clause
        //            return DataType.Create(this.Usage.Value);
        //        else
        //            return null;
        //    }
        //}

        /// <summary>
        /// TODO This method should be split like this:
        /// - PhysicalLength 
        /// - PhysicalLengthWithChildren
        /// - LogicalLength
        /// - LogicalLengthWithChildren
        /// </summary>
        public virtual int Length
        {
            get
            {
                if (this.CodeElement != null)
                {
                    return ((DataDefinitionEntry)this.CodeElement).Length;
                }
                return 0;
            }
        }

        /// <summary>If this node a subordinate of a TYPEDEF entry?</summary>
        public bool IsPartOfATypeDef
        {
            get
            {
                var parent = Parent;
                while (parent != null)
                {
                    if (!(parent is DataDefinition)) return false;
                    if (parent is TypeDefinition) return true;
                    parent = parent.Parent;
                }
                return false;
            }
        }

        /// <summary>
        /// Return Parent TypeDefinition if this node is under it
        /// Otherwise return null
        /// </summary>
        [CanBeNull]
        public TypeDefinition GetParentTypeDefinition {
            get { return GetParentTypeDefinitionWithPath(new List<string>()); }
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

        #region TypeProperties
        public AlphanumericValue Picture { get {return _ComonDataDesc != null ? _ComonDataDesc.Picture : null;}}
        public bool IsJustified { get {  if(_ComonDataDesc != null && _ComonDataDesc.IsJustified != null) return _ComonDataDesc.IsJustified.Value; else return false; } }
        public DataUsage? Usage { get { if (_ComonDataDesc != null && _ComonDataDesc.Usage != null) return _ComonDataDesc.Usage.Value; else return null; } }
        public bool IsGroupUsageNational { get { if (_ComonDataDesc != null && _ComonDataDesc.IsGroupUsageNational != null) return _ComonDataDesc.IsGroupUsageNational.Value; else return false; } }
        public long MinOccurencesCount { get { if (_ComonDataDesc != null && _ComonDataDesc.MinOccurencesCount != null) return _ComonDataDesc.MinOccurencesCount.Value; else return 1; } }
        public long MaxOccurencesCount { get {return _ComonDataDesc != null && _ComonDataDesc.MaxOccurencesCount != null ? _ComonDataDesc.MaxOccurencesCount.Value : 1;}}


        public NumericVariable OccursDependingOn { get {return _ComonDataDesc != null ? _ComonDataDesc.OccursDependingOn : null;}}
        public bool HasUnboundedNumberOfOccurences { get { if (_ComonDataDesc != null && _ComonDataDesc.HasUnboundedNumberOfOccurences != null) return _ComonDataDesc.HasUnboundedNumberOfOccurences.Value; else return false; } }
        public bool IsTableOccurence { get { if (_ComonDataDesc != null) return _ComonDataDesc.IsTableOccurence; else return false; } }
        public CodeElementType? Type { get { if (_ComonDataDesc != null) return _ComonDataDesc.Type; else return null; } }
        public bool SignIsSeparate { get { if (_ComonDataDesc != null && _ComonDataDesc.SignIsSeparate != null) return _ComonDataDesc.SignIsSeparate.Value; else return false;  } }
        public SignPosition? SignPosition { get { if (_ComonDataDesc != null && _ComonDataDesc.SignPosition != null) return _ComonDataDesc.SignPosition.Value; else return null; } }
        public bool IsSynchronized { get { if (_ComonDataDesc != null && _ComonDataDesc.IsSynchronized != null) return _ComonDataDesc.IsSynchronized.Value; else return false;  } }
        public SymbolReference ObjectReferenceClass { get { if (_ComonDataDesc != null) return _ComonDataDesc.ObjectReferenceClass; else return null; } }
        #endregion
    }

    public class DataDescription: DataDefinition, CodeElementHolder<DataDescriptionEntry>, Parent<DataDescription>{
        public DataDescription(DataDescriptionEntry entry): base(entry) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
        /// <summary>
        /// A Dictonary that gives for a Token that appears in a qualified name its subtitution.
        /// </summary>
        public Dictionary<Token, string> QualifiedTokenSubsitutionMap;
    }
    public class DataCondition: DataDefinition, CodeElementHolder<DataConditionEntry> 
    {
        public DataCondition(DataConditionEntry entry): base(entry) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    public class DataRedefines: DataDefinition, CodeElementHolder<DataRedefinesEntry> {
        public DataRedefines(DataRedefinesEntry entry): base(entry) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    public class DataRenames: DataDefinition, CodeElementHolder<DataRenamesEntry> {
        public DataRenames(DataRenamesEntry entry): base(entry) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    // [COBOL 2002]
    public class TypeDefinition: DataDefinition, CodeElementHolder<DataTypeDescriptionEntry>, Parent<DataDescription>
    {
        public TypeDefinition(DataTypeDescriptionEntry entry): base(entry) { }
        public RestrictionLevel RestrictionLevel { get { return this.CodeElement().RestrictionLevel; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }

        public override bool Equals(object obj)
        {
            if ((obj as TypeDefinition) != null)
            {
                var compareTypeDef = (TypeDefinition) obj;
                return compareTypeDef.DataType == this.DataType &&
                       //compareTypeDef.PrimitiveDataType == this.PrimitiveDataType &&
                       compareTypeDef.QualifiedName.ToString() == this.QualifiedName.ToString();
            }
            else if ((obj as GeneratedDefinition) != null)
            {
                //if (this.PrimitiveDataType != null)
                //    return this.PrimitiveDataType == ((GeneratedDefinition)obj).DataType;
                //else
                    return this.DataType == ((GeneratedDefinition)obj).DataType;
            }
            return false;
        }
    }
    // [/COBOL 2002]

    // [TYPECOBOL]
    public class ParameterDescription: TypeCobol.Compiler.Nodes.DataDescription, CodeElementHolder<ParameterDescriptionEntry>, Parent<ParametersProfileNode> {

        private readonly ParameterDescriptionEntry _CodeElement;

        public ParameterDescription(ParameterDescriptionEntry entry): base(entry) { _CodeElement = (ParameterDescriptionEntry)this.CodeElement; }
       
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }

        public new DataType DataType {
            get
            {
                return _CodeElement.DataType;
            }
        }

        public AlphanumericValue Picture { get { return _CodeElement.Picture; } }
        public IntegerValue LevelNumber { get { return _CodeElement.LevelNumber; } }
        public SymbolDefinition DataName { get { return _CodeElement.DataName; } }

        public bool IsOmittable { get { return _CodeElement.IsOmittable; } }


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
