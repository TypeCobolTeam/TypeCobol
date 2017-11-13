
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
    ///   IndexDefinition (TODO)
    /// </summary>
    public abstract class DataDefinition: Node, Parent<DataDefinition>, ITypedNode {

        private CommonDataDescriptionAndDataRedefines _ComonDataDesc { get { return this.CodeElement as CommonDataDescriptionAndDataRedefines; } }
        protected DataDefinition(DataDefinitionEntry entry): base(entry) {  }
        public override string ID { get { return "data-definition"; } }
        public override string Name { get { return ((DataDefinitionEntry)this.CodeElement).Name; } }


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

} // end of namespace TypeCobol.Compiler.Nodes
