
namespace TypeCobol.Compiler.Nodes {

	using System;
	using TypeCobol.Compiler.CodeElements;
    


    public class DataDivision: Node, CodeElementHolder<DataDivisionHeader>, Parent<DataSection> {
	    public DataDivision(DataDivisionHeader header): base(header) { }
	    public override string ID { get { return "data-division"; } }

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
        protected DataDefinition(DataDefinitionEntry entry): base(entry) { }
        public override string ID { get { return ((DataDefinitionEntry)this.CodeElement).Name; } }
        public override bool VisitNode(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }

        public DataType DataType
        {
            get
            {
                var dataDefinitionEntry = this.CodeElement as DataDefinitionEntry;
                if (dataDefinitionEntry != null)
                {
                    return dataDefinitionEntry.DataType;
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
                var dataDefinitionEntry = this.CodeElement as DataDefinitionEntry;
                if (dataDefinitionEntry != null)
                {
                    return dataDefinitionEntry.Length;
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

        public bool IsStronglyTyped
        {
            get
            {
                if (DataType.IsStrong) return true;
                var parent = Parent as DataDefinition;
                return parent != null && parent.IsStronglyTyped;
            }
        }
    }

    public class DataDescription: DataDefinition, CodeElementHolder<DataDescriptionEntry>, Parent<DataDescription>{
        public DataDescription(DataDescriptionEntry entry): base(entry) { }

        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
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
        public bool IsStrong { get { return this.CodeElement().IsStrong; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    // [/COBOL 2002]

    // [TYPECOBOL]
    public class ParameterDescription: TypeCobol.Compiler.Nodes.DataDescription {
	    public ParameterDescription(ParameterDescriptionEntry entry): base(entry) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return base.VisitNode(astVisitor) && astVisitor.Visit(this);
        }
    }
    // [/TYPECOBOL]

} // end of namespace TypeCobol.Compiler.Nodes
