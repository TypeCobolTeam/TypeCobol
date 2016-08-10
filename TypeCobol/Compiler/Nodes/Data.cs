namespace TypeCobol.Compiler.Nodes {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements;



public class DataDivision: Node<DataDivisionHeader>, DataSection {
	public DataDivision(DataDivisionHeader header): base(header) { }
	public override string ID { get { return "data-division"; } }
}



public interface DataSection { }

public class FileSection: Node<FileSectionHeader>, DataSection {
	public FileSection(FileSectionHeader header): base(header) { }
	public override string ID { get { return "file"; } }
}

public class WorkingStorageSection: Node<WorkingStorageSectionHeader>, DataSection {
	public WorkingStorageSection(WorkingStorageSectionHeader header): base(header) { }
	public override string ID { get { return "working-storage"; } }
	public virtual IList<DataDefinition> GetChildren() { return (IList<DataDefinition>)children; }
}

public class LocalStorageSection: Node<LocalStorageSectionHeader>, DataSection {
	public LocalStorageSection(LocalStorageSectionHeader header): base(header) { }
	public override string ID { get { return "local-storage"; } }
	public virtual IList<DataDefinition> GetChildren() { return (IList<DataDefinition>)children; }
}

public class LinkageSection: Node<LinkageSectionHeader>, DataSection {
	public LinkageSection(LinkageSectionHeader header): base(header) { }
	public override string ID { get { return "linkage"; } }
	public virtual IList<DataDescription> GetChildren() { return (IList<DataDescription>)children; }
}



public interface DataDefinition { }

public class DataDescription: Node<DataDescriptionEntry>, DataDefinition {
	public DataDescription(DataDescriptionEntry data): base(data) { }
	public override string ID { get { return CodeElement.DataName.Name; } }
	public virtual IList<DataDescription> GetChildren() { return (IList<DataDescription>)children; }
}

public class DataCondition: Node<DataConditionEntry>, DataDefinition {
	public DataCondition(DataConditionEntry data): base(data) { }
	public override string ID { get { return CodeElement.DataName.Name; } }
}

public class DataRedefines: Node<DataRedefinesEntry>, DataDefinition {
	public DataRedefines(DataRedefinesEntry data): base(data) { }
	public override string ID { get { return CodeElement.DataName.Name; } }
}

public class DataRenames: Node<DataRenamesEntry>, DataDefinition {
	public DataRenames(DataRenamesEntry data): base(data) { }
	public override string ID { get { return CodeElement.DataName.Name; } }
}
// [COBOL 2002]
public class TypeDefinition: Node<TypeDefinitionEntry>, DataDefinition {
	public TypeDefinition(TypeDefinitionEntry data): base(data) { }
	public override string ID { get { return CodeElement.DataName.Name; } }
}
// [/COBOL 2002]



} // end of namespace TypeCobol.Compiler.Nodes
