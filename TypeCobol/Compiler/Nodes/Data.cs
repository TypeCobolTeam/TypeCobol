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
	public virtual IList<DataDefinition<DataDefinitionEntry>> GetChildren() { return (IList<DataDefinition<DataDefinitionEntry>>)children; }
}

public class LocalStorageSection: Node<LocalStorageSectionHeader>, DataSection {
	public LocalStorageSection(LocalStorageSectionHeader header): base(header) { }
	public override string ID { get { return "local-storage"; } }
	public virtual IList<DataDefinition<DataDefinitionEntry>> GetChildren() { return (IList<DataDefinition<DataDefinitionEntry>>)children; }
}

public class LinkageSection: Node<LinkageSectionHeader>, DataSection {
	public LinkageSection(LinkageSectionHeader header): base(header) { }
	public override string ID { get { return "linkage"; } }
	public virtual IList<DataDescription> GetChildren() { return (IList<DataDescription>)children; }
}



public abstract class DataDefinition<T>: Node<T> where T:DataDefinitionEntry {
	public DataDefinition(T data): base(data) { }
	public override string ID { get { return CodeElement.DataName.Name; } }
}

public class DataDescription: DataDefinition<DataDescriptionEntry> {
	public DataDescription(DataDescriptionEntry data): base(data) { }
	public virtual IList<DataDescription> GetChildren() { return (IList<DataDescription>)children; }
}

public class DataCondition: DataDefinition<DataConditionEntry> {
	public DataCondition(DataConditionEntry data): base(data) { }
}

public class DataRedefines: DataDefinition<DataRedefinesEntry> {
	public DataRedefines(DataRedefinesEntry data): base(data) { }
}

public class DataRenames: DataDefinition<DataRenamesEntry> {
	public DataRenames(DataRenamesEntry data): base(data) { }
}
// [COBOL 2002]
public class TypeDescription: DataDefinition<TypeDefinitionEntry> {
	public TypeDescription(TypeDefinitionEntry data): base(data) { }
	public virtual IList<DataDescription> GetChildren() { return (IList<DataDescription>)children; }
}
// [/COBOL 2002]



} // end of namespace TypeCobol.Compiler.Nodes
