namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public class DataDivision: CodeElementNode<DataDivisionHeader>, DataSection {
	public DataDivision(DataDivisionHeader header): base(header) { }
	public override string ID { get { return "data-division"; } }
}

public interface DataSection { }

public class FileSection: CodeElementNode<FileSectionHeader>, DataSection {
	public FileSection(FileSectionHeader header): base(header) { }
	public override string ID { get { return "file"; } }
}

public class WorkingStorageSection: CodeElementNode<WorkingStorageSectionHeader>, DataSection {
	public WorkingStorageSection(WorkingStorageSectionHeader header): base(header) { }
	public override string ID { get { return "working-storage"; } }
}

public class LocalStorageSection: CodeElementNode<LocalStorageSectionHeader>, DataSection {
	public LocalStorageSection(LocalStorageSectionHeader header): base(header) { }
	public override string ID { get { return "local-storage"; } }
}

public class LinkageSection: CodeElementNode<LinkageSectionHeader>, DataSection {
	public LinkageSection(LinkageSectionHeader header): base(header) { }
	public override string ID { get { return "linkage"; } }
}

public class DataDefinition: CodeElementNode<DataDescriptionEntry> {
	public DataDefinition(DataDescriptionEntry data): base(data) { }
	public override string ID { get { return CodeElement.QualifiedName.ToString(); } }
}



} // end of namespace TypeCobol.Compiler.Nodes
