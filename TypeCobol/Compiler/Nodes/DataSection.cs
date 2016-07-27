using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Nodes {

	public interface DataSection { }

	public class WorkingStorageSection: Node, DataSection {
		public WorkingStorageSection(WorkingStorageSectionHeader header): base(header) { }
	}

	public class LocalStorageSection: Node, DataSection {
		public LocalStorageSection(LocalStorageSectionHeader header): base(header) { }
	}

	public class LinkageSection: Node, DataSection {
		public LinkageSection(LinkageSectionHeader header): base(header) { }
	}
}
