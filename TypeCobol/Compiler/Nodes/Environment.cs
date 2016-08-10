namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public class EnvironmentDivision: Node<EnvironmentDivisionHeader> {
	public EnvironmentDivision(EnvironmentDivisionHeader header): base(header) { }
	public override string ID { get { return "environment-division"; } }
}

public interface EnvironmentSection { }

public class ConfigurationSection: Node<ConfigurationSectionHeader>, EnvironmentSection {
	public ConfigurationSection(ConfigurationSectionHeader header): base(header) { }
	public override string ID { get { return "configuration"; } }
}

public class SourceComputer: Node<SourceComputerParagraph> {
	public SourceComputer(SourceComputerParagraph paragraph): base(paragraph) { }
	public override string ID { get { return "source-computer"; } }
}

public class ObjectComputer: Node<ObjectComputerParagraph> {
	public ObjectComputer(ObjectComputerParagraph paragraph): base(paragraph) { }
	public override string ID { get { return "object-computer"; } }
}

public class SpecialNames: Node<SpecialNamesParagraph> {
	public SpecialNames(SpecialNamesParagraph paragraph): base(paragraph) { }
	public override string ID { get { return "special-names"; } }
}

public class Repository: Node<RepositoryParagraph> {
	public Repository(RepositoryParagraph paragraph): base(paragraph) { }
	public override string ID { get { return "repository"; } }
}

public class InputOutputSection: Node<InputOutputSectionHeader>, EnvironmentSection {
	public InputOutputSection(InputOutputSectionHeader header): base(header) { }
	public override string ID { get { return "input-output"; } }
}

} // end of namespace TypeCobol.Compiler.Nodes
