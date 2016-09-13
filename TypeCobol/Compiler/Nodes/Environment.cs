namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public class EnvironmentDivision: CodeElementNode<EnvironmentDivisionHeader> {
	public EnvironmentDivision(EnvironmentDivisionHeader header): base(header) { }
	public override string ID { get { return "environment-division"; } }
}

public interface EnvironmentSection { }

public class ConfigurationSection: CodeElementNode<ConfigurationSectionHeader>, EnvironmentSection {
	public ConfigurationSection(ConfigurationSectionHeader header): base(header) { }
	public override string ID { get { return "configuration"; } }
}

public class SourceComputer: CodeElementNode<SourceComputerParagraph> {
	public SourceComputer(SourceComputerParagraph paragraph): base(paragraph) { }
	public override string ID { get { return "source-computer"; } }
}

public class ObjectComputer: CodeElementNode<ObjectComputerParagraph> {
	public ObjectComputer(ObjectComputerParagraph paragraph): base(paragraph) { }
	public override string ID { get { return "object-computer"; } }
}

public class SpecialNames: CodeElementNode<SpecialNamesParagraph> {
	public SpecialNames(SpecialNamesParagraph paragraph): base(paragraph) { }
	public override string ID { get { return "special-names"; } }
}

public class Repository: CodeElementNode<RepositoryParagraph> {
	public Repository(RepositoryParagraph paragraph): base(paragraph) { }
	public override string ID { get { return "repository"; } }
}

public class InputOutputSection: CodeElementNode<InputOutputSectionHeader>, EnvironmentSection {
	public InputOutputSection(InputOutputSectionHeader header): base(header) { }
	public override string ID { get { return "input-output"; } }
}

} // end of namespace TypeCobol.Compiler.Nodes
