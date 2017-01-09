namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

    public class EnvironmentDivision: Node, CodeElementHolder<EnvironmentDivisionHeader> {
	    public EnvironmentDivision(EnvironmentDivisionHeader header): base(header) { }
	    public override string ID { get { return "environment-division"; } }

        public override bool VisitNode(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }
    }

    public interface EnvironmentSection { }

    public class ConfigurationSection: Node, CodeElementHolder<ConfigurationSectionHeader>, EnvironmentSection {
	    public ConfigurationSection(ConfigurationSectionHeader header): base(header) { }
	    public override string ID { get { return "configuration"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class SourceComputer: Node, CodeElementHolder<SourceComputerParagraph> {
	    public SourceComputer(SourceComputerParagraph paragraph): base(paragraph) { }
	    public override string ID { get { return "source-computer"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class ObjectComputer: Node, CodeElementHolder<ObjectComputerParagraph> {
	    public ObjectComputer(ObjectComputerParagraph paragraph): base(paragraph) { }
	    public override string ID { get { return "object-computer"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class SpecialNames: Node, CodeElementHolder<SpecialNamesParagraph> {
	    public SpecialNames(SpecialNamesParagraph paragraph): base(paragraph) { }
	    public override string ID { get { return "special-names"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class Repository: Node, CodeElementHolder<RepositoryParagraph> {
	    public Repository(RepositoryParagraph paragraph): base(paragraph) { }
	    public override string ID { get { return "repository"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class InputOutputSection: Node, CodeElementHolder<InputOutputSectionHeader>, EnvironmentSection {
	    public InputOutputSection(InputOutputSectionHeader header): base(header) { }
	    public override string ID { get { return "input-output"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }


    public class FileControlParagraphHeaderNode: Node, CodeElementHolder<FileControlParagraphHeader>, EnvironmentSection {
	    public FileControlParagraphHeaderNode(FileControlParagraphHeader header): base(header) { }
	    public override string ID { get { return "file-control"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class FileControlEntryNode: Node, CodeElementHolder<FileControlEntry>, EnvironmentSection {
	    public FileControlEntryNode(FileControlEntry entry): base(entry) { }
	    public override string ID { get { return "file-control"; } }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }



} // end of namespace TypeCobol.Compiler.Nodes
