namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public interface ExceptionCondition { }

    public class OnSizeError: GenericNode<OnSizeErrorCondition>, ExceptionCondition {
	    public OnSizeError(OnSizeErrorCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoSizeError: GenericNode<NotOnSizeErrorCondition>, ExceptionCondition {
	    public NoSizeError(NotOnSizeErrorCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnAtEnd: GenericNode<AtEndCondition>, ExceptionCondition {
	    public OnAtEnd(AtEndCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoAtEnd: GenericNode<NotAtEndCondition>, ExceptionCondition {
	    public NoAtEnd(NotAtEndCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnException: GenericNode<OnExceptionCondition>, ExceptionCondition {
	    public OnException(OnExceptionCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoException: GenericNode<NotOnExceptionCondition>, ExceptionCondition {
	    public NoException(NotOnExceptionCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnInvalidKey: GenericNode<InvalidKeyCondition>, ExceptionCondition {
	    public OnInvalidKey(InvalidKeyCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoInvalidKey: GenericNode<NotInvalidKeyCondition>, ExceptionCondition {
	    public NoInvalidKey(NotInvalidKeyCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnOverflow: GenericNode<OnOverflowCondition>, ExceptionCondition {
	    public OnOverflow(OnOverflowCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoOverflow: GenericNode<NotOnOverflowCondition>, ExceptionCondition {
	    public NoOverflow(NotOnOverflowCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

} // end of namespace TypeCobol.Compiler.Nodes
