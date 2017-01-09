namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public interface ExceptionCondition { }

    public class OnSizeError: Node, CodeElementHolder<OnSizeErrorCondition>, ExceptionCondition {
	    public OnSizeError(OnSizeErrorCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoSizeError: Node, CodeElementHolder<NotOnSizeErrorCondition>, ExceptionCondition {
	    public NoSizeError(NotOnSizeErrorCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnAtEnd: Node, CodeElementHolder<AtEndCondition>, ExceptionCondition {
	    public OnAtEnd(AtEndCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoAtEnd: Node, CodeElementHolder<NotAtEndCondition>, ExceptionCondition {
	    public NoAtEnd(NotAtEndCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnException: Node, CodeElementHolder<OnExceptionCondition>, ExceptionCondition {
	    public OnException(OnExceptionCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoException: Node, CodeElementHolder<NotOnExceptionCondition>, ExceptionCondition {
	    public NoException(NotOnExceptionCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnInvalidKey: Node, CodeElementHolder<InvalidKeyCondition>, ExceptionCondition {
	    public OnInvalidKey(InvalidKeyCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoInvalidKey: Node, CodeElementHolder<NotInvalidKeyCondition>, ExceptionCondition {
	    public NoInvalidKey(NotInvalidKeyCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnOverflow: Node, CodeElementHolder<OnOverflowCondition>, ExceptionCondition {
	    public OnOverflow(OnOverflowCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoOverflow: Node, CodeElementHolder<NotOnOverflowCondition>, ExceptionCondition {
	    public NoOverflow(NotOnOverflowCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

} // end of namespace TypeCobol.Compiler.Nodes
