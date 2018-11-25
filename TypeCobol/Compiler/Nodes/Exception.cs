namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public interface ExceptionCondition { }

    public class OnSizeError: GenericNode<OnSizeErrorCondition>, CodeElementHolder<OnSizeErrorCondition>, ExceptionCondition {
	    public OnSizeError(OnSizeErrorCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoSizeError: GenericNode<NotOnSizeErrorCondition>, CodeElementHolder<NotOnSizeErrorCondition>, ExceptionCondition {
	    public NoSizeError(NotOnSizeErrorCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnAtEnd: GenericNode<AtEndCondition>, CodeElementHolder<AtEndCondition>, ExceptionCondition {
	    public OnAtEnd(AtEndCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoAtEnd: GenericNode<NotAtEndCondition>, CodeElementHolder<NotAtEndCondition>, ExceptionCondition {
	    public NoAtEnd(NotAtEndCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnException: GenericNode<OnExceptionCondition>, CodeElementHolder<OnExceptionCondition>, ExceptionCondition {
	    public OnException(OnExceptionCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoException: GenericNode<NotOnExceptionCondition>, CodeElementHolder<NotOnExceptionCondition>, ExceptionCondition {
	    public NoException(NotOnExceptionCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnInvalidKey: GenericNode<InvalidKeyCondition>, CodeElementHolder<InvalidKeyCondition>, ExceptionCondition {
	    public OnInvalidKey(InvalidKeyCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoInvalidKey: GenericNode<NotInvalidKeyCondition>, CodeElementHolder<NotInvalidKeyCondition>, ExceptionCondition {
	    public NoInvalidKey(NotInvalidKeyCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

    public class OnOverflow: GenericNode<OnOverflowCondition>, CodeElementHolder<OnOverflowCondition>, ExceptionCondition {
	    public OnOverflow(OnOverflowCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }
    public class NoOverflow: GenericNode<NotOnOverflowCondition>, CodeElementHolder<NotOnOverflowCondition>, ExceptionCondition {
	    public NoOverflow(NotOnOverflowCondition condition): base(condition) { }
        public override bool VisitNode(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this);
        }
    }

} // end of namespace TypeCobol.Compiler.Nodes
