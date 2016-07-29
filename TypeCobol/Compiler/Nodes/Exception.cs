namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public interface ExceptionCondition { }

public class OnSizeError: CodeElementNode<OnSizeErrorCondition>, ExceptionCondition {
	public OnSizeError(OnSizeErrorCondition condition): base(condition) { }
}
public class NoSizeError: CodeElementNode<NotOnSizeErrorCondition>, ExceptionCondition {
	public NoSizeError(NotOnSizeErrorCondition condition): base(condition) { }
}

public class OnAtEnd: CodeElementNode<AtEndCondition>, ExceptionCondition {
	public OnAtEnd(AtEndCondition condition): base(condition) { }
}
public class NoAtEnd: CodeElementNode<NotAtEndCondition>, ExceptionCondition {
	public NoAtEnd(NotAtEndCondition condition): base(condition) { }
}

public class OnException: CodeElementNode<OnExceptionCondition>, ExceptionCondition {
	public OnException(OnExceptionCondition condition): base(condition) { }
}
public class NoException: CodeElementNode<NotOnExceptionCondition>, ExceptionCondition {
	public NoException(NotOnExceptionCondition condition): base(condition) { }
}

public class OnInvalidKey: CodeElementNode<InvalidKeyCondition>, ExceptionCondition {
	public OnInvalidKey(InvalidKeyCondition condition): base(condition) { }
}
public class NoInvalidKey: CodeElementNode<NotInvalidKeyCondition>, ExceptionCondition {
	public NoInvalidKey(NotInvalidKeyCondition condition): base(condition) { }
}

public class OnOverflow: CodeElementNode<OnOverflowCondition>, ExceptionCondition {
	public OnOverflow(OnOverflowCondition condition): base(condition) { }
}
public class NoOverflow: CodeElementNode<NotOnOverflowCondition>, ExceptionCondition {
	public NoOverflow(NotOnOverflowCondition condition): base(condition) { }
}

} // end of namespace TypeCobol.Compiler.Nodes
