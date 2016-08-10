namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public interface ExceptionCondition { }

public class OnSizeError: Node<OnSizeErrorCondition>, ExceptionCondition {
	public OnSizeError(OnSizeErrorCondition condition): base(condition) { }
}
public class NoSizeError: Node<NotOnSizeErrorCondition>, ExceptionCondition {
	public NoSizeError(NotOnSizeErrorCondition condition): base(condition) { }
}

public class OnAtEnd: Node<AtEndCondition>, ExceptionCondition {
	public OnAtEnd(AtEndCondition condition): base(condition) { }
}
public class NoAtEnd: Node<NotAtEndCondition>, ExceptionCondition {
	public NoAtEnd(NotAtEndCondition condition): base(condition) { }
}

public class OnException: Node<OnExceptionCondition>, ExceptionCondition {
	public OnException(OnExceptionCondition condition): base(condition) { }
}
public class NoException: Node<NotOnExceptionCondition>, ExceptionCondition {
	public NoException(NotOnExceptionCondition condition): base(condition) { }
}

public class OnInvalidKey: Node<InvalidKeyCondition>, ExceptionCondition {
	public OnInvalidKey(InvalidKeyCondition condition): base(condition) { }
}
public class NoInvalidKey: Node<NotInvalidKeyCondition>, ExceptionCondition {
	public NoInvalidKey(NotInvalidKeyCondition condition): base(condition) { }
}

public class OnOverflow: Node<OnOverflowCondition>, ExceptionCondition {
	public OnOverflow(OnOverflowCondition condition): base(condition) { }
}
public class NoOverflow: Node<NotOnOverflowCondition>, ExceptionCondition {
	public NoOverflow(NotOnOverflowCondition condition): base(condition) { }
}

} // end of namespace TypeCobol.Compiler.Nodes
