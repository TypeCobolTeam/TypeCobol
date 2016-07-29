namespace TypeCobol.Compiler.Nodes {

using TypeCobol.Compiler.CodeElements;

public class OnSizeError: CodeElementNode<OnSizeErrorCondition> {
	public OnSizeError(OnSizeErrorCondition condition): base(condition) { }
}
public class NoSizeError: CodeElementNode<NotOnSizeErrorCondition> {
	public NoSizeError(NotOnSizeErrorCondition condition): base(condition) { }
}

public class OnAtEnd: CodeElementNode<AtEndCondition> {
	public OnAtEnd(AtEndCondition condition): base(condition) { }
}
public class NoAtEnd: CodeElementNode<NotAtEndCondition> {
	public NoAtEnd(NotAtEndCondition condition): base(condition) { }
}

public class OnException: CodeElementNode<OnExceptionCondition> {
	public OnException(OnExceptionCondition condition): base(condition) { }
}
public class NoException: CodeElementNode<NotOnExceptionCondition> {
	public NoException(NotOnExceptionCondition condition): base(condition) { }
}

public class OnInvalidKey: CodeElementNode<InvalidKeyCondition> {
	public OnInvalidKey(InvalidKeyCondition condition): base(condition) { }
}
public class NoInvalidKey: CodeElementNode<NotInvalidKeyCondition> {
	public NoInvalidKey(NotInvalidKeyCondition condition): base(condition) { }
}

public class OnOverflow: CodeElementNode<OnOverflowCondition> {
	public OnOverflow(OnOverflowCondition condition): base(condition) { }
}
public class NoOverflow: CodeElementNode<NotOnOverflowCondition> {
	public NoOverflow(NotOnOverflowCondition condition): base(condition) { }
}

} // end of namespace TypeCobol.Compiler.Nodes
