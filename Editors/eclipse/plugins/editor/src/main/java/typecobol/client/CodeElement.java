package typecobol.client;

import org.msgpack.template.Template;
import org.msgpack.packer.Packer;
import org.msgpack.unpacker.Unpacker;

import java.util.List;
import java.util.ArrayList;

// this annotation is necessary only if you don't
// use MessagePack.register(<this class>)
//@org.msgpack.annotation.Message
public class CodeElement {
	public CodeElementType type;
	public int begin;
	public int end;
	public int lineFirst;
	public int lineLast;
	public List<Token> tokens;
	public List<Error> errors;

	@Override
	public String toString() {
		return "("+begin+"["+lineFirst+"]; "+end+"["+lineLast+"]) ["+type+"]";
	}

	public static class CodeElementTemplate implements Template<CodeElement> {

		@Override
		public CodeElement read(final Unpacker unpacker, final CodeElement e)
				throws java.io.IOException {
			return read(unpacker, e, true);
		}
		@Override
		public CodeElement read(final Unpacker unpacker, CodeElement e, final boolean required)
				throws java.io.IOException {
			e = new CodeElement();
			int size = unpacker.readMapBegin();

			unpacker.readString();
			e.type  = CodeElementType.asEnum(unpacker.readString());
			unpacker.readString();
			e.begin = unpacker.readInt();
			unpacker.readString();
			e.end   = unpacker.readInt();
			unpacker.readString();
			e.lineFirst = unpacker.readInt();
			unpacker.readString();
			e.lineLast  = unpacker.readInt();

			unpacker.readString();//"Tokens"
			size = unpacker.readArrayBegin();
			e.tokens = new ArrayList<Token>();
			for (int c=0; c<size; c++) e.tokens.add(unpacker.read(Token.TToken));
			unpacker.readArrayEnd(required);

			unpacker.readString();//"Errors"
			e.errors = new ArrayList<Error>();
			if (unpacker.getNextType() == org.msgpack.type.ValueType.ARRAY) {
				size = unpacker.readArrayBegin();
				for (int c=0; c<size; c++) e.errors.add(unpacker.read(Error.TError));
				unpacker.readArrayEnd(required);
			} else unpacker.readNil();

			unpacker.readMapEnd(required);
			return e;
		}

		@Override
		public void write(final Packer packer, final CodeElement e)
				throws java.io.IOException {
			write(packer, e, false);
		}
		@Override
		public void write(final Packer packer, final CodeElement e, final boolean required)
				throws java.io.IOException {
			throw new UnsupportedOperationException("TODO");
		}

	}
	public static final Template<CodeElement> TCodeElement = new CodeElementTemplate();
}
