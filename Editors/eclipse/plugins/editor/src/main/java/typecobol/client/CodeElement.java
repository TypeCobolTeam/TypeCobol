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
		public CodeElement read(final Unpacker unpacker, final CodeElement token)
				throws java.io.IOException {
			return read(unpacker, token, false);
		}
		@Override
		public CodeElement read(final Unpacker unpacker, CodeElement token, final boolean required)
				throws java.io.IOException {
			token = new CodeElement();
			int size = unpacker.readMapBegin();

			unpacker.readString();
			token.type  = CodeElementType.asEnum(unpacker.readString());
			unpacker.readString();
			token.begin = unpacker.readInt();
			unpacker.readString();
			token.end   = unpacker.readInt();
			unpacker.readString();
			token.lineFirst = unpacker.readInt();
			unpacker.readString();
			token.lineLast  = unpacker.readInt();

			unpacker.readString();//"Tokens"
			size = unpacker.readArrayBegin();
			token.tokens = new ArrayList<Token>();
			for (int c=0; c<size; c++) token.tokens.add(unpacker.read(Token.TToken)); 
			unpacker.readArrayEnd();

			unpacker.readString();//"Errors"
			token.errors = new ArrayList<Error>();
			if (unpacker.getNextType() == org.msgpack.type.ValueType.ARRAY) {
				size = unpacker.readArrayBegin();
				for (int c=0; c<size; c++) token.errors.add(unpacker.read(Error.TError));
				unpacker.readArrayEnd();
			} // else this CodeElement has no error

			unpacker.readMapEnd();
			return token;
		}

		@Override
		public void write(final Packer packer, final CodeElement token)
				throws java.io.IOException {
			write(packer, token, false);
		}
		@Override
		public void write(final Packer packer, final CodeElement token, final boolean required)
				throws java.io.IOException {
			throw new UnsupportedOperationException("TODO");
		}

	}
	public static final Template<CodeElement> TCodeElement = new CodeElementTemplate();
}
