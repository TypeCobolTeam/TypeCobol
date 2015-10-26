package typecobol.client;

import org.msgpack.template.Template;
import org.msgpack.packer.Packer;
import org.msgpack.unpacker.Unpacker;
import static org.msgpack.template.Templates.*;

// this annotation is necessary only if you don't
// use MessagePack.register(<this class>)
//@org.msgpack.annotation.Message
public class CodeElement {
	public CodeElementType type;
	public int begin;
	public int end;
	public int lineFirst;
	public int lineLast;
	public String text;

	@Override
	public String toString() {
		return "("+begin+"["+lineFirst+"]; "+end+"["+lineLast+"]) ["+type+": \""+text+"\"]";
	}

	public static class TokenTemplate implements Template<CodeElement> {

		@Override
		public CodeElement read(final Unpacker unpacker, final CodeElement token)
				throws java.io.IOException {
			return read(unpacker, token, false);
		}
		@Override
		public CodeElement read(final Unpacker unpacker, CodeElement token, final boolean required)
				throws java.io.IOException {
			token = new CodeElement();
			unpacker.readArrayBegin();
			token.type  = CodeElementType.asEnum(TString.read(unpacker, null, required));
			token.begin = TInteger.read(unpacker, null, required);
			token.end   = TInteger.read(unpacker, null, required);
			token.lineFirst = TInteger.read(unpacker, null, required);
			token.lineLast  = TInteger.read(unpacker, null, required);
			token.text  = TString.read(unpacker, null, required);
			unpacker.readArrayEnd();
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
	public static Template<CodeElement> tToken = new TokenTemplate();
}
