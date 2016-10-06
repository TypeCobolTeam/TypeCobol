package typecobol.client;

import org.msgpack.packer.Packer;
import org.msgpack.template.Template;
import org.msgpack.unpacker.Unpacker;



public class Token {
	public TokenFamily family;
	public int begin;
	public int length;
	public int line;
	public String text;

	@Override
	public String toString() {
		return "("+begin+";"+length+")@"+line+" ["+family+": \""+text+"\"]";
	}



	public static class TokenTemplate implements Template<Token> {

		@Override
		public Token read(final Unpacker unpacker, final Token token)
				throws java.io.IOException {
			return read(unpacker, token, false);
		}
		@Override
		public Token read(final Unpacker unpacker, Token token, final boolean required)
				throws java.io.IOException {
			token = new Token();
			unpacker.readMapBegin(); //give me 5
			unpacker.readString();
			token.family = TokenFamily.asEnum(unpacker.readInt());
			unpacker.readString();
			token.begin  = unpacker.readInt();
			unpacker.readString();
			token.length = unpacker.readInt();
			unpacker.readString();
			token.line   = unpacker.readInt();
			unpacker.readString();
			token.text   = unpacker.readString();
			unpacker.readMapEnd();
			return token;
		}

		@Override
		public void write(final Packer packer, final Token token)
				throws java.io.IOException {
			write(packer, token, false);
		}
		@Override
		public void write(final Packer packer, final Token token, final boolean required)
				throws java.io.IOException {
			throw new UnsupportedOperationException("TODO");
		}

	}
	public static final Template<Token> TToken = new TokenTemplate();
}
