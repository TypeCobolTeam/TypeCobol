package typecobol.client;

import static org.msgpack.template.Templates.TInteger;
import static org.msgpack.template.Templates.TString;

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
			unpacker.readArrayBegin();
			token.family = TokenFamily.asEnum(TInteger.read(unpacker, null, required));
			token.begin  = TInteger.read(unpacker, null, required);
			token.length = TInteger.read(unpacker, null, required);
			token.line   = TInteger.read(unpacker, null, required);
			token.text   = TString.read(unpacker, null, required);
			unpacker.readArrayEnd();
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
	public static Template<Token> tToken = new TokenTemplate();
}
