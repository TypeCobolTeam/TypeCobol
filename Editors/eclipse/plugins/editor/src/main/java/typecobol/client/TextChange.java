package typecobol.client;

import static org.msgpack.template.Templates.TInteger;
import static org.msgpack.template.Templates.TString;

import java.io.IOException;
import org.msgpack.packer.Packer;
import org.msgpack.template.Template;
import org.msgpack.unpacker.Unpacker;

public class TextChange {

	public TextChangeType type;
	public int line;
	public String text;

	@Override
	public String toString() {
		return "@"+line+": "+type+" \""+text+"\"";
	}
	public static String toString(final TextChange[] changes) {
		final String separator = "\n";
		final StringBuilder str = new StringBuilder();
		for (final TextChange change: changes) str.append(change).append(separator);
		str.deleteCharAt(str.length()-separator.length());
		return str.toString();
	}

	public static class TextChangeTemplate implements Template<TextChange> {

		@Override
		public TextChange read(final Unpacker unpacker, TextChange change) throws IOException {
			return read(unpacker, change, false);
		}
		@Override
		public TextChange read(final Unpacker unpacker, TextChange change, final boolean required) throws IOException {
			change = new TextChange();
			change.type = TextChangeType.asEnum(TInteger.read(unpacker, null, required));
			change.line = TInteger.read(unpacker, null, required);
			change.text = TString.read(unpacker, null, required);
			return change;
		}

		@Override
		public void write(final Packer packer, final TextChange change) throws IOException {
			write(packer, change, false);
		}
		@Override
		public void write(final Packer packer, final TextChange change, final boolean required) throws IOException {
			TInteger.write(packer, change.type.code, required);
			TInteger.write(packer, change.line, required);
			TString.write(packer, change.text, required);
		}

	}
	public static final Template<TextChange> TTextChange = new TextChangeTemplate();
}
