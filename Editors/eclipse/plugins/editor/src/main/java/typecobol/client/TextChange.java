package typecobol.client;

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
			return read(unpacker, change, true);
		}
		@Override
		public TextChange read(final Unpacker unpacker, TextChange change, final boolean required) throws IOException {
			change = new TextChange();
			unpacker.readArrayBegin();
			change.type = TextChangeType.asEnum(unpacker.readInt());
			change.line = unpacker.readInt();
			change.text = unpacker.readString();
			unpacker.readArrayEnd();
			return change;
		}

		@Override
		public void write(final Packer packer, final TextChange change) throws IOException {
			write(packer, change, true);
		}
		@Override
		public void write(final Packer packer, final TextChange change, final boolean required) throws IOException {
			packer.writeArrayBegin(3);
			packer.write(change.type.code);
			packer.write(change.line);
			packer.write(change.text);
			packer.writeArrayEnd();
		}

	}
	public static final Template<TextChange> TTextChange = new TextChangeTemplate();
}
