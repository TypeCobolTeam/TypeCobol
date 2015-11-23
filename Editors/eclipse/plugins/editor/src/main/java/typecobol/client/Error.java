package typecobol.client;

import static org.msgpack.template.Templates.TInteger;
import static org.msgpack.template.Templates.TString;

import org.msgpack.packer.Packer;
import org.msgpack.template.Template;
import org.msgpack.unpacker.Unpacker;

public class Error {
	public int begin;
	public int end;
	public String message;
	public int severity;
	public int category;
	public int code;

	@Override
	public String toString() {
		return "("+begin+"; "+end+") ["+category+":"+code+":"+severity+"] "+message;
	}

	public static class ErrorTemplate implements Template<Error> {

		@Override
		public Error read(final Unpacker unpacker, final Error error)
				throws java.io.IOException {
			return read(unpacker, error, false);
		}
		@Override
		public Error read(final Unpacker unpacker, Error error, final boolean required)
				throws java.io.IOException {
			error = new Error();
			unpacker.readArrayBegin();
			error.begin = TInteger.read(unpacker, null, required);
			error.end   = TInteger.read(unpacker, null, required);
			error.message = TString.read(unpacker, null, required);
			error.severity = TInteger.read(unpacker, null, required);
			error.category = TInteger.read(unpacker, null, required);
			error.code     = TInteger.read(unpacker, null, required);
			unpacker.readArrayEnd();
			return error;
		}

		@Override
		public void write(final Packer packer, final Error error)
				throws java.io.IOException {
			write(packer, error, false);
		}
		@Override
		public void write(final Packer packer, final Error error, final boolean required)
				throws java.io.IOException {
			throw new UnsupportedOperationException("TODO");
		}

	}
	public static final Template<Error> TError = new ErrorTemplate();
}
