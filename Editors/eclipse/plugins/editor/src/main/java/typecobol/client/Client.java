package typecobol.client;

import java.io.*;
import java.util.*;

import org.msgpack.MessagePack;
import org.msgpack.packer.Packer;
import org.msgpack.unpacker.Unpacker;
import org.msgpack.template.Template;
import static org.msgpack.template.Templates.*;

public class Client {

	private final RandomAccessFile pipe;
	private final MessagePack msgpack;

	public Client(final RandomAccessFile pipe) {
		this.pipe = pipe;

		msgpack = new MessagePack();
		 // if A has a field of class B,
		// B must be registered before A
		msgpack.register(Error.class);
		msgpack.register(TokenFamily.class);
		msgpack.register(Token.class);
		msgpack.register(CodeElementType.class);
		msgpack.register(CodeElement.class);
		msgpack.register(TextChangeType.class);
		msgpack.register(TextChange.class);
	}

	public boolean initialize(final String path) throws IOException {
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		final Packer packer = msgpack.createPacker(out);

		final int order = 67;
		packer.write(order);
		packer.write(path);
		packer.close();
		this.pipe.write(out.toByteArray());
		out.close();

		// read response
		final InputStream in = java.nio.channels.Channels.newInputStream(pipe.getChannel());
		final Unpacker unpacker = msgpack.createUnpacker(in);
		final int status = unpacker.read(TInteger);
		System.out.println("Executed command:"+order+" status:"+status);
		unpacker.close();
		in.close();

		return status == 0;
	}

	public List<CodeElement> parse(final String path, final TextChange[] changes) throws IOException {
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		final Packer packer = msgpack.createPacker(out);

		final int order = 66;// 66 = let's parse!
		packer.write(order);
		packer.write(path);

		Map<String,List<TextChange>> map = new HashMap<String,List<TextChange>>();
		map.put("Events", Arrays.asList(changes));
		tMap(TString, tList(TextChange.TTextChange)).write(packer, map);
		packer.close();
		this.pipe.write(out.toByteArray());
		out.close();
		// read response
		final InputStream in = java.nio.channels.Channels.newInputStream(pipe.getChannel());
		final Unpacker unpacker = msgpack.createUnpacker(in);
		final int status = unpacker.read(TInteger);
		System.out.println("Executed command:"+order+" status:"+status);
		//final Template<java.util.List<CodeElement>> ctemplate = tList(CodeElement.TCodeElement);
		final Template<Map<String,List<CodeElement>>> ctemplate = tMap(TString,tList(CodeElement.TCodeElement));
		Map<String,List<CodeElement>> result = unpacker.read(ctemplate);
		unpacker.close();
		in.close();

		return result.get("CodeElements");
	}
}
