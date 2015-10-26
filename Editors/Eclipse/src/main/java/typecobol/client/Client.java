package typecobol.client;

import java.io.*;
import java.util.List;

import org.msgpack.MessagePack;
import org.msgpack.packer.Packer;
import org.msgpack.unpacker.Unpacker;
import org.msgpack.template.Template;
import static org.msgpack.template.Templates.tList;

public class Client {

	final RandomAccessFile pipe;

	public Client(final RandomAccessFile pipe) {
		this.pipe = pipe;
	}

	public List<CodeElement> sendrcv(final String data) throws IOException {
		final MessagePack msgpack = new MessagePack();
		msgpack.register(Error.class);
		msgpack.register(CodeElementType.class);// if A has a field of class B,
		msgpack.register(CodeElement.class);   // B must be registered before A
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		final Packer packer = msgpack.createPacker(out);

		final int order = 66;// 66 = let's parse!
		packer.write(order);
		packer.write(data);
		this.pipe.write(out.toByteArray());
		packer.close();
		out.close();

		// read response
		final InputStream in = java.nio.channels.Channels.newInputStream(pipe.getChannel());
		final Unpacker unpacker = msgpack.createUnpacker(in);
		final Template<java.util.List<CodeElement>> ctemplate = tList(CodeElement.tToken);
		List<CodeElement> result = unpacker.read(ctemplate);
		System.out.println("--------------------\n"+data);
		System.out.println("----- Got: "+result.size()+" tokens.\n--------------------");
		//for(final Token t: result) System.out.println(t);
		unpacker.close();
		in.close();

		return result;
	}
}
