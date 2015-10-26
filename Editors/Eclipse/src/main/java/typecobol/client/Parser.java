package typecobol.client;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.List;

public class Parser {

	private final String pipename;
	public List<CodeElement> elements;


	public Parser() {
		this.pipename = "testpipe";
	}

	public boolean parse(final String text) {
		elements = null;

		RandomAccessFile pipe = null;
		try { // connect to pipe
			final String path = "\\\\.\\pipe\\"+pipename;
			pipe = new RandomAccessFile(path, "rw"); //FNFException
		} catch (final FileNotFoundException ex) {
			System.err.println("Pipe \""+pipename+"\" not found.");
			return false;
		}

		boolean status = true;
		try { elements = new Client(pipe).sendrcv(text); }
		catch (final Exception ex) {
			System.err.println("Error sending/receiving data.");
			ex.printStackTrace();
			status = false;
		}
		try { pipe.close(); }
		catch (final IOException ex) {
			System.err.println("Error closing pipe \""+pipename+"\".");
			ex.printStackTrace();
		}
		return status;
	}
}
