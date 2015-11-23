package typecobol.client;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.List;

public class Parser {

	private final String pipename;
	public boolean initialized = false;
	public List<CodeElement> elements;


	public Parser() {
		this.pipename = typecobol.editors.eclipse.cobol.Editor.PIPE_NAME;
	}

	private static RandomAccessFile connect(final String path) {
		RandomAccessFile pipe = null;
		while (pipe == null) {
			try { pipe = new RandomAccessFile(path, "rw"); } // connect to pipe
			catch (final FileNotFoundException fnfex) { } // pipe not yet open
			finally {
				if (pipe == null) { // not connected, wait a bit
					System.out.println("Waiting for pipe \""+path+"\"...");
					try { Thread.sleep(100); }
					catch (final InterruptedException iex) { iex.printStackTrace(); return null; }
				}
			}
		}
		return pipe;
	}

	public boolean initialize(final String path) {
		RandomAccessFile pipe = connect("\\\\.\\pipe\\"+pipename);
		final Client client = new Client(pipe);
		boolean status = true;
		try { initialized = client.initialize(path); }
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

	public boolean parse(final String path, final TextChange[] changes) {
		elements = null;
		RandomAccessFile pipe = connect("\\\\.\\pipe\\"+pipename);
		final Client client = new Client(pipe);
		boolean status = true;
		try { elements = client.parse(path, changes); }
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
