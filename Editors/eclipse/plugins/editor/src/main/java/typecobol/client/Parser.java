package typecobol.client;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.List;

public class Parser {

	private final String pipename;
	public List<CodeElement> elements;


	public Parser() {
		this.pipename = typecobol.editors.eclipse.cobol.Editor.PIPE_NAME;
	}

	public boolean parse(final String text) {
		elements = null;
		final String path = "\\\\.\\pipe\\"+pipename;
		RandomAccessFile pipe = null;
		while(pipe == null) {
			try { pipe = new RandomAccessFile(path, "rw"); } // connect to pipe
			catch (final FileNotFoundException fnfex) { } // pipe not yet open
			finally {
				if (pipe == null) { // not connected, wait a bit
					System.out.println("Waiting for pipe \""+pipename+"\"...");
					try { Thread.sleep(100); }
					catch (final InterruptedException iex) { iex.printStackTrace(); return false; }
				}
			}
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
