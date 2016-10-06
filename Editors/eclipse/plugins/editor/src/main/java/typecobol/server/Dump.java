package typecobol.server;

import java.io.*;

public class Dump implements Runnable {

	private final InputStream in;
	private final PrintStream out;

	public Dump(final InputStream in, final PrintStream out) {
		this.in = in;
		this.out = out;
	}

	@Override
	public void run() {
		final BufferedReader reader = new BufferedReader(new InputStreamReader((in)));
		String line = "";
		try {
			while ((line = reader.readLine()) != null) {
				out.println(line);
			}
		}
		catch (final IOException ex) { ex.printStackTrace(); }
		try { reader.close(); }
		catch (final IOException ex) { ex.printStackTrace(); }
	}
}
