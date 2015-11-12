package typecobol.server;

import java.io.File;

public class Server extends Thread {

	static volatile boolean run = false;
	private Process process = null;
	private final String name;

	public Server(final String name) { this.name = name; }

	@Override
	public void run() {
		final ProcessBuilder builder = createProcessBuilder(getExecutable());
		try { process = builder.start(); }
		catch (final java.io.IOException ex) {
			ex.printStackTrace();
			cleanexit();
			return;
		}
		System.out.println("Server started. Listening on \""+name+"\".");
		final Runnable out = new Dump(process.getInputStream(), System.out);
		final Runnable err = new Dump(process.getErrorStream(), System.err);
		new Thread(out).start();
		new Thread(err).start();
		run = true;
		while(run) {
			try { sleep(500); }
			catch (final InterruptedException ex) { }//DO nothing
		}
		cleanexit();
	}

	private void cleanexit() {
		if (process != null) {
			process.destroy();
			process = null;
			System.out.println("Server killed.");
		}
		System.out.println("Server finished.");
	}

	private static File getExecutable() {
		final File home = new File(System.getProperty("user.home"));
		final File root = home.getParentFile().getParentFile();
		final File project = new File(root, "sources"+File.separator+"TypeCobol");
		final File path = new File(project, "TypeCobol.Server"+File.separator+"bin"+File.separator+"Debug");
		return new File(path, "TypeCobol.Server.exe");
	}

	private ProcessBuilder createProcessBuilder(final File path) {
		final String[] args = { path.getAbsolutePath(), name };
		final ProcessBuilder builder = new ProcessBuilder(args);
		return builder;
	}

	public static void main(final String[] args) {
		final String name;
		if (args != null && args.length > 0) 
			 name = args[0];
		else name = typecobol.editors.eclipse.cobol.Editor.PIPE_NAME;
		final Thread thread = new Server(name);
		thread.start();
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				System.out.println("Server shutting down...");
				run = false;
				thread.interrupt();
				try { thread.join(); }
				catch (final InterruptedException ex) { } //DO nothing
		    }
		});
	}
}
