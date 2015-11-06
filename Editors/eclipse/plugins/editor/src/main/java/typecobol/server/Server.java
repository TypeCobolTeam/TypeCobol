package typecobol.server;

import java.io.File;

public class Server extends Thread {

	static volatile boolean run = true;
	private Process process = null;

	@Override
	public void run() {
		final ProcessBuilder builder = createProcessBuilder(getExecutable());
//		final Map<String, String> environment = builder.environment();
//		for (final Entry<String, String> entry : environment.entrySet())
//			System.out.println(entry.getKey() + " : " + entry.getValue());
		try { process = builder.start(); }
		catch (final java.io.IOException ex) {
			ex.printStackTrace();
			cleanexit();
			return;
		}
		System.out.println("Server started.");
		final Runnable out = new Dump(process.getInputStream(), System.out);
		final Runnable err = new Dump(process.getErrorStream(), System.err);
		new Thread(out).start();
		new Thread(err).start();
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

	private static ProcessBuilder createProcessBuilder(final File path) {
		final String[] args = { path.getAbsolutePath(), "testpipe" };
		final ProcessBuilder builder = new ProcessBuilder(args);
		//final ProcessBuilder builder = new ProcessBuilder(path.getName());
		//builder.directory(path.getParentFile());
		return builder;
	}

	public static void main(final String[] args) {
		final Thread thread = new Server();
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
