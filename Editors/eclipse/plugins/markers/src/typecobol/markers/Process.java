package typecobol.markers;

public class Process implements Runnable {

	public final int delayMs = 1000;
	private final int maxWaitMs = 100;

	private final Task task;
	private final Thread thread;
	private boolean stop = false;

	public Process(final Task task) {
		this.task = task;
		thread = new Thread(this);
	}

	public void start() { thread.start(); }
	public void stop() {
		try {
			stop = true;
			thread.join(maxWaitMs);
			if (thread.isAlive()) {
				thread.interrupt();
				thread.join();
			}
		} catch (final InterruptedException ex ) { ex.printStackTrace(); }
	}

	@Override
	public void run() {
		while (!stop) {
			task.execute();
			try { Thread.sleep(delayMs); }
			catch (final InterruptedException ex ) { stop = true; } 
		}
	}

}
