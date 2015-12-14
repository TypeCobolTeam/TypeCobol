package typecobol.markers;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class Activator extends AbstractUIPlugin
	implements org.eclipse.ui.IStartup { //TODO not a really clean way to initialize this plug-in...

	public static final String PLUGIN_ID = "TypeCobol.Markers";
	private static Activator plugin;
	private Process subprocess;

	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		final Task task = new Parse();
		subprocess = new Process(task);
		subprocess.start();
	}

	@Override
	public void stop(final BundleContext context) throws Exception {
		subprocess.stop();
		plugin = null;
		super.stop(context);
	}

	public static Activator getDefault() { return plugin; }

	@Override
	public void earlyStartup() { } //DO nothing
}
