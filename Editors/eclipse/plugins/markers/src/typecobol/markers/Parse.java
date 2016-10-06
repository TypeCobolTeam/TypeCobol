package typecobol.markers;

import java.util.Map.Entry;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

public class Parse implements Task {

	@Override
	public void execute() {
		final String extension = "java";
		final java.util.Collection<IResource> resources = UIUtils.getActiveResources(extension);
		for (final IResource resource: resources) {
			try {
				final IMarker[] markers = resource.findMarkers(null, true, IResource.DEPTH_INFINITE);
				System.out.println("Found "+markers.length+" markers.");
				int c = 0;
				for (final IMarker marker: markers) {
//					for(final Entry<String, Object> entry: marker.getAttributes().entrySet())
//						System.out.println(entry.getKey()+"="+entry.getValue());
					if (c>0) marker.delete();
					c++;
				}
			} catch(final CoreException ex) { ex.printStackTrace(); }
		}
	}
}
