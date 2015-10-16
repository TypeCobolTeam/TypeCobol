package typecobol.editors.eclipse;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

public class MarkerHandler {

	public IResource input;



	public void deleteMarkers() {
		int depth = org.eclipse.core.resources.IResource.DEPTH_INFINITE;
		try { input.deleteMarkers(IMarker.PROBLEM, true, depth); }
		catch (CoreException ex) { ex.printStackTrace(); }
	}

	public void error(final int startregion, final int endregion) {
		try {
			final IMarker marker = input.createMarker(IMarker.PROBLEM);
			if (marker.exists()) {
				marker.setAttribute(IMarker.MESSAGE, "A sample marker message");
				marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_NORMAL);
				marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
				marker.setAttribute(IMarker.MESSAGE, "Uh-oh!");
				marker.setAttribute("data", "bool|int|string|?");
				//marker.setAttribute(IMarker.LINE_NUMBER, Integer.valueOf(line));
				marker.setAttribute(IMarker.CHAR_START, startregion);
				marker.setAttribute(IMarker.CHAR_END, endregion);
			}
		} catch (CoreException ex) { ex.printStackTrace(); }
	}
}
