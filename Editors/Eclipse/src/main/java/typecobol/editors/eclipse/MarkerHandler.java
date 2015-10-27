package typecobol.editors.eclipse;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import typecobol.client.CodeElement;
import typecobol.client.Error;

public class MarkerHandler {

	private static final String TYPECOBOL_SEVERITY = "-tc-severity";
	private static final String TYPECOBOL_CATEGORY = "-tc-category";
	private static final String TYPECOBOL_CODE     = "-tc-code";

	public IResource input;


	public void deleteMarkers() {
		int depth = org.eclipse.core.resources.IResource.DEPTH_INFINITE;
		try { input.deleteMarkers(IMarker.PROBLEM, true, depth); }
		catch (CoreException ex) { ex.printStackTrace(); }
	}

	public void error(final CodeElement element, final int offset) {
		try { markError(element, offset); }
		catch (final CoreException ex) { ex.printStackTrace(); }
	}

	private void markError(final CodeElement element, final int offset) throws CoreException {
		final IMarker[] markers = input.findMarkers(IMarker.PROBLEM, false, IResource.DEPTH_ZERO);
		//System.err.println("Already "+markers.length+" markers.");
		//for(final IMarker marker : markers) System.err.println(marker.getType());
		for(final typecobol.client.Error error: element.errors) {
			if (!exists(markers, error, offset)) createMarker(error, offset);
		}
	}

	private static boolean exists(final IMarker[] markers, final Error error, final int offset) throws CoreException {
		final int start = offset + error.begin;
		final int end   = offset + error.end;
		for (final IMarker marker: markers) {
			//java.util.Map<String, Object> attrs = marker.getAttributes();
			//for(final String key: attrs.keySet()) System.out.println(key+"="+attrs.get(key));
			System.out.println("MarkerHandler.exists()? "+(int)marker.getAttribute(IMarker.CHAR_START) +" vs "+ start+" <-- dirty marker");
			if ((int)marker.getAttribute(IMarker.CHAR_START) != start) continue;
			if ((int)marker.getAttribute(IMarker.CHAR_END)   != end)   continue;
			if ((int)marker.getAttribute(TYPECOBOL_SEVERITY) != error.severity) continue;
			if ((int)marker.getAttribute(TYPECOBOL_CATEGORY) != error.severity) continue;
			if ((int)marker.getAttribute(TYPECOBOL_CODE)     != error.code)     continue;
			System.out.println("marker exists");
			return true;
		}
		return false;
	}

	private IMarker createMarker(final Error error, final int offset) throws CoreException {
		final IMarker marker = input.createMarker(IMarker.PROBLEM);
		if (!marker.exists()) return null;
		marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_NORMAL);
		marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
		marker.setAttribute(TYPECOBOL_SEVERITY, error.severity);
		marker.setAttribute(TYPECOBOL_CATEGORY, error.category);
		marker.setAttribute(TYPECOBOL_CODE,     error.code);
		marker.setAttribute(IMarker.MESSAGE,    error.message);
		//marker.setAttribute(IMarker.LINE_NUMBER, Integer.valueOf(line));
		marker.setAttribute(IMarker.CHAR_START, offset + error.begin);
		marker.setAttribute(IMarker.CHAR_END,   offset + error.end);
		return marker;
	}
}
