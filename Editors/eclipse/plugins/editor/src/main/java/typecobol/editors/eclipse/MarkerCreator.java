package typecobol.editors.eclipse;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import typecobol.client.CodeElement;
import typecobol.client.Error;

public class MarkerCreator {

	private static final String TYPECOBOL_SEVERITY = "-tc-severity";
	private static final String TYPECOBOL_CATEGORY = "-tc-category";
	private static final String TYPECOBOL_CODE     = "-tc-code";

	public IResource input;

	public void error(final CodeElement element, final int offset) {
		try { markError(element, offset); }
		catch (final CoreException ex) { ex.printStackTrace(); }
	}

	private void markError(final CodeElement element, final int offset) throws CoreException {
		final IMarker[] markers = input.findMarkers(IMarker.PROBLEM, false, IResource.DEPTH_ZERO);
		for(final typecobol.client.Error error: element.errors) {
			if (!exists(markers, error, element.lineFirst/*, offset*/)) createMarker(error, element.lineFirst, offset);
		}
	}

	private static boolean exists(final IMarker[] markers, final Error error, final int line/*, final int offset*/) throws CoreException {
//		final int start = offset + error.begin;
//		final int end   = offset + error.end;
		for (final IMarker marker: markers) {
			/*System.out.println(start+">"+end+"@"+line+" vs "
		+marker.getId()+":"+marker.getAttribute(IMarker.CHAR_START)+">"+marker.getAttribute(IMarker.CHAR_END)+"@"+marker.getAttribute(IMarker.LINE_NUMBER)
		+" <-- old marker, real position tracked by eclipse");*/
			if ((int)marker.getAttribute(IMarker.LINE_NUMBER) != line) continue;
			//if ((int)marker.getAttribute(IMarker.CHAR_START) != start) continue;
			//if ((int)marker.getAttribute(IMarker.CHAR_END)   != end)   continue;
			if ((int)marker.getAttribute(TYPECOBOL_SEVERITY) != error.severity) continue;
			if ((int)marker.getAttribute(TYPECOBOL_CATEGORY) != error.category) continue;
			if ((int)marker.getAttribute(TYPECOBOL_CODE)     != error.code)     continue;
			return true;
		}
		return false;
	}

	private IMarker createMarker(final Error error, final int line, final int offset) throws CoreException {
		final IMarker marker = input.createMarker(IMarker.PROBLEM);
		if (!marker.exists()) return null;
		java.util.Map<String,Object> attrs = new java.util.HashMap<String,Object>();
		attrs.put(IMarker.PRIORITY, IMarker.PRIORITY_NORMAL);
		attrs.put(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
		attrs.put(TYPECOBOL_SEVERITY, error.severity);
		attrs.put(TYPECOBOL_CATEGORY, error.category);
		attrs.put(TYPECOBOL_CODE,     error.code);
		attrs.put(IMarker.MESSAGE,    error.message);
		attrs.put(IMarker.CHAR_START, offset + error.begin);
		attrs.put(IMarker.CHAR_END,   offset + error.end);
//		attrs.put(IMarker.LINE_NUMBER, line);
		marker.setAttributes(attrs);
		// IResourceChangeListeners are notified when setAttribute(key,value) is used
		// but they are NOT notified when setAttributes(attributes) is used. T_T
		marker.setAttribute(IMarker.LINE_NUMBER, line);

		return marker;
	}
}
