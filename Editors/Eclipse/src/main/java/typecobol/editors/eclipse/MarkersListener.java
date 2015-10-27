package typecobol.editors.eclipse;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;

public class MarkersListener implements IResourceChangeListener {

	@Override
	public void resourceChanged(final IResourceChangeEvent event) {
		if (event.getType() != IResourceChangeEvent.POST_CHANGE) return;
		final IResourceDelta resourcedelta = event.getDelta();
		if (resourcedelta.getKind() != IResourceDelta.CHANGED) return;
		final boolean[] okay = { false };
		final IResourceDeltaVisitor visitor = new IResourceDeltaVisitor() {
			@Override
			public boolean visit(final IResourceDelta delta) throws CoreException {
				if (delta.getKind() != IResourceDelta.CHANGED) return true;
				if ((delta.getFlags() & IResourceDelta.MARKERS) == 0) return true;
				int c = 0;
				for(final IMarkerDelta d: delta.getMarkerDeltas()) {
					final Object ve = d.getAttribute(IMarker.CHAR_START);
					if (ve == null) continue;
					final IMarker marker = d.getMarker();
					final Object vi = marker.exists() ? marker.getAttribute(IMarker.CHAR_START) : null;
					if (vi == null) continue;
					final IMarker[] markers = delta.getResource().findMarkers(IMarker.PROBLEM, false, IResource.DEPTH_ZERO);
					System.out.println("IResourceChangeListener.resourceChanged(): "+(c++)+": "+ve+" vs "+vi+" <-- updated marker, but too late 'cause there's now "+markers.length+" markers");
					dump(markers);
					okay[0] = true;
				}
				return true;
			}

			private void dump(IMarker[] markers) {

				for (final IMarker marker: markers) {
					try {
						System.out.println(marker.getAttribute(IMarker.LINE_NUMBER)+":"+marker.getAttribute(IMarker.CHAR_START)+">"+marker.getAttribute(IMarker.CHAR_END)+" "+marker.getAttribute("-tc-severity")+";"+marker.getAttribute("-tc-category")+";"+marker.getAttribute("-tc-code"));
					} catch (CoreException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				
			}
		};
		try { resourcedelta.accept(visitor); }
		catch (final CoreException ex) { ex.printStackTrace(); }
	}
}
