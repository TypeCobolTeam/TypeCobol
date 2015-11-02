package typecobol.editors.eclipse;

import java.util.Set;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

public class MarkersCleaner implements IResourceChangeListener {

	public static void deleteMarkers(final IResource input) {
		if (input != null) {
			try { input.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE); }
			catch (CoreException ex) { ex.printStackTrace(); }
		}
	}



	@Override
	public void resourceChanged(final IResourceChangeEvent event) {
		if (event.getType() != IResourceChangeEvent.POST_CHANGE) return;
		final IResourceDelta resourcedelta = event.getDelta();
		if (resourcedelta.getKind() != IResourceDelta.CHANGED) return;
		final IResourceDeltaVisitor visitor = new IResourceDeltaVisitor() {
			@Override
			public boolean visit(final IResourceDelta delta) throws CoreException {
				if (delta.getKind() != IResourceDelta.CHANGED) return true;
				if ((delta.getFlags() & IResourceDelta.MARKERS) == 0) return true;
				for(final IMarkerDelta d: delta.getMarkerDeltas()) {
					final Object ve = d.getAttribute(IMarker.CHAR_START);
					if (ve == null) continue;
					final IMarker marker = d.getMarker();
					final Object vi = marker.exists() ? marker.getAttribute(IMarker.CHAR_START) : null;
					if (vi == null) continue;
					final IMarker[] markers = delta.getResource().findMarkers(IMarker.PROBLEM, false, IResource.DEPTH_ZERO);
					//System.out.println("MarkersListener.resourceChanged(): "+d.getId()+" <-- new marker wrongly created, so there's now "+markers.length+" markers");
					asyncRemoveDuplicateMarkers(markers);
				}
				return true;
			}

			private void asyncRemoveDuplicateMarkers(final IMarker[] markers) throws CoreException {
				final java.util.Set<IMarker> duplicates = new java.util.HashSet<IMarker>();
				final java.util.Set<IMarker> lump = new java.util.HashSet<IMarker>();
				for (final IMarker i : markers) {
					if (contains(lump,i)) duplicates.add(i);
					lump.add(i);
				}
				Job job = new Job("MarkerMrProper") {
					@Override
					protected IStatus run(IProgressMonitor arg0) {
						try { for(final IMarker m: duplicates) m.delete(); }
						catch (final CoreException ex ) { return null; }
						return Status.OK_STATUS;
					}
				};
				job.setPriority(Job.SHORT);
				job.schedule();
			}

			private boolean contains(final Set<IMarker> markers, final IMarker marker) throws CoreException {
				for (final IMarker m: markers) {
					if (((int)m.getAttribute(IMarker.CHAR_START) == (int)marker.getAttribute(IMarker.CHAR_START))
					 && ((int)m.getAttribute(IMarker.CHAR_END)   == (int)marker.getAttribute(IMarker.CHAR_END))) return true;
				}
				return false;
			}
		};
		try { resourcedelta.accept(visitor); }
		catch (final CoreException ex) { ex.printStackTrace(); }
	}
}
