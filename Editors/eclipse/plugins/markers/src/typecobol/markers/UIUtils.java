package typecobol.markers;

import java.util.*;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.ui.*;

public class UIUtils {

	public static Collection<IResource> getActiveResources(final String extension) {
		final IWorkbenchWindow window = getWorkbenchWindow();
		return getResources(window, extension);
	}

	private static Collection<IResource> getResources(final IWorkbenchWindow window, final String extension) {
		final List<IResource> resources = new ArrayList<IResource>();
		if (window == null) return resources;
		final IWorkbenchPage page = window.getActivePage();
		// page.getActiveEditor() might not suffice
		IEditorReference[] editors = page.getEditorReferences();
		for(final IEditorReference reference: editors) {
			IResource resource = null;
			try { resource = getResource(reference.getEditorInput()); }
			catch (final PartInitException ex) { ex.printStackTrace(); }
			if (resource == null) continue;
			if (resource.getFileExtension().equals(extension))
				resources.add(resource);
		}
		return resources;
	}

	private static IResource getResource(final IEditorInput input) {
		if (input instanceof org.eclipse.ui.part.FileEditorInput) {
			final IFile file = org.eclipse.ui.ide.ResourceUtil.getFile(input);
			//final String path = file.getLocation().toString();
			return file;
		} else
		if (input instanceof org.eclipse.ui.ide.FileStoreEditorInput) {
			final String path = ((org.eclipse.ui.ide.FileStoreEditorInput)input).getURI().getPath();
			System.err.println("Cannot create resource from out-of-project file \""+path+"\".");
		}
		return null;
	}

	private static IWorkbenchWindow getWorkbenchWindow() {
		final IWorkbench workbench = PlatformUI.getWorkbench();
		IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
		if (window != null) return window;
		if (workbench.getWorkbenchWindowCount() > 0)
			return workbench.getWorkbenchWindows()[0];
		return null;
	}

}
