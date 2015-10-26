package typecobol.editors.eclipse.cobol;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.editors.text.FileDocumentProvider;
import org.eclipse.ui.editors.text.TextEditor;
import typecobol.editors.eclipse.ColorMap;
import typecobol.editors.eclipse.MarkerHandler;

public class Editor extends TextEditor {

	private ColorMap colors;
	private MarkerHandler handler = new MarkerHandler();

	public Editor() {
		super();
		colors = new ColorMap();
		setSourceViewerConfiguration(new Configuration(handler, colors));
		setDocumentProvider(new FileDocumentProvider());
	}

	@Override
	public void dispose() {
		colors.dispose();
		super.dispose();
	}

	@Override
	protected void doSetInput(final IEditorInput input) throws CoreException
	{
		super.doSetInput(input);
		IFile file = org.eclipse.ui.ide.ResourceUtil.getFile(input);
		handler.input = file;
		handler.deleteMarkers();
	}
}
