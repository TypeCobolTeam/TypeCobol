package typecobol.editors.eclipse.cobol;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.editors.text.FileDocumentProvider;
import org.eclipse.ui.editors.text.TextEditor;
import typecobol.editors.eclipse.ColorMap;
import typecobol.editors.eclipse.DocumentListener;
import typecobol.editors.eclipse.MarkerCreator;
import typecobol.editors.eclipse.MarkersCleaner;

public class Editor extends TextEditor {

	private final ColorMap colors;
	private final MarkerCreator handler = new MarkerCreator();
	private final DocumentListener listener = new DocumentListener(handler);
	private final MarkersCleaner cleaner = new MarkersCleaner();
	private final Scanner scanner;

	public Editor() {
		super();
		colors = new ColorMap();
		scanner = new Scanner(listener, colors);
		setSourceViewerConfiguration(new Configuration(scanner));
		setDocumentProvider(new FileDocumentProvider());
		
		ResourcesPlugin.getWorkspace().addResourceChangeListener(cleaner);
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
		final IDocument document = getDocumentProvider().getDocument(input);
		final IFile file = org.eclipse.ui.ide.ResourceUtil.getFile(input);
		handler.input = file;
		MarkersCleaner.deleteMarkers(file);

		document.addDocumentListener(listener);
		listener.documentChanged(new DocumentEvent(document, 0, document.get().length(), document.get()));
	}
}
