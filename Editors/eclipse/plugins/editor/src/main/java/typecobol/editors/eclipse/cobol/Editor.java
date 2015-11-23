package typecobol.editors.eclipse.cobol;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.editors.text.FileDocumentProvider;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;

import typecobol.client.Parser;
import typecobol.editors.eclipse.ColorMap;
import typecobol.editors.eclipse.DocumentListener;
import typecobol.editors.eclipse.MarkerCreator;
import typecobol.editors.eclipse.MarkersCleaner;

public class Editor extends TextEditor {

	public static final String PIPE_NAME = "TypeCobol.Eclipse";

	private final ColorMap colors;
	private final Parser parser = new Parser();
	private final DocumentListener listener;
	private final MarkerCreator handler = new MarkerCreator();
	private final MarkersCleaner cleaner = new MarkersCleaner();
	private final Scanner scanner;

	public Editor() {
		super();
		colors = new ColorMap();
		listener = new DocumentListener(parser, handler);
		scanner = new Scanner(listener, colors);
		setSourceViewerConfiguration(new Configuration(scanner));
		
		ResourcesPlugin.getWorkspace().addResourceChangeListener(cleaner);
	}

	@Override
	public void dispose() {
		colors.dispose();
		super.dispose();
	}

	private static IDocumentProvider createDocumentProvider(final IEditorInput input) {
		if (input instanceof org.eclipse.ui.part.FileEditorInput) {
			return new FileDocumentProvider();
		}
		if (input instanceof org.eclipse.ui.ide.FileStoreEditorInput) {
			return new TextFileDocumentProvider();// outside of workbench
		}
		throw new UnsupportedOperationException();
	}

	@Override
	protected void doSetInput(final IEditorInput input) throws CoreException {
		setDocumentProvider(createDocumentProvider(input));
		super.doSetInput(input);
		final IDocument document = getDocumentProvider().getDocument(input);

		org.eclipse.core.resources.IResource resource = null;
		String path = null;
		if (input instanceof org.eclipse.ui.part.FileEditorInput) {
			final IFile file = org.eclipse.ui.ide.ResourceUtil.getFile(input);
			path = file.getLocation().toString();
			resource = file;
		} else
		if (input instanceof org.eclipse.ui.ide.FileStoreEditorInput) {
			path = ((org.eclipse.ui.ide.FileStoreEditorInput)input).getURI().getPath();
			System.err.println("Cannot create resource from out-of-project file \""+path+"\".");
		}
		handler.input = resource;
		MarkersCleaner.deleteMarkers(resource);
		parser.initialize(path);

		if (document != null) {
			document.addDocumentListener(listener);
			listener.setDocumentPath(document, path);
			listener.documentChanged(new DocumentEvent(document, 0, document.get().length(), document.get()));
		}
	}
}
