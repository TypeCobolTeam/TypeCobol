package typecobol.editors.eclipse;

import typecobol.client.CodeElement;
import typecobol.client.CodeElementsProvider;
import typecobol.client.Parser;

import java.util.List;

import org.eclipse.jface.text.*;

public class DocumentListener implements IDocumentListener, IDocumentPartitioningListener, CodeElementsProvider {

	private final Parser parser = new Parser();
	private final MarkerHandler handler;

	public DocumentListener(final MarkerHandler handler) {
		this.handler = handler;
	}

	@Override
	public void documentAboutToBeChanged(final DocumentEvent event) { }

	@Override
	public void documentChanged(final DocumentEvent event) {
		parser.parse(event.fDocument.get());
		if (parser.elements != null) {
			for (final CodeElement e: parser.elements) {
				if (e.errors.size() < 1) continue;

				int offset = -1;
				try { offset = event.fDocument.getLineOffset(e.lineFirst); }
				catch (final BadLocationException ex) { }
				handler.error(e, offset);
			}
		}
		else ;//TODO error marker parsing failed
	}

	@Override
	public void documentPartitioningChanged(final IDocument document) { }

	@Override
	public List<CodeElement> getCodeElements() { return parser.elements; }

}
