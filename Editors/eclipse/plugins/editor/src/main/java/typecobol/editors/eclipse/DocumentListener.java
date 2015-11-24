package typecobol.editors.eclipse;

import typecobol.client.CodeElement;
import typecobol.client.TokensProvider;
import typecobol.client.Parser;
import typecobol.client.TextChange;

import java.util.List;

import org.eclipse.jface.text.*;

public class DocumentListener implements IDocumentListener, IDocumentPartitioningListener, TokensProvider {

	private final Parser parser;
	private final MarkerCreator handler;

	private List<typecobol.client.Token> tokens = null;
	private java.util.Map<IDocument,String> paths = new java.util.HashMap<IDocument,String>();
	private Diff diff = new Diff();

	public DocumentListener(final Parser parser, final MarkerCreator handler) {
		this.parser = parser;
		this.handler = handler;
	}

	@Override
	public void documentAboutToBeChanged(final DocumentEvent event) {
		diff = new Diff();
		diff.set(Diff.BEFORE, event);
	}

	@Override
	public void documentChanged(final DocumentEvent event) {
		diff.set(Diff.AFTER, event);
		System.out.println(diff);

		final TextChange[] changes = diff.createTextChanges();
//		for (final TextChange change: changes) doParse(event.getDocument(), new TextChange[] { change });
		doParse(event.getDocument(), changes);
	}

	private void doParse(final IDocument document, final TextChange[] changes) {
		parser.parse(paths.get(document), changes);
		tokens = createTokens();
		handleErrors(document);
	}

	private void handleErrors(final IDocument document) {
		for (final CodeElement e: parser.elements) {
			if (e.errors.size() < 1) continue;
			int offset = -1;
			try { offset = document.getLineOffset(e.lineFirst); }
			catch (final BadLocationException ex) { ex.printStackTrace(); }
			handler.error(e, offset);
		}
	}



	@Override
	public void documentPartitioningChanged(final IDocument document) { }

	private List<typecobol.client.Token> createTokens() {
		final List<typecobol.client.Token> tokens = new java.util.ArrayList<typecobol.client.Token>();
		if (parser.elements == null) return tokens;
		for (final CodeElement e: parser.elements) tokens.addAll(e.tokens);
		return tokens;
	}

	@Override
	public List<typecobol.client.Token> getTokens() {
		return tokens;
	}

	public void setDocumentPath(final IDocument document, final String path) {
		paths .put(document, path);
	}

}
