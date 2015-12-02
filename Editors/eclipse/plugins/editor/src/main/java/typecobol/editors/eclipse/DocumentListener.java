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

		// diff.before can be uninitialized because we manually call
		// documentChanged (ie. this method) from Editor.doSetInput
		if (diff.before == null) diff.before = new Diff.Info(0);
		final TextChange[] changes = diff.createTextChanges();
		doParse(event.getDocument(), changes);
	}

	public static String toString(final DocumentEvent event) {
		final IDocument d = event.getDocument();
		final int lengthOfRemovedText = event.getLength();
		final int lengthOfInsertedText = event.getText().length();
		final int lengthOfUpdate = lengthOfRemovedText == 0 ? lengthOfInsertedText : lengthOfRemovedText;
	    int indexOfLastLine;
		int indexOfFirstLine;
		try { indexOfFirstLine = d.getLineOfOffset(event.getOffset()); }
		catch(final BadLocationException ex) { indexOfFirstLine = -1; }
		try { indexOfLastLine  = d.getLineOfOffset(Math.min(event.getOffset()+lengthOfUpdate, d.getLength())); }
		catch(final BadLocationException ex) { indexOfLastLine = -1; }
		final StringBuilder str = new StringBuilder(); str
			.append(" d.len="+d.getLength())
			.append(" e.os="+event.getOffset())
			.append(" @"+indexOfFirstLine+(indexOfLastLine>indexOfFirstLine?("-"+indexOfLastLine):""))
			.append(" e.len[deleted]="+event.getLength())
			.append(" e.text=\n\""+event.getText()+"\"(e.text.len[inserted]="+event.getText().length()+")")
		; return str.toString();
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
