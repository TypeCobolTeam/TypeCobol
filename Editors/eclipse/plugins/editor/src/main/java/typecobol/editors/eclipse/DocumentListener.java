package typecobol.editors.eclipse;

import typecobol.client.CodeElement;
import typecobol.client.TokensProvider;
import typecobol.client.Parser;
import typecobol.client.TextChange;
import typecobol.client.TextChangeType;

import java.util.List;

import org.eclipse.jface.text.*;

public class DocumentListener implements IDocumentListener, IDocumentPartitioningListener, TokensProvider {

	private final Parser parser;
	private final MarkerCreator handler;

	private List<typecobol.client.Token> tokens = null;
	private java.util.Map<IDocument,String> paths = new java.util.HashMap<IDocument,String>();

	public DocumentListener(final Parser parser, final MarkerCreator handler) {
		this.parser = parser;
		this.handler = handler;
	}

	@Override
	public void documentAboutToBeChanged(final DocumentEvent event) { }

	@Override
	public void documentChanged(final DocumentEvent event) {
		TextChange[] changes;
		try {
			changes = createTextChanges(event);
			parser.parse(paths.get(event.getDocument()), changes);
			int indexOfFirstLine = event.getDocument().getLineOfOffset(event.getOffset());
			tokens = createTokens(indexOfFirstLine);
		}
		catch (final BadLocationException ex) {
			System.err.println("Error calculating edition span.");
			changes = new TextChange[0];
		}

		if (parser.elements != null) {
			for (final CodeElement e: parser.elements) {
				if (e.errors.size() < 1) continue;

				int offset = -1;
				try { offset = event.fDocument.getLineOffset(e.lineFirst); }
				catch (final BadLocationException ex) { }
				handler.error(e, offset);
			}
		}
		else System.err.println("ERROR: error marker parsing failed");//TODO
	}



	private static TextChange[] createTextChanges(final DocumentEvent event) throws BadLocationException {
		final IDocument d = event.getDocument();
		final int lengthOfRemovedText = event.getLength();
		final int lengthOfInsertedText = event.getText().length();
		final int indexOfFirstLine = d.getLineOfOffset(event.getOffset());
		final int lengthOfUpdate = lengthOfRemovedText == 0 ? lengthOfInsertedText : lengthOfRemovedText;
		final int indexOfLastLine  = d.getLineOfOffset(Math.min(event.getOffset()+lengthOfUpdate, d.getLength()));
		System.out.println(">>> removed:"+lengthOfRemovedText+" inserted: "+lengthOfInsertedText
				+" @"+indexOfFirstLine+(indexOfLastLine!=indexOfFirstLine?("-"+indexOfLastLine):""));
		final TextChange[] changes = new TextChange[indexOfLastLine-indexOfFirstLine+1];
		for (int l=indexOfFirstLine; l<=indexOfLastLine; l++) {
			final TextChange change = new TextChange();
			change.line = 0;
			change.type = TextChangeType.LineInserted;
			final IRegion line = d.getLineInformation(l);
			change.text = d.get(line.getOffset(), line.getLength());
			changes[l-indexOfFirstLine] = change;
		}
		return changes;
	}



	@Override
	public void documentPartitioningChanged(final IDocument document) { }

	private List<typecobol.client.Token> createTokens(final int lineOffset) {
		final List<typecobol.client.Token> tokens = new java.util.ArrayList<typecobol.client.Token>();
		if (parser.elements == null) return tokens;
		for (final CodeElement e: parser.elements) {
			for(final typecobol.client.Token t: e.tokens) {
				t.line += lineOffset;
			}
			tokens.addAll(e.tokens);
		}
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
