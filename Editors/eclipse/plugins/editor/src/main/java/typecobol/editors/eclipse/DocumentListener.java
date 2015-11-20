package typecobol.editors.eclipse;

import typecobol.client.CodeElement;
import typecobol.client.TokensProvider;
import typecobol.client.Parser;

import java.util.List;

import org.eclipse.jface.text.*;

public class DocumentListener implements IDocumentListener, IDocumentPartitioningListener, TokensProvider {

	private final Parser parser;
	private final MarkerCreator handler;

	private List<typecobol.client.Token> tokens = null;

	public DocumentListener(final Parser parser, final MarkerCreator handler) {
		this.parser = parser;
		this.handler = handler;
	}

	@Override
	public void documentAboutToBeChanged(final DocumentEvent event) { }

	@Override
	public void documentChanged(final DocumentEvent event) {
		String text;
		int firstLineIndex;
		try {
			final IDocument d = event.getDocument();
			firstLineIndex = d.getLineOfOffset(event.getOffset());
			final int nbCharsInserted = event.getLength() == 0 ? event.getText().length() : event.getLength();
			final int lastLineIndex  = d.getLineOfOffset(Math.min(event.getOffset()+nbCharsInserted, d.getLength()));

			int length = 0;
			for (int l=firstLineIndex; l<=lastLineIndex; l++) {
				final String delimiter = d.getLineDelimiter(l);
				if (delimiter != null) length += delimiter.length();
				length += d.getLineInformation(l).getLength();
			}
			//System.out.println("documentChanged @["+firstLineIndex+(firstLineIndex!=lastLineIndex?("-"+lastLineIndex):"")+"]");
			text = event.getDocument().get(d.getLineOffset(firstLineIndex), length);
			//System.out.println("\""+text+"\"");
		}
		catch (final BadLocationException ex) {
			ex.printStackTrace();
			System.err.println("Error calculating edition span; defaulting to whole document.");
			firstLineIndex = 0;
			text = event.getDocument().get();
		}
		parser.parse(text);
		tokens = createTokens(firstLineIndex);

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

}
