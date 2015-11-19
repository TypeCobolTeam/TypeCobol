package typecobol.editors.eclipse;

import typecobol.client.CodeElement;
import typecobol.client.TokensProvider;
import typecobol.client.Parser;

import java.util.List;

import org.eclipse.jface.text.*;

public class DocumentListener implements IDocumentListener, IDocumentPartitioningListener, TokensProvider {

	private final Parser parser = new Parser();
	private final MarkerCreator handler;

	private List<typecobol.client.Token> tokens = null;

	public DocumentListener(final MarkerCreator handler) {
		this.handler = handler;
	}

	@Override
	public void documentAboutToBeChanged(final DocumentEvent event) { }

	@Override
	public void documentChanged(final DocumentEvent event) {
		String text;
		try {
			final IDocument d = event.getDocument();
			final int firstLineIndex = d.getLineOfOffset(event.getOffset());
			final int lastLineIndex  = d.getLineOfOffset(event.getOffset()+event.getLength());
			System.out.print("documentChanged: ["+firstLineIndex+(firstLineIndex!=lastLineIndex?("-"+lastLineIndex):"")+"]; ");
			int length = 0;
			for (int l=firstLineIndex; l<=lastLineIndex; l++) {
				final String delimiter = d.getLineDelimiter(l);
				if (delimiter != null) length += delimiter.length();
				length += d.getLineInformation(l).getLength();
			}
			System.out.println("get("+d.getLineOffset(firstLineIndex)+","+length+")");
			text = event.getDocument().get(d.getLineOffset(firstLineIndex), length);
			System.out.println("\""+text+"\"");
		}
		catch (final BadLocationException ex) {
			ex.printStackTrace();
			System.err.println("Error calculating edition span; defaulting to whole document.");
			text = event.getDocument().get();
		}
		parser.parse(text);
		tokens = createTokens();

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

}
