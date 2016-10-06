package typecobol.editors.eclipse;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;

import typecobol.client.TokenFamily;
import typecobol.client.TokensProvider;

public class Scanner implements ITokenScanner {

	private final ColorMap colors;
	private int index;
	private typecobol.client.Token current;
	private final TokensProvider provider;
	private IDocument document;

	public Scanner(final TokensProvider provider, final ColorMap colors) {
		this.provider = provider;
		this.colors = colors;
	}

	@Override //ITokenScanner
	public int getTokenLength() {
		return current.length;
	}

	@Override //ITokenScanner
	public int getTokenOffset() {
		int offset = -1;
		try { offset = document.getLineOffset(current.line); }
		catch (final BadLocationException ex) { ex.printStackTrace(); }
		return offset + current.begin;
	}

	@Override //ITokenScanner
	public IToken nextToken() {
		final java.util.List<typecobol.client.Token> tokens = provider.getTokens();
		if (tokens == null) return Token.EOF;
		index ++;
		if (index >= tokens.size()) return Token.EOF;
		final typecobol.client.Token t = tokens.get(index);
		if (t.line >= document.getNumberOfLines()) {
			// prevents BadLocationException in getTokenOffset, but shouldn't be necessary
			// TypeCobol.Server seems to send too much CodeElements
			System.err.println("Token "+t+": line index is "+t.line+" but document has only "+document.getNumberOfLines()+" lines.");
			return Token.EOF;
		}
		current =  t;
		return new Token(new TextAttribute(getColor(current.family), null, SWT.BOLD));
	}

	private Color getColor(final TokenFamily family) {
		return colors.getColor(getColorCode(family));
	}

	private static boolean hackIssue128 = true;
	private static String getColorCode(final TokenFamily family) {
		hackIssue128 = !hackIssue128;
		switch(family) {
			case Invalid:
				return hackIssue128?"#fe0000":"#ff0000";
			case Comments:
			case CompilerDirective:
				return hackIssue128?"#00a900":"#00aa00";
			case AlphanumericLiteral:
				return hackIssue128?"#aa00a9":"#aa00aa";
			case NumericLiteral:
				return hackIssue128?"#00aaa9":"#00aaaa";
			case CompilerDirectiveStartingKeyword:
			case CodeElementStartingKeyword:
			case StatementStartingKeyword:
			case StatementEndingKeyword:
				return hackIssue128?"#0000fe":"#0000ff";
			case SpecialRegisterKeyword:
			case FigurativeConstantKeyword:
			case SpecialObjetIdentifierKeyword:
			case SyntaxKeyword:
				return hackIssue128?"#0000ab":"#0000aa";
			case Symbol:
				return hackIssue128?"#feaa00":"#ffaa00";
			case SyntaxSeparator:
			case ArithmeticOperator:
			case RelationalOperator:
			case Whitespace:
			case InternalTokenGroup:
			case SyntaxLiteral:
			default:
				return hackIssue128?"#000001":"#000000";
		}
	}

	@Override //ITokenScanner
	public void setRange(final IDocument document, final int offset, final int range) {
		this.document = document;
		this.index = -1;
		this.current = null;
	}

}
