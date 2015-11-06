package typecobol.editors.eclipse.cobol;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;

import typecobol.client.TokenFamily;
import typecobol.client.TokensProvider;
import typecobol.editors.eclipse.ColorMap;

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
		current =  tokens.get(index);
		//if (current.family == TokenFamily.StatementStartingKeyword && current.begin==4 && current.length==6) System.out.println("Statement starting @"+getTokenOffset()+" l="+getTokenLength());
		return new Token(new TextAttribute(getColor(current.family), null, SWT.BOLD));
	}

	private Color getColor(final TokenFamily family) {
		return colors.getColor(getColorCode(family));
	}

	private static String getColorCode(final TokenFamily family) {
		switch(family) {
			case Invalid:
				return "#ff0000";
			case Comments:
			case CompilerDirective:
				return "#00aa00";
			case AlphanumericLiteral:
				return "#aa00aa";
			case NumericLiteral:
				return "#00aaaa";
			case CompilerDirectiveStartingKeyword:
			case CodeElementStartingKeyword:
			case StatementStartingKeyword:
			case StatementEndingKeyword:
				return "#0000ff";
			case SpecialRegisterKeyword:
			case FigurativeConstantKeyword:
			case SpecialObjetIdentifierKeyword:
			case SyntaxKeyword:
				return "#0000aa";
			case Symbol:
				return "#ffaa00";
			case SyntaxSeparator:
			case ArithmeticOperator:
			case RelationalOperator:
			case Whitespace:
			case InternalTokenGroup:
			case SyntaxLiteral:
			default:
				return "#000000";
		}
	}

	@Override //ITokenScanner
	public void setRange(final IDocument document, final int offset, final int range) {
		this.document = document;
		this.index = -1;
		this.current = null;
	}

}
