package typecobol.editors.eclipse.cobol;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;

import typecobol.client.CodeElement;
import typecobol.client.CodeElementsProvider;
import typecobol.editors.eclipse.ColorMap;

public class Scanner implements ITokenScanner {

	private final ColorMap colors;
	private int color = 0;
	private int index;
	private CodeElement current;
	private final CodeElementsProvider provider;
	private IDocument document;

	public Scanner(final CodeElementsProvider provider, final ColorMap colors) {
		this.provider = provider;
		this.colors = colors;
	}

	@Override //ITokenScanner
	public int getTokenLength() {
		return current.end-current.begin;
	}

	@Override //ITokenScanner
	public int getTokenOffset() {
		int offset = -1;
		try { offset = document.getLineOffset(current.lineFirst); }
		catch (final BadLocationException ex) { }
		//System.out.println("getTokenOffset(["+index+"] "+current+"): los("+current.lineFirst+"):"+offset+" +"+current.begin+" = "+(offset+current.begin));
		return offset + current.begin;
	}

	@Override //ITokenScanner
	public IToken nextToken() {
		final java.util.List<CodeElement> elements = provider.getCodeElements();
		if (elements == null) return Token.EOF;
		index ++;
		if (index >= elements.size()) return Token.EOF;
		current =  elements.get(index);
		return new Token(new TextAttribute(colors.getColor(generate()), null, SWT.ITALIC));
	}

	private RGB generate() {
		color = (color+1)%6;
		switch(color) {
			case 0: return hex2rgb("#aa0000");
			case 1: return hex2rgb("#00aa00");
			case 2: return hex2rgb("#0000aa");
			case 3: return hex2rgb("#00aaaa");
			case 4: return hex2rgb("#aa00aa");
			default: return hex2rgb("#aaaa00");
		}
	}

	public static RGB hex2rgb(String code) {
	    final java.awt.Color color = new java.awt.Color(
	            Integer.valueOf(code.substring(1,3), 16),
	            Integer.valueOf(code.substring(3,5), 16),
	            Integer.valueOf(code.substring(5,7), 16));
	    return new RGB(color.getRed(), color.getGreen(), color.getBlue());
	}

	@Override //ITokenScanner
	public void setRange(final IDocument document, final int offset, final int range) {
		this.document = document;
		this.index = -1;
		this.current = null;
	}

}
