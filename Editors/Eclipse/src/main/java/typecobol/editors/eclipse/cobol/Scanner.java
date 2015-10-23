package typecobol.editors.eclipse.cobol;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;

import typecobol.editors.eclipse.ColorMap;
import typecobol.editors.eclipse.WhitespaceDetector;

public class Scanner implements ITokenScanner {

	private final WhitespaceDetector detector = new WhitespaceDetector();
	private ColorMap colors;
	private String text;
	private int baseOffset;
	private int range;
	private int offset;
	private int length;
	private int color = 0;

	public Scanner(final ColorMap colors) {
		this.colors = colors;
	}

	@Override //ITokenScanner
	public int getTokenLength() {
		return length;
	}

	@Override //ITokenScanner
	public int getTokenOffset() {
		return baseOffset + offset-length;
	}

	@Override //ITokenScanner
	public IToken nextToken() {
		if (offset >= range) return Token.EOF;
		length = 0;
		char c = text.charAt(offset);
		boolean spaaace = detector.isWhitespace(c);
		while (detector.isWhitespace(c) == spaaace) {
			length++;
			if (offset+length >= range) break;
			c = text.charAt(offset+length);
		}
		//System.out.println("("+(baseOffset+offset)+","+(baseOffset+offset+length)+"): \""+text.substring(baseOffset+offset, baseOffset+offset+length)+"\"");
		offset += length;
		//System.out.println("nextToken(): "+(offset-length)+">"+offset+"("+length+") \""+text.substring(offset-length, offset)+"\"");
		return new Token(new TextAttribute(colors.getColor(spaaace? hex2rgb("#ffffff") : generate()), null, SWT.ITALIC));
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
		//System.out.println("setRange(.., "+offset+", "+range+")");
		this.baseOffset = offset;
		this.offset = 0;
		this.range = range;
		this.text = null;
		try { this.text = document.get(offset, range); }
		catch (BadLocationException ex) { ex.printStackTrace(); return; }
	}

}
