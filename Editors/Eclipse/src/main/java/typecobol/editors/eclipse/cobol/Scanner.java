package typecobol.editors.eclipse.cobol;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.List;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;

import typecobol.client.Client;
import typecobol.client.CodeElement;
import typecobol.editors.eclipse.ColorMap;
import typecobol.editors.eclipse.WhitespaceDetector;

public class Scanner implements ITokenScanner {

	private final WhitespaceDetector detector = new WhitespaceDetector();
	private ColorMap colors;
	private String text;
	private int index;
	private int offsetEndOfLastLine;
	private int offsetInLine;
	private int color = 0;
	private List<CodeElement> elements;

	public Scanner(final ColorMap colors) {
		this.colors = colors;
	}

	@Override //ITokenScanner
	public int getTokenLength() {
		final CodeElement e = elements.get(index);
		System.out.println("getTokenLength("+index+"): "+e.end+"-"+e.begin+"+1="+(e.end-e.begin+1));
		return e.end-e.begin +1;
	}

	@Override //ITokenScanner
	public int getTokenOffset() {
		System.out.println("getTokenOffset="+offsetEndOfLastLine+"+"+offsetInLine+"="+(offsetEndOfLastLine + offsetInLine));
		return offsetEndOfLastLine + offsetInLine;
	}

	@Override //ITokenScanner
	public IToken nextToken() {
		index ++;
		if (index >= elements.size()) return Token.EOF;
		final CodeElement e = elements.get(index);
		if (index == 0) {
			offsetEndOfLastLine = 0;
		} else {
			if (e.begin <= offsetInLine) {
				offsetEndOfLastLine += elements.get(index-1).end+1;
			}
		}
		offsetInLine = e.begin;
		System.out.println("nextToken(): ["+index+"] "+e);
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
		//System.out.println("setRange(.., "+offset+", "+range+")");
		this.text = null;
		this.index = -1;

		try { this.text = document.get(offset, range); }
		catch (BadLocationException ex) { ex.printStackTrace(); return; }
		System.out.println(">>>>>>>>>>>>>>>>>>>>\n"+text+"\n<<<<<<<<<<<<<<<<<<<<");
		if (text != null) parse(text);
	}



	private boolean parse(final String text) {
		elements = null;
		final String pipename = "testpipe";

		RandomAccessFile pipe = null;
		try { // connect to pipe
			final String path = "\\\\.\\pipe\\"+pipename;
			pipe = new RandomAccessFile(path, "rw"); //FNFException
		} catch (final FileNotFoundException ex) {
			System.err.println("Pipe \""+pipename+"\" not found.");
			return false;
		}

		boolean status = true;
		try { elements = new Client(pipe).sendrcv(text); }
		catch (final Exception ex) {
			System.err.println("Error sending/receiving data.");
			ex.printStackTrace();
			status = false;
		}
		try { pipe.close(); }
		catch (final IOException ex) {
			System.err.println("Error closing pipe \""+pipename+"\".");
			ex.printStackTrace();
		}
		return status;
	}

}
