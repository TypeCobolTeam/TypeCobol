package typecobol.editors.eclipse;

import java.util.Iterator;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;

import typecobol.client.TextChange;
import typecobol.client.TextChangeType;

public class Diff {

	public static final boolean BEFORE = false;
	public static final boolean AFTER  = true;
	public Info before = null;
	public Info after  = null;

	public void set(final boolean after, final DocumentEvent event) {
		Diff.Info info = null;
		final int lengthOfRemovedText = event.getLength();
		final int lengthOfInsertedText = event.getText().length();
		if (( after && lengthOfInsertedText == 0)
		 || (!after && lengthOfRemovedText  == 0)) {
			info = new Diff.Info(event.getDocument().getLength());
		} else {
			try { info = new Diff.Info(event); }
			catch(final BadLocationException ex) { ex.printStackTrace(); }
		}
		if (after) this.after = info;
		else this.before = info;
	}

	public TextChange[] createTextChanges() {
		if (after.lengthOfDocument == 0) return createDocumentClearedTextChanges();
		final TextChange[] changes = new TextChange[Math.max(after.lines.length, before.lines.length)];
		int c;
		for (c = 0; c < after.lines.length; c++) {
			changes[c] = new TextChange();
			if (before == null || c >= before.lines.length)
				 changes[c].type = TextChangeType.LineInserted;
			else changes[c].type = TextChangeType.LineUpdated;
			changes[c].line = after.lines[c].index;
			changes[c].text = after.lines[c].text;
		}
		if (before == null) return changes;
		for (; c < changes.length && c < before.lines.length; c++) {
			changes[c] = new TextChange();
			changes[c].type = TextChangeType.LineRemoved;
			changes[c].line = (after.lengthOfDocument < c ? after.lines[c].index : before.lines[c].index);
			changes[c].text = null;// unused by parser
		}
		return changes;
	}

	private static TextChange[] createDocumentClearedTextChanges() {
		final TextChange[] changes = new TextChange[1];
		changes[0] = new TextChange();
		changes[0].type = TextChangeType.DocumentCleared;
		changes[0].line = 0;    // unused by parser
		changes[0].text = null;// unused by parser
		return changes;
	}

	@Override
	public String toString() {
		final StringBuilder str = new StringBuilder();
		str.append("Before:\n").append(before!=null?before:"?\n");
		str.append("After:\n").append(after!=null?after:"?\n");
		return str.toString();
	}



	static class Line {
		final int index;
		final int offset;
		final String text;

		public Line(final int index, final int offset, final String text) {
			this.index = index;
			this.offset = offset;
			this.text = text;
		}

		@Override
		public String toString() {
			final StringBuilder str = new StringBuilder();
			str.append(index).append(": \"").append(text).append("\" (").append(text.length()).append(")");
			return str.toString();
		}
	}
	static class Info implements Iterable<Line> {
		final int lengthOfDocument;
		final Line[] lines;

		public Info(final int lengthOfDocument) {
			this.lengthOfDocument = lengthOfDocument;
			lines = new Line[0];
		}

		public Info(final DocumentEvent event) throws BadLocationException {
			final IDocument d = event.getDocument();
			lengthOfDocument = d.getLength();
			final int lengthOfRemovedText = event.getLength();
			final int lengthOfInsertedText = event.getText().length();
			final int indexOfFirstLine = d.getLineOfOffset(event.getOffset());
			final int lengthOfUpdate = lengthOfRemovedText == 0 ? lengthOfInsertedText : lengthOfRemovedText;
			      int indexOfLastLine  = d.getLineOfOffset(Math.min(event.getOffset()+lengthOfUpdate-1, d.getLength()));
			lines = new Line[indexOfLastLine-indexOfFirstLine+1];
			for (int l=indexOfFirstLine; l<=indexOfLastLine; l++) {
				final IRegion line = d.getLineInformation(l);
				lines[l-indexOfFirstLine] = new Line(l, line.getOffset(), d.get(line.getOffset(),line.getLength()));
			}
		}

		@Override
		public String toString() {
			final StringBuilder str = new StringBuilder();
			for(final Line line: this) str.append(line).append("\n");
			return str.toString();
		}

		@Override
		public Iterator<Line> iterator() {
			Iterator<Line> iterator = new Iterator<Line>() {
				private int current = 0;
				@Override
				public boolean hasNext() {
					return current < lines.length;
				}
				@Override
				public Line next() {
					return lines[current++];
				}
				@Override
				public void remove() {
					throw new UnsupportedOperationException();
				}
			};
			return iterator;
		}
	}
}
