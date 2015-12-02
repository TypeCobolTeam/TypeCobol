package typecobol.editors.eclipse;

import java.util.List;
import java.util.ArrayList;
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
		DocumentEventAdapter e = null;
		Diff.Info info = null;
		try {
			e = new DocumentEventAdapter(event);
			info = new Diff.Info(e);
		} catch(final BadLocationException ex) { ex.printStackTrace(); }
		if (after) {
			this.after = info;
			cleanupLines(e);
		} else this.before = info;
	}

	private void cleanupLines(DocumentEventAdapter event) {
		final int nblines = event.indexOfLastLine - event.indexOfFirstLine +1;
		if (nblines > 1 || nblines < before.lines.size()) {
			final int lengthOfUpdate = event.lengthOfInsertedText - event.lengthOfRemovedText;
			if (lengthOfUpdate < 0) {
				this.after.removeLastLine();
			} else
			if (lengthOfUpdate > 0) {
				final Line bLast = this.before.removeLastLine();
				final Line aLast = this.after.lines.get(this.after.lines.size()-1);
				if (bLast.text.compareTo(aLast.text) == 0)
					this.after.removeLastLine();
			}
		}
	}

	public TextChange[] createTextChanges() {
		if (after.lengthOfDocument == 0) return createDocumentClearedTextChanges();
		final java.util.List<TextChange> changes = new java.util.ArrayList<TextChange>();
		for (final Line line: after) {
			final TextChange change = new TextChange();
			if (before.contains(line.index))
				 change.type = TextChangeType.LineUpdated;
			else change.type = TextChangeType.LineInserted;
			change.line = line.index;
			change.text = line.text;
			changes.add(change);
		}
		for (final Line line: before) {
			if (after.contains(line.index)) continue;// already added in previous loop
			final TextChange change = new TextChange();
			change.type = TextChangeType.LineRemoved;
			change.line = line.index;
			change.text = line.text;
			changes.add(change);
		}
		return changes.toArray(new TextChange[changes.size()]);
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
		final List<Line> lines = new ArrayList<Line>();

		public Info(final int lengthOfDocument) {
			this.lengthOfDocument = lengthOfDocument;
		}

		public Info(final DocumentEventAdapter event) throws BadLocationException {
			lengthOfDocument = event.lengthOfDocument;
			for (int l=event.indexOfFirstLine; l<=event.indexOfLastLine; l++) {
				final IRegion line = event.document.getLineInformation(l);
				lines.add(new Line(l, line.getOffset(), event.document.get(line.getOffset(),line.getLength())));
			}
			assert(lines.size() == (event.indexOfLastLine-event.indexOfFirstLine+1));
		}

		public boolean contains(int index) {
			for(final Line line: lines)
				if (line.index == index)
					return true;
			return false;
		}

		public boolean contains(final String text) {
			for(final Line line: lines)
				if (line.text == text)
					return true;
			return false;
		}

		public Line removeLastLine() {
			return lines.remove(lines.size()-1);
		}

		@Override
		public String toString() {
			final StringBuilder str = new StringBuilder();
			for(final Line line: this) str.append(line).append("\n");
			return str.toString();
		}

		@Override
		public Iterator<Line> iterator() {
			return lines.iterator();
		}
	}
	static class DocumentEventAdapter {
		final IDocument document;
		final int lengthOfDocument;
		final int lengthOfRemovedText;
		final int lengthOfInsertedText;
		final int indexOfFirstLine;
		final int indexOfLastLine;

		public DocumentEventAdapter(final DocumentEvent event) throws BadLocationException {
			final IDocument d = event.getDocument();
			lengthOfDocument = d.getLength();
			lengthOfRemovedText = event.getLength();
			lengthOfInsertedText = event.getText().length();
			indexOfFirstLine = d.getLineOfOffset(event.getOffset());
			final int max = Math.max(lengthOfInsertedText, lengthOfRemovedText);
			indexOfLastLine  = d.getLineOfOffset(Math.min(event.getOffset()+max/*-1*/, d.getLength()));
			document = d;
		}

		@Override
		public String toString() {
			final StringBuilder str = new StringBuilder();
			str.append("document.length=").append(lengthOfDocument);
			str.append(" removed:").append(lengthOfRemovedText);
			str.append(" inserted:").append(lengthOfInsertedText);
			str.append(" @").append(indexOfFirstLine);
			if (indexOfLastLine != indexOfFirstLine)
				str.append("-").append(indexOfLastLine);
			str.append("\ntext=\"").append(document.get()).append("\"");
			return str.toString();
		}
	}
}
