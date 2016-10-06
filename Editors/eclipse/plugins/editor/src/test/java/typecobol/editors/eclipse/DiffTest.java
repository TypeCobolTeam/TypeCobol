package typecobol.editors.eclipse;

import static org.junit.Assert.*;
import org.eclipse.jface.text.*;
import org.junit.Test;
import typecobol.client.TextChange;
import typecobol.client.TextChangeType;
import typecobol.editors.eclipse.Diff.DocumentEventAdapter;

public class DiffTest {

	@org.junit.Before
	public void setUp() throws Exception { }

	@org.junit.After
	public void tearDown() throws Exception { }

	private DocumentEvent createEvent(final String content, final int offset, final int lengthOfReplacedText, final String insertedText) {
		return new DocumentEvent(new Document(content), offset, lengthOfReplacedText, insertedText);
	}
	private DocumentEvent[] createEvents(final String content, final int offset, final int lengthOfRemovedText, final String insertedText) {
		final DocumentEvent[] events = new DocumentEvent[2];
		events[0] = createEvent(content, offset, lengthOfRemovedText, insertedText);
		final String editedContent = content.substring(0, offset)+insertedText+content.substring(offset+lengthOfRemovedText);
		events[1] = createEvent(editedContent, offset, lengthOfRemovedText, insertedText);
		return events;
	}

	private Diff createDiff(final String beforeEdit, final int offset, final int deleted, final String edit, final String afterEdit) {
		assertEquals(afterEdit.length(), beforeEdit.length()-deleted+edit.length());
		final DocumentEvent[] events = createEvents(beforeEdit, offset, deleted, edit);
		// DocumentEvent BEFORE edit is done
		assertEquals(events[0].getDocument().getLength(), beforeEdit.length());
		assertEquals(events[0].getDocument().get(),       beforeEdit);
		assertEquals(events[0].getOffset(),               offset);
		assertEquals(events[0].getLength(),               deleted);
		assertEquals(events[0].getText(),                 edit);
		// DocumentEvent AFTER resource is edited
		assertEquals(events[1].getDocument().getLength(), afterEdit.length());
		assertEquals(events[1].getDocument().get(),       afterEdit);
		assertEquals(events[1].getOffset(),               events[0].getOffset());
		assertEquals(events[1].getLength(),               events[0].getLength());
		assertEquals(events[1].getText(),                 events[0].getText());

		final Diff diff = new Diff();
		diff.set(false, events[0]); // sets diff.before
		diff.set(true,  events[1]); // sets diff.after
		return diff;
	}

	private void checkLines(final java.util.List<Diff.Line> lines, final String content, final int[] expected) {
		assertEquals(lines.size(), expected.length);
		final java.util.regex.Matcher m = java.util.regex.Pattern.compile("\r\n|\r|\n").matcher(content);
		int nblines = 1;
		int start = 0;
		while (m.find()) {
			final int index = nblines-1;
			if (java.util.Arrays.asList(expected).contains(index)) {
				final Diff.Line line = lines.get(index);
				final String text = content.substring(start, m.start());
				assertEquals(line.index, index);
				assertEquals(line.text,  text);
				assertEquals(line.offset,start);
			}
			nblines++;;
			start = m.end();
		}
	}



	@Test
	public void onDocumentOpened() {
		final String content =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PROGRAM-ID. Test."
			+"\r\n"+"END PROGRAM Test."
			;
		final DocumentEvent after = createEvent(content, 0, content.length(), content);
		// (nothing BEFORE edit)
		// DocumentEvent AFTER resource is edited
		assertEquals(after.getDocument().getLength(), content.length());
		assertEquals(after.getDocument().get(),       content);
		assertEquals(after.getOffset(),               0);
		assertEquals(after.getLength(),               content.length());
		assertEquals(after.getText(),                 content);

		final Diff diff = new Diff();
		diff.before = new Diff.Info(0);
		diff.set(true, after);
		checkLines(diff.before.lines, "",      new int[]{ });
		checkLines(diff.after.lines,  content, new int[]{ 0,1,2 });

		final TextChange[] changes = diff.createTextChanges();
		assertEquals(changes.length,  3);
		assertEquals(changes[0].line, 0);
		assertEquals(changes[0].type, TextChangeType.LineInserted);
		assertEquals(changes[0].text, "IDENTIFICATION DIVISION.");
		assertEquals(changes[1].line, 1);
		assertEquals(changes[1].type, TextChangeType.LineInserted);
		assertEquals(changes[1].text, "  PROGRAM-ID. Test.");
		assertEquals(changes[2].line, 2);
		assertEquals(changes[2].type, TextChangeType.LineInserted);
		assertEquals(changes[2].text, "END PROGRAM Test.");
	}

	@Test
	public void onDocumentCleared() {
		final String content =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PROGRAM-ID. Test."
			+"\r\n"+"END PROGRAM Test."
			;
		final Diff diff = createDiff(content, 0, content.length(), "", "");
		checkLines(diff.before.lines, content, new int[]{ 0,1,2 });
		checkLines(diff.after.lines,  "",      new int[]{ 0 });
		final TextChange[] changes = diff.createTextChanges();
		assertEquals(changes.length,  1);
		assertEquals(changes[0].line, 0);
		assertEquals(changes[0].type, TextChangeType.DocumentCleared);
		assertEquals(changes[0].text, null);
	}

	@Test
	public void onLineUpdatedAdd() {
		final String content =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PROGRAM-ID. Test."
			+"\r\n"+"END PROGRAM Test."
			;
		Diff diff;
		TextChange[] changes;
		// FIRST EDIT
		final int offset1 = 44;
		final String edit1 = "ud";
		final int del1 = 0;
		final String afterEdit1 =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PROGRAM-ID. Testud."
			+"\r\n"+"END PROGRAM Test."
			;
		diff = createDiff(content, offset1, del1, edit1, afterEdit1);
		checkLines(diff.before.lines, content,    new int[]{ 1 });
		checkLines(diff.after.lines,  afterEdit1, new int[]{ 1 });
		changes = diff.createTextChanges();
		assertEquals(changes.length,  1);
		assertEquals(changes[0].line, 1);
		assertEquals(changes[0].type, TextChangeType.LineUpdated);
		assertEquals(changes[0].text, "  PROGRAM-ID. Testud.");

		// SECOND EDIT
		final int offset2 = 60;
		final String edit2 = " Tu peux pas";
		final int del2 = 0;
		final String afterEdit2 =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PROGRAM-ID. Testud."
			+"\r\n"+"END PROGRAM Tu peux pas Test."
			;
		diff = createDiff(afterEdit1, offset2, del2, edit2, afterEdit2);
		checkLines(diff.before.lines, content,    new int[]{ 1 });
		checkLines(diff.after.lines,  afterEdit1, new int[]{ 1 });
		changes = diff.createTextChanges();
		assertEquals(changes.length,  1);
		assertEquals(changes[0].line, 2);
		assertEquals(changes[0].type, TextChangeType.LineUpdated);
		assertEquals(changes[0].text, "END PROGRAM Tu peux pas Test.");
	}

	@Test
	public void onLineUpdatedDel() {
		final String content =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PROGRAM-ID. Test."
			+"\r\n"+"END PROGRAM Test."
			;
		Diff diff;
		TextChange[] changes;
		// FIRST EDIT
		final int offset1 = 31;
		final String edit1 = "";
		final int del1 = 14;
		final String afterEdit1 =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PRO"
			+"\r\n"+"END PROGRAM Test."
			;
		diff = createDiff(content, offset1, del1, edit1, afterEdit1);
		checkLines(diff.before.lines, content,    new int[]{ 1 });
		checkLines(diff.after.lines,  afterEdit1, new int[]{ 1 });
		changes = diff.createTextChanges();
		assertEquals(changes.length,  1);
		assertEquals(changes[0].line, 1);
		assertEquals(changes[0].type, TextChangeType.LineUpdated);
		assertEquals(changes[0].text, "  PRO");

		// SECOND EDIT
		final int offset2 = 36;
		final String edit2 = "";
		final int del2 = 14;
		final String afterEdit2 =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PRO"
			+"\r\n"+"END"
			;
		diff = createDiff(afterEdit1, offset2, del2, edit2, afterEdit2);
		checkLines(diff.before.lines, afterEdit1, new int[]{ 2 });
		checkLines(diff.after.lines,  afterEdit2, new int[]{ 2 });
		changes = diff.createTextChanges();
		//for (final TextChange c: changes) System.out.println(c);
		assertEquals(changes.length,  1);
		assertEquals(changes[0].line, 2);
		assertEquals(changes[0].type, TextChangeType.LineUpdated);
		assertEquals(changes[0].text, "END");
	}

	@Test
	public void onLineInserted() {
		final String content =
					"IDENTIFICATION DIVISION."
			;
		Diff diff;
		TextChange[] changes;
		// FIRST EDIT
		final int offset1 = content.length();
		final String edit1 = "\r\n"+"END PROGRAM Test.";
		final int del1 = 0;
		final String afterEdit1 =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"END PROGRAM Test."
			;
		diff = createDiff(content, offset1, del1, edit1, afterEdit1);
		checkLines(diff.before.lines, content,    new int[]{ 0 });
		checkLines(diff.after.lines,  afterEdit1, new int[]{ 0,1 });
		changes = diff.createTextChanges();
		assertEquals(changes.length,  2);
		assertEquals(changes[0].line, 0);
		assertEquals(changes[0].type, TextChangeType.LineUpdated);
		assertEquals(changes[0].text, "IDENTIFICATION DIVISION.");
		assertEquals(changes[1].line, 1);
		assertEquals(changes[1].type, TextChangeType.LineInserted);
		assertEquals(changes[1].text, "END PROGRAM Test.");

		// SECOND EDIT
		final int offset2 = offset1;
		final String edit2 = "\r\n"+"  PROGRAM-ID. Test.";
		final int del2 = 0;
		final String afterEdit2 =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PROGRAM-ID. Test."
			+"\r\n"+"END PROGRAM Test."
			;
		diff = createDiff(afterEdit1, offset2, del2, edit2, afterEdit2);
		checkLines(diff.before.lines, content,    new int[]{ 0 });
		checkLines(diff.after.lines,  afterEdit2, new int[]{ 0,1 });
		changes = diff.createTextChanges();
		assertEquals(changes.length,  2);
		assertEquals(changes[0].line, 0);
		assertEquals(changes[0].type, TextChangeType.LineUpdated);
		assertEquals(changes[0].text, "IDENTIFICATION DIVISION.");
		assertEquals(changes[1].line, 1);
		assertEquals(changes[1].type, TextChangeType.LineInserted);
		assertEquals(changes[1].text, "  PROGRAM-ID. Test.");
	}

	@Test
	public void onLinesMerged() {
		final String content =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PROGRAM-ID. Test."
			+"\r\n"+"END PROGRAM Test."
			;
		Diff diff;
		TextChange[] changes;
		// EDIT
		final int offset1 = 15;
		final String edit1 = "";
		final int del1 = 44;
		final String afterEdit1 =
					"IDENTIFICATION Test."
			;
		diff = createDiff(content, offset1, del1, edit1, afterEdit1);
		checkLines(diff.before.lines, content,    new int[]{ 0,1,2 });
		checkLines(diff.after.lines,  afterEdit1, new int[]{ 0 });
		changes = diff.createTextChanges();
		assertEquals(changes.length,  3);
		assertEquals(changes[0].line, 0);
		assertEquals(changes[0].type, TextChangeType.LineUpdated);
		assertEquals(changes[0].text, "IDENTIFICATION Test.");
		assertEquals(changes[1].line, 1);
		assertEquals(changes[1].type, TextChangeType.LineRemoved);
		assertEquals(changes[1].text, "  PROGRAM-ID. Test.");
		assertEquals(changes[2].line, 2);
		assertEquals(changes[2].type, TextChangeType.LineRemoved);
		assertEquals(changes[2].text, "END PROGRAM Test.");
	}

	@Test
	public void onLinesChanged() {
		final String content =
					"IDENTIFICATION DIVISION."
			+"\r\n"+"  PROGRAM-ID. Test."
			+"\r\n"+"END PROGRAM Test."
			;
		Diff diff;
		TextChange[] changes;
		// EDIT
		final int offset1 = 6;
		final String edit1 = "TE wiztigers.";
		final int del1 = 58;
		final String afterEdit1 =
					"IDENTITE wiztigers."
			;
		diff = createDiff(content, offset1, del1, edit1, afterEdit1);
		checkLines(diff.before.lines, content,    new int[]{ 0,1,2 });
		checkLines(diff.after.lines,  afterEdit1, new int[]{ 0 });
		changes = diff.createTextChanges();
		assertEquals(changes.length,  3);
		assertEquals(changes[0].line, 0);
		assertEquals(changes[0].type, TextChangeType.LineUpdated);
		assertEquals(changes[0].text, "IDENTITE wiztigers.");
		assertEquals(changes[1].line, 1);
		assertEquals(changes[1].type, TextChangeType.LineRemoved);
		assertEquals(changes[1].text, "  PROGRAM-ID. Test.");
		assertEquals(changes[2].line, 2);
		assertEquals(changes[2].type, TextChangeType.LineRemoved);
		assertEquals(changes[2].text, "END PROGRAM Test.");
	}
}
