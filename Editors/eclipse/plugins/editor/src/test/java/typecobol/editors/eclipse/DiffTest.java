package typecobol.editors.eclipse;

import static org.junit.Assert.*;
import org.eclipse.jface.text.*;
import org.junit.Test;
import typecobol.client.TextChange;
import typecobol.client.TextChangeType;

public class DiffTest {

	private static final String SEP = "\r\n";
	private static final String LEMPTY   = "";
	private static final String LINSPECT = "INSPECT a TALLYING i FOR CHARACTERS BY x j FOR CHARACTERS.";
	private static final String LADDX    =    "ADD 1 TO x.";
	private static final String LADDY    =    "ADD 2 TO 2 GIVING y.";
	private static final String LADDY_DELETE  = "2 GIVING ";
	private static final String LADDY_UPDATED = "ADD 2 TO y.";
	private static final String LADDZ    =    "ADD 3 TO z.";

	private static final byte INSERT = 0x01;
	private static final byte DELETE = 0x10;
	private static final byte UPDATE = INSERT|DELETE;

	@org.junit.Before
	public void setUp() throws Exception { }

	@org.junit.After
	public void tearDown() throws Exception { }

	@Test
	public void onDocumentCleared() {
		// given
		final String before = LINSPECT+SEP + LEMPTY+SEP + LADDX+SEP + LADDY;
		final String after  = LEMPTY;
		final String edit = before;
		final Diff diff = createDiff(before, DELETE, edit, after);
		// when
		final TextChange[] changes = diff.createTextChanges();
		// then
		assertEquals(changes.length, 1);
		assertEquals(changes[0].type, TextChangeType.DocumentCleared);
			// we don't care about text or line
	}

	@Test
	public void onLineDeleted() {
		// given
		final String before = LINSPECT+SEP + LEMPTY+SEP + LADDX+SEP + LADDY;
		final String after  = LINSPECT+SEP + LEMPTY+SEP + LADDY_UPDATED;
		final String edit = LADDX+SEP;
		final Diff diff = createDiff(before, DELETE, edit, after);
		// when
		final TextChange[] changes = diff.createTextChanges();
		// then
		assertEquals(changes.length, 1);
		assertEquals(changes[0].type, TextChangeType.LineRemoved);
		assertEquals(changes[0].line, 2);
			// we don't care about text
	}

	@Test
	public void onLineUpdated() {
		// given
		final String before = LINSPECT+SEP + LEMPTY+SEP + LADDX+SEP + LADDY;
		final String after  = LINSPECT+SEP + LEMPTY+SEP + LADDX+SEP + LADDY_UPDATED;
		final String edit = LADDY_DELETE;
		final Diff diff = createDiff(before, UPDATE, edit, after);
		// when
		final TextChange[] changes = diff.createTextChanges();
		// then
		assertEquals(changes.length, 1);
		assertEquals(changes[0].type, TextChangeType.LineUpdated);
		assertEquals(changes[0].text, LADDY_UPDATED);
		assertEquals(changes[0].line, 3);
	}

	@Test
	public void onLineInserted() {
		// given
		final String before = LINSPECT+SEP + LEMPTY+SEP + LADDX+SEP + LADDY;
		final String after  = LINSPECT+SEP + LEMPTY+SEP + LADDX+SEP + LADDZ+SEP + LADDY;
		final String edit = LADDZ+SEP; //TODO same but inverted
		final Diff diff = createDiff(before, INSERT, edit, after);
		// when
		final TextChange[] changes = diff.createTextChanges();
		// then
		assertEquals(changes.length, 1);
		assertEquals(changes[0].type, TextChangeType.LineInserted);
		assertEquals(changes[0].text, LADDZ);
		assertEquals(changes[0].line, 3);
	}

	private Diff createDiff(final String before, final byte mode, final String edit, final String after) {
		int offset;
		if ((mode & DELETE) > 0)
			 offset = before.indexOf(edit);
		else offset = after.indexOf(edit);
		final DocumentEvent ebefore = createEvent(mode, offset, edit, new Document(before));
		final DocumentEvent eafter  = createEvent(mode, offset, edit, new Document(after));
		final Diff diff = new Diff();
		diff.set(Diff.BEFORE, ebefore);
		diff.set(Diff.AFTER,  eafter);
		return diff;
	}

	private DocumentEvent createEvent(final byte mode, final int offset, final String text, final IDocument document) {
		final DocumentEvent event = new DocumentEvent();
		event.fLength = (mode & DELETE) > 0 ? text.length() : 0;
		event.fText   = (mode & INSERT) > 0 ? text : "";
		event.fOffset = offset;
		//if (event.fOffset < 0 && document.get().length() == 0) event.fOffset = 0;
		event.fDocument = document;
		return event;
	}

}
