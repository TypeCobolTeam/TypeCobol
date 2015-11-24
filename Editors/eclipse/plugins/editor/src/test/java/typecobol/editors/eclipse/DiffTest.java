package typecobol.editors.eclipse;

import static org.junit.Assert.*;
import org.eclipse.jface.text.*;
import org.junit.Test;
import typecobol.client.TextChange;
import typecobol.client.TextChangeType;

public class DiffTest {

	private static final String LEMPTY   = "\n";
	private static final String LINSPECT = "INSPECT a TALLYING i FOR CHARACTERS BY x j FOR CHARACTERS.\n";
	private static final String LADDX =    "ADD 1 TO x.\n";
	private static final String LADDY =    "ADD 2 TO 2 GIVING y.";
	private static final String LADDZ =    "ADD 3 TO z.\n";

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
		final String initial = LINSPECT + LEMPTY + LADDX + LADDY;
		final String edited  = "";
		final Diff diff = createDiff(initial, DELETE, initial, edited);
		// when
		final TextChange[] changes = diff.createTextChanges();
		// then
		assertEquals(changes.length, 1);
		assertEquals(changes[0].type, TextChangeType.DocumentCleared);
	}

	private Diff createDiff(final String before, final byte mode, final String edit, final String after) {
		final DocumentEvent ebefore = createEvent(INSERT, before, new Document(before));
		final DocumentEvent eafter  = createEvent(mode,   edit,   new Document(after));
		final Diff diff = new Diff();
		diff.set(Diff.BEFORE, ebefore);
		diff.set(Diff.AFTER,  eafter);
		return diff;
	}

	private DocumentEvent createEvent(final byte mode, final String text, final IDocument document) {
		final DocumentEvent event = new DocumentEvent();
		event.fLength = (mode & DELETE) > 0 ? text.length() : 0;
		event.fOffset = document.get().indexOf(text);
		if (event.fOffset < 0 && document.get().length() == 0) event.fOffset = 0;
		event.fText = text;
		event.fDocument = document;
		return event;
	}

}
