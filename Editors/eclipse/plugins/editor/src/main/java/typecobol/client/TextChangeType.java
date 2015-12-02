package typecobol.client;

public enum TextChangeType {

    LineInserted(0),
    LineUpdated(1),
    LineRemoved(2),
    DocumentCleared(3);



	public final int code;
	private TextChangeType(final int code) { this.code = code; }
	@Override
	public String toString() { return this.name(); }

	public static TextChangeType asEnum(final int code) {
		for(final TextChangeType type: TextChangeType.values()) {
			if (type.code == code) return type;
		}
		throw new IllegalArgumentException("Invalid code for enum: "+code);
	}
}
