namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;
	using TypeCobol.Compiler.CodeElements.Expressions;

/// <summary>
/// p346:
/// The INSPECT statement examines characters or groups of characters in a data item.
///
/// The INSPECT statement does the following tasks:
/// * Counts the occurrences of a specific character (alphanumeric, DBCS, or national)
/// in a data item (formats 1 and 3).
/// * Counts the occurrences of specific characters and fills all or portions of a data
/// item with specified characters, such as spaces or zeros (formats 2 and 3).
/// * Converts all occurrences of specific characters in a data item to user-supplied
/// replacement characters (format 4).
/// </summary>
public abstract class InspectStatement: StatementElement, VariableWriter {
    protected InspectStatement(StatementType statementType): base(CodeElementType.InspectStatement, statementType) { }

	/// <summary>
	/// p349:
	/// identifier-1
	/// Is the inspected item and can be any of the following items:
	/// * An alphanumeric group item or a national group item
	/// * An elementary data item described explicitly or implicitly with usage
	/// DISPLAY, DISPLAY-1, or NATIONAL. The item can have any category
	/// that is valid for the selected usage.
	/// </summary>
	public ReceivingStorageArea InspectedItem { get; set; }

	public IDictionary<QualifiedName,object> Variables { get { return VariablesWritten; } }
	protected IDictionary<QualifiedName,object> variablesWritten;
	public IDictionary<QualifiedName,object> VariablesWritten {
		get {
			if (variablesWritten != null) return variablesWritten;
			variablesWritten = new Dictionary<QualifiedName, object>();
			if (InspectedItem == null) return variablesWritten;
			string receiver = InspectedItem.StorageArea.ToString();
			if (InspectedItem.SendingStorageAreas == null || InspectedItem.SendingStorageAreas.Length < 1) {
				variablesWritten.Add(new URI(receiver), "?MAGIC_FOR_INSPECTED_ITEM_AS_ALPHANUM?");
				return variablesWritten;
			}
			foreach(var variable in InspectedItem.SendingStorageAreas) {
				string sender = variable.SymbolReference.Name;
				variablesWritten.Add(new URI(receiver), sender);
			}
			return variablesWritten;
		}
	}
	public bool IsUnsafe { get { return false; } }



	/// <summary>
	/// p351:
	/// BEFORE and AFTER phrases (all formats)
	/// This phrase narrows the set of items being tallied or replaced.
	///
	/// No more than one BEFORE phrase and one AFTER phrase can be specified for any
	/// one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase.
	/// </summary>
	public class CountingOrReplacingCondition {
		/// <summary>
		/// The BEFORE and AFTER phrases change how counting and replacing are done:
		/// p352:
		/// * When BEFORE is specified, counting or replacing of the inspected item
		/// (identifier-1) begins at the leftmost character position and continues until the first
		/// occurrence of the delimiter is encountered. If no delimiter is present in the
		/// inspected item, counting or replacing continues toward the rightmost character
		/// position.
		///  * When AFTER is specified, counting or replacing of the inspected item
		/// (identifier-1) begins with the first character position to the right of the delimiter
		/// and continues toward the rightmost character position in the inspected item. If
		/// no delimiter is present in the inspected item, no counting or replacement takes
		/// place.
		/// </summary>
		public SyntaxProperty<StartCharacterPosition> StartCharacterPosition { get; set; }

		/// <summary>
		/// p351:
		/// INITIAL
		/// The first occurrence of a specified item.
		/// </summary>
		public SyntaxProperty<bool> InitialOccurence { get; set; }

		/// <summary>
		/// identifier-4 or literal-2
		/// Is the delimiter.
		/// Delimiters are not counted or replaced.
		/// </summary>
		public AlphanumericVariable Delimiter { get; set; }
	}

	/// <summary>
	/// The BEFORE and AFTER phrases change how counting and replacing are done
	/// </summary>
	public enum StartCharacterPosition {
		/// <summary>
		/// p352:
		/// * When BEFORE is specified, counting or replacing of the inspected item
		/// (identifier-1) begins at the leftmost character position and continues until the first
		/// occurrence of the delimiter is encountered. If no delimiter is present in the
		/// inspected item, counting or replacing continues toward the rightmost character
		/// position.
		/// </summary>
		Before,
		/// <summary>
		/// * When AFTER is specified, counting or replacing of the inspected item
		/// (identifier-1) begins with the first character position to the right of the delimiter
		/// and continues toward the rightmost character position in the inspected item. If
		/// no delimiter is present in the inspected item, no counting or replacement takes
		/// place.
		/// </summary>
		After
	}
}

/// <summary>
/// p346:
/// The INSPECT statement examines characters or groups of characters in a data item.
///
/// The INSPECT statement does the following tasks:
/// * Counts the occurrences of a specific character (alphanumeric, DBCS, or national)
/// in a data item (formats 1 and 3).
/// </summary>
public class InspectTallyingStatement: InspectStatement {
	public InspectTallyingStatement(): base(StatementType.InspectTallyingStatement) { }
	protected InspectTallyingStatement(StatementType statementType) : base(statementType) { }

	/// <summary>
	/// p349:
	/// TALLYING phrase (formats 1 and 3)
	/// This phrase counts the occurrences of a specific character or special character in a
	/// data item.
	///
	/// When identifier-1 is a DBCS data item, DBCS characters are counted; when
	/// identifier-1 is a data item of usage national, national characters (encoding units) are
	/// counted; otherwise, alphanumeric characters (bytes) are counted.
	/// </summary>
	public InspectTallyingInstruction[] TallyingInstructions { get; set; }



	/// <summary>
	/// p349:
	/// TALLYING phrase (formats 1 and 3)
	/// This phrase counts the occurrences of a specific character or special character in a
	/// data item.
	/// </summary>
	public class InspectTallyingInstruction {
		/// <summary>
		/// p350:
		/// identifier-2
		/// Is the count field, and must be an elementary integer item defined without
		/// the symbol P in its PICTURE character-string.
		/// identifier-2 cannot be of category external floating-point.
		/// You must initialize identifier-2 before execution of the INSPECT statement
		/// begins.
		///
		/// Usage note: The count field can be an integer data item defined with usage
		/// NATIONAL.
		/// </summary>
		public ReceivingStorageArea CountField { get; set; }

		public CountAllCharactersOperation[] CountAllCharactersOperations { get; set; }

		public CountCharacterStringsOperation[] CountCharacterStringsOperations { get; set; }


		public class CountAllCharactersOperation {
			public CountingOrReplacingCondition[] CountingConditions { get; set; }
		}

		public class CountCharacterStringsOperation {
			public SyntaxProperty<CharacterStringsSelection> CharacterStringsSelection { get; set; }
			public CountCharacterStringPattern[] CharacterStringPatterns { get; set; }
		}

		public class CountCharacterStringPattern {
			public AlphanumericVariable SearchedCharacterString { get; set; }
			public CountingOrReplacingCondition[] CountingConditions { get; set; }
		}

	}



	public enum CharacterStringsSelection {
		/// <summary>
		/// p351:
		/// When ALL is specified and neither the BEFORE nor AFTER phrase is
		/// specified, the substitution field replaces each nonoverlapping occurrence of
		/// the subject field in the inspected item (identifier-1), beginning at the
		/// leftmost character position and continuing to the rightmost.
		/// </summary>
		All,
		/// <summary>
		/// p351:
		/// When LEADING is specified and neither the BEFORE nor AFTER phrase is
		/// specified, the substitution field replaces each contiguous nonoverlapping
		/// occurrence of the subject field in the inspected item (identifier-1), provided
		/// that the leftmost such occurrence is at the point where comparison began
		/// in the first comparison cycle for which this substitution field is eligible to
		/// participate.
		/// </summary>
		Leading,
		/// <summary>
		/// p351:
		/// When FIRST is specified and neither the BEFORE nor AFTER phrase is
		/// specified, the substitution field replaces the leftmost occurrence of the
		/// subject field in the inspected item (identifier-1).
		/// </summary>
		First
	}

}

/// <summary>
/// p346:
/// The INSPECT statement examines characters or groups of characters in a data item.
///
/// The INSPECT statement does the following tasks:
/// * Counts the occurrences of specific characters and fills all or portions of a data
/// item with specified characters, such as spaces or zeros (formats 2 and 3).
/// </summary>
public class InspectReplacingStatement: InspectTallyingStatement {
	public InspectReplacingStatement(): base(StatementType.InspectReplacingStatement) { }

	// p349:
	// REPLACING phrase (formats 2 and 3)
	// This phrase fills all or portions of a data item with specified characters, such as
	// spaces or zeros.

	public ReplaceAllCharactersOperation[] ReplaceAllCharactersOperations { get; set; }
	public ReplaceCharacterStringsOperation[] ReplaceCharacterStringsOperations { get; set; }



	public class ReplaceAllCharactersOperation {
		public AlphanumericVariable ReplacingCharacterString { get; set; }
		public CountingOrReplacingCondition[] ReplacingConditions { get; set; }
	}

	public class ReplaceCharacterStringsOperation {
		public SyntaxProperty<CharacterStringsSelection> CharacterStringsSelection { get; set; }
		public ReplaceCharacterStringPattern[] CharacterStringPatterns { get; set; }
	}

	public class ReplaceCharacterStringPattern {
		public AlphanumericVariable SearchedCharacterString { get; set; }
		public AlphanumericVariable ReplacingCharacterString { get; set; }
		public CountingOrReplacingCondition[] ReplacingConditions { get; set; }
	}
}

/// <summary>
/// p352:
/// CONVERTING phrase (format 4)
/// This phrase converts all occurrences of a specific character or string of characters in
/// a data item (identifier-1) to user-supplied replacement characters.
///
/// A format-4 INSPECT statement is interpreted and executed as if a format-2
/// INSPECT statement had been written with a series of ALL phrases (one for each
/// character of literal-4), specifying the same identifier-1. The effect is as if each single
/// character of literal-4 were referenced as literal-1, and the corresponding single
/// character of literal-5 referenced as literal-3. Correspondence between the characters
/// of literal-4 and the characters of literal-5 is by ordinal position within the data item.
///
/// If identifier-4, identifier-6, or identifier-7 occupies the same storage area as identifier-1,
/// the result of the execution of this statement is undefined, even if they are defined
/// by the same data description entry.
/// </summary>
public class InspectConvertingStatement: InspectStatement {
	public InspectConvertingStatement(): base(StatementType.InspectConvertingStatement) { }

	/// <summary>
	/// p352:
	/// identifier-7 or literal-5
	/// Specifies the replacing character string.
	/// The replacing character string (identifier-7 or literal-5) must be the same size
	/// as the replaced character string (identifier-6 or literal-4).
	/// </summary>
	public AlphanumericVariable SearchedCharacterString { get; set; }

	/// <summary>
	/// p352:
	/// identifier-6 or literal-4
	/// Specifies the character string to be replaced.
	/// The same character must not appear more than once in either literal-4 or
	/// identifier-6.
	/// </summary>
	public AlphanumericVariable ReplacingCharacterString { get; set; }

	/// <summary>
	/// p351:
	/// BEFORE and AFTER phrases (all formats)
	/// This phrase narrows the set of items being tallied or replaced.
	///
	/// No more than one BEFORE phrase and one AFTER phrase can be specified for any
	/// one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase.
	/// </summary>
	public CountingOrReplacingCondition[] ReplacingConditions { get; set; }
}

}
