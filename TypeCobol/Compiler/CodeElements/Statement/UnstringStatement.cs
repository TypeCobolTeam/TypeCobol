namespace TypeCobol.Compiler.CodeElements {

    using System;
    using System.Collections.Generic;
    using System.Text;
    using TypeCobol.Compiler.CodeElements.Expressions;

/// <summary>
/// p441: UNSTRING statement
/// The UNSTRING statement causes contiguous data in a sending field to be
/// separated and placed into multiple receiving fields.
/// </summary>
public class UnstringStatement: StatementElement, VariableWriter {
    public UnstringStatement(): base(CodeElementType.UnstringStatement, StatementType.UnstringStatement) { }

    /// <summary>
    /// identifier-1
    /// Represents the sending field. Data is transferred from this field to the data
    /// receiving fields (identifier-4).
    /// identifier-1 must reference a data item of category alphabetic, alphanumeric,
    /// alphanumeric-edited, DBCS, national, or national-edited.
    /// </summary>
    public Variable SendingField { get; set; }

    /// <summary>
    /// This phrase specifies delimiters within the data that control the data transfer.
    /// Each identifier-2, identifier-3, literal-1, or literal-2 represents one delimiter.
    /// Two or more delimiters
    /// When two or more delimiters are specified, an OR condition exists, and each
    /// nonoverlapping occurrence of any one of the delimiters is recognized in the
    /// sending field in the sequence specified.
    /// </summary>
    public Delimiter[] Delimiters { get; set; }

    /// <summary>
    /// This phrase specifies the fields where the data is to be moved.
    /// identifier-4 represents the data receiving fields.
    /// </summary>
    public Receiving[] ReceivingFields { get; set; }

    /// <summary>
    /// When the POINTER phrase is specified, the value of the pointer field, identifier-7,
    /// behaves as if it were increased by 1 for each examined character position in the
    /// sending field. When execution of the UNSTRING statement is completed, the
    /// pointer field contains a value equal to its initial value plus the number of character
    /// positions examined in the sending field.
    /// When this phrase is specified, the user must initialize the pointer field before
    /// execution of the UNSTRING statement begins.
    /// </summary>
    public ReceivingStorageArea CharacterPositionsExaminedInSendingField { get; set; }

    /// <summary>
    /// identifier-8
    /// Specifies a field that is incremented by the number of delimited fields
    /// processed.
    /// </summary>
    public ReceivingStorageArea NumberOfDelimitedFieldsProcessed { get; set; } 

    public override string ToString() {
        if (SendingField == null && Delimiters == null && ReceivingFields == null && CharacterPositionsExaminedInSendingField == null && NumberOfDelimitedFieldsProcessed == null)
            return base.ToString();
        var str = new StringBuilder("");
        if (SendingField != null) str.AppendLine("UNSTRING " + SendingField);
        if (Delimiters?.Length > 0) {
            str.Append(" DELIMITED BY ");
            foreach (var delimiter in Delimiters) str.Append(delimiter).Append(" OR ");
            str.Length -= 4;
            str.AppendLine();
        }
        if (ReceivingFields?.Length > 0) {
            str.Append(" INTO ");
            foreach (var receiver in ReceivingFields) str.Append(receiver);
            str.AppendLine();
        }
        if (CharacterPositionsExaminedInSendingField != null) str.Append(" WITH POINTER ").AppendLine(CharacterPositionsExaminedInSendingField.ToString());
        if (NumberOfDelimitedFieldsProcessed != null) str.Append(" TALLYING IN ").AppendLine(NumberOfDelimitedFieldsProcessed.ToString());
        return str.ToString();
    }

    private IDictionary<StorageArea,object> variables;
    public  IDictionary<StorageArea, object> Variables {
        get {
            if (variables != null) return variables;
            variables = new Dictionary<StorageArea, object>();
            foreach(var kv in VariablesWritten) variables.Add(kv.Key, kv.Value);
            if (SendingField?.StorageArea != null && !SendingField.IsLiteral) variables.Add(SendingField.StorageArea, null);
            return variables;
        }
    }
    private IDictionary<StorageArea, object> variablesWritten;
    public  IDictionary<StorageArea, object> VariablesWritten {
        get {
            if (variablesWritten != null) return variablesWritten;
            variablesWritten = new Dictionary<StorageArea, object>();
            if (ReceivingFields == null) return variablesWritten;
            string sending = SendingField == null? null : SendingField.ToString();
            foreach(var field in ReceivingFields)
            {
                if (field.ReceivingField?.StorageArea == null) continue;
        
                if (!variablesWritten.ContainsKey(field.ReceivingField.StorageArea)) {
                    variablesWritten.Add(field.ReceivingField.StorageArea, sending);
                }
                
            }
            return variablesWritten;
        }
    }
    public bool IsUnsafe { get { return false; }  }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, SendingField, CharacterPositionsExaminedInSendingField,
                   NumberOfDelimitedFieldsProcessed)
                   && this.ContinueVisitToChildren(astVisitor, Delimiters, ReceivingFields);
        }


        public class Delimiter : IVisitable {
        /// <summary>
        /// ALL
        /// Multiple contiguous occurrences of any delimiters are treated as if there
        /// were only one occurrence; this one occurrence is moved to the delimiter
        /// receiving field (identifier-5), if specified. The delimiting characters in the
        /// sending field are treated as an elementary item of the same usage and
        /// category as identifier-1 and are moved into the current delimiter receiving
        /// field according to the rules of the MOVE statement.
        /// When DELIMITED BY ALL is not specified, and two or more contiguous
        /// occurrences of any delimiter are encountered, the current data receiving
        /// field (identifier-4) is filled with spaces or zeros, according to the description
        /// of the data receiving field.
        /// </summary>
        public SyntaxProperty<bool> All { get; set; }

        /// <summary>
        /// This phrase specifies delimiters within the data that control the data transfer.
        /// Each identifier-2, identifier-3, literal-1, or literal-2 represents one delimiter.
        /// Delimiter with two or more characters
        /// A delimiter that contains two or more characters is recognized as a delimiter only
        /// if the delimiting characters are in both of the following cases:
        /// - Contiguous
        /// - In the sequence specified in the sending field
        /// </summary>
        public Variable DelimiterCharacters { get; set; }

        public override string ToString() {
            return (All != null && All.Value ? "ALL " : "") + (DelimiterCharacters != null ? DelimiterCharacters.ToString() : "?");
        }

        public bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return this.ContinueVisitToChildren(astVisitor, All, DelimiterCharacters);
        }
    }

    public class Receiving : IVisitable {
        /// <summary>
        /// This phrase specifies the fields where the data is to be moved.
        /// identifier-4 represents the data receiving fields.
        /// </summary>
        public ReceivingStorageArea ReceivingField { get; set; }

        /// <summary>
        /// This phrase specifies the fields where the delimiters are to be moved.
        /// identifier-5 represents the delimiter receiving fields.
        /// The DELIMITER IN phrase must not be specified if the DELIMITED BY
        /// phrase is not specified.
        /// </summary>
        public ReceivingStorageArea DelimiterReceivingField { get; set; }

        /// <summary>
        /// This phrase specifies the field where the count of examined character
        /// positions is held.
        /// identifier-6 is the data count field for each data transfer. Each field holds the
        /// count of examined character positions in the sending field, terminated by
        /// the delimiters or the end of the sending field, for the move to this
        /// receiving field. The delimiters are not included in this count.
        /// The COUNT IN phrase must not be specified if the DELIMITED BY phrase
        /// is not specified.
        /// </summary>
        public ReceivingStorageArea CharTransferredCount { get; set; }

        public override string ToString()
        {
            var str = new StringBuilder();
            if (ReceivingField != null) str.Append(ReceivingField);
            else str.Append("?");
            if (DelimiterReceivingField != null) str.Append(" DELIMITER IN ").Append(DelimiterReceivingField);
            if (CharTransferredCount != null) str.Append(" COUNT IN ").Append(CharTransferredCount);
            return str.ToString();
        }

        public bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return this.ContinueVisitToChildren(astVisitor, ReceivingField, DelimiterReceivingField, CharTransferredCount);
        }
    }

}

}