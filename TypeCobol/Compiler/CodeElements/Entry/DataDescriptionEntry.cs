using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements {

    /// <summary>
    /// Base class for all types of data definition entries :
    /// - DataDescriptionEntry
    /// - DataRedefinesEntry
    /// - DataRenamesEntry
    /// - DataConditionEntry
    ///
    /// DataDefinitionEntry
    ///     DataConditionEntry
    ///     CommonDataDescriptionAndDataRedefines
    ///         DataDescriptionEntry
    ///             DataTypeDescriptionEntry
    ///             SpecialRegisterDescriptionEntry
    ///             ParameterDescriptionEntry
    ///             FunctionCallResultDescriptionEntry
    ///         DataRedefinesEntry
    ///     DataRenamesEntry
    /// 
    /// </summary>
    public abstract class DataDefinitionEntry: NamedCodeElement {
	    protected DataDefinitionEntry(CodeElementType codeElementType) : base(codeElementType) {
	        this.DataType = DataType.Unknown;
	    }

		/// <summary>
		/// The level-number specifies the hierarchy of data within a record, and identifies
		/// special-purpose data entries. A level-number begins a data description entry, a
		/// renamed or redefined item, or a condition-name entry.
		/// A level-number has an integer value between 1 and 49, inclusive, or one of the
		/// special level-number values 66, 77, or 88.
		/// 66 
		/// Identifies items that must contain a RENAMES clause;
		/// such items regroup previously defined data items.
		/// (For details, see “RENAMES clause” on page 219.)
		/// 77 
		/// Identifies data item description entries that are independent
		/// WORKING-STORAGE, LOCAL-STORAGE, or LINKAGE SECTION items;
		/// they are not subdivisions of other items and are not subdivided themselves.
		/// 88 
		/// Identifies any condition-name entry that is associated
		/// with a particular value of a conditional variable.
		/// (For details, see “VALUE clause” on page 237.)
		/// </summary>
		[CanBeNull]
		public IntegerValue LevelNumber { get; set; }

		/// <summary>
		/// data-name-1 cannot be used as a qualifier; it can be qualified only by the
		/// names of level indicator entries or level-01 entries.
		///
		/// p187:
		/// Explicitly identifies the data being described.
		/// data-name-1, if specified, identifies a data item used in the program.
		/// data-name-1 must be the first word following the level-number.
		/// The data item can be changed during program execution.
		/// data-name-1 must be specified for level-66 and level-88 items. It must also
		/// be specified for any entry containing the GLOBAL or EXTERNAL clause,
		/// and for record description entries associated with file description entries
		/// that have the GLOBAL or EXTERNAL clauses.
		///
		/// Level-77 and level-01 entries in the WORKING-STORAGE, LOCAL-STORAGE,
		/// or LINKAGE SECTION that are referenced in a program or method must be given
		/// unique data-names because level-77 and level-01 entries cannot be qualified.
		/// Subordinate data-names that are referenced in the program or method must be
		/// either uniquely defined, or made unique through qualification.
		/// Unreferenced data-names need not be uniquely defined.
		/// </summary>
		public SymbolDefinition DataName { get; set; }

	    public override bool VisitCodeElement(IASTVisitor astVisitor) {
	        return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this) 
                && this.ContinueVisitToChildren(astVisitor, LevelNumber, DataName);
	    }

	    private string _Name;
	    public override string Name { get {
	        if (_Name == null)
	            _Name = DataName != null? DataName.Name : null;

	        return _Name;
	    } }

        private DataType _dataType;

        /// <summary>
        /// Get the DataType of this CodeElement
        /// Once the DataType has been set to a value different than DataType.Unknown, you cannot change it
        /// </summary>
        public virtual DataType DataType {
            [NotNull]
            get { return _dataType; }
            internal set
            {
                if (value == null)
                {
                    throw new ArgumentNullException(@"DataType", "DataType cannot be null");
                }
                if (_dataType == null || _dataType == DataType.Unknown) {
                    this._dataType = value;
                } else {
                    throw new InvalidOperationException("DataType is already set");
                }
            }
        }

        /// <summary>
        /// TODO This method should be split like this ?
        /// - PhysicalLength (or length used when data is written to a file)
        /// - PhysicalLengthWithChildren
        /// - LogicalLength
        /// - LogicalLengthWithChildren
        /// </summary>
        public abstract int Length{get;}
    }


    /// <summary>
    /// Common attributes between DataDescriptionEntry and DataRedefinesEntry
    /// </summary>
    public abstract class CommonDataDescriptionAndDataRedefines : DataDefinitionEntry {
        protected CommonDataDescriptionAndDataRedefines(CodeElementType codeElementType) : base(codeElementType) {
            
        }


        /// <summary>
        /// p187:
        /// A data item that is not explicitly referred to in a program. The keyword
        /// FILLER is optional. If specified, FILLER must be the first word following
        /// the level-number.
        /// The keyword FILLER can be used with a conditional variable if explicit
        /// reference is never made to the conditional variable but only to values that
        /// it can assume. FILLER cannot be used with a condition-name.
        /// In a MOVE CORRESPONDING statement or in an ADD CORRESPONDING or
        /// SUBTRACT CORRESPONDING statement, FILLER items are ignored.
        /// In an INITIALIZE statement, elementary FILLER items are ignored.
        ///
        /// If data-name-1 or the FILLER clause is omitted, the data item being described is
        /// treated as though FILLER had been specified.
        /// </summary>
        public SyntaxProperty<bool> Filler { get; internal set; }

        public bool IsFiller { get { return Filler != null && Filler.Value; } }

        /// <summary>
        /// p198:
        /// The PICTURE clause specifies the general characteristics and editing requirements
        /// of an elementary item.
        ///
        /// p199:
        /// The PICTURE clause must be specified for every elementary item except
        /// the following ones:
        /// - Index data items
        /// - The subject of the RENAMES clause
        /// - Items described with USAGE POINTER, USAGE FUNCTION-POINTER,
        ///   USAGE PROCEDURE-POINTER, or USAGE OBJECT REFERENCE
        /// - Internal floating-point data items
        /// In these cases, use of the PICTURE clause is prohibited.
        /// The PICTURE clause can be specified only at the elementary level.
        ///
        ///   character-string is made up of certain COBOL characters used as picture
        ///   symbols. The allowable combinations determine the category of the
        ///   elementary data item.
        ///
        ///   character-string can contain a maximum of 50 characters.
        /// </summary>
        public AlphanumericValue Picture { get; set; }

// [COBOL 2002]        
        /// <summary>
        /// COBOL 2002 TYPE clause.
        /// The TYPE clause allows a user-defined data type (or type name) to be used to define a data item. 
        /// This is done by specifying the type name (which is declared using the TYPEDEF clause) in a TYPE clause. 
        /// If the type name is a group item, then the defined data item will also be a group item: 
        /// its subordinate entries will correspond in name, hierarchy, and characteristics to those subordinate to the type name. 
        /// 
        /// type-name-1 
        /// The name of the type name that is to be used to define the subject data name.
        /// </summary>
        public SymbolReference UserDefinedDataType { get; internal set; }
// [/COBOL 2002]

        public override int Length {
            get {
                if (Picture == null) return 1;
                var parts = Picture.Value.Split('(',')');
                return parts.Length == 3 ? int.Parse(parts[1]) : 1;
            }
        }

        /// <summary>
        /// p188:
        /// The BLANK WHEN ZERO clause specifies that an item contains only spaces when
        /// its value is zero.
        ///
        /// The BLANK WHEN ZERO clause may be specified only for an elementary item
        /// described by its picture character string as category numeric-edited or numeric,
        /// without the picture symbol S or *. These items must be described, either implicitly
        /// or explicitly, as USAGE DISPLAY or USAGE NATIONAL.
        ///
        /// A BLANK WHEN ZERO clause that is specified for an item defined as numeric by
        /// its picture character string defines the item as category numeric-edited.
        /// </summary>
        public SyntaxProperty<bool> IsBlankWhenZero { get; set; }

        /// <summary>
        /// p189:
        /// The GLOBAL clause specifies that a data-name is available to every program
        /// contained within the program that declares it, as long as the contained program
        /// does not itself have a declaration for that name. All data-names subordinate to or
        /// condition-names or indexes associated with a global name are global names.
        /// A data-name is global if the GLOBAL clause is specified either in the data
        /// description entry by which the data-name is declared or in another entry to which
        /// that data description entry is subordinate. The GLOBAL clause can be specified in
        /// the WORKING-STORAGE SECTION, the FILE SECTION, the LINKAGE SECTION,
        /// and the LOCAL-STORAGE SECTION, but only in data description entries whose
        /// level-number is 01.
        /// In the same DATA DIVISION, the data description entries for any two data items
        /// for which the same data-name is specified must not include the GLOBAL clause.
        /// A statement in a program contained directly or indirectly within a program that
        /// describes a global name can reference that name without describing it again.
        /// Two programs in a run unit can reference common data in the following
        /// circumstances:
        /// * The data content of an external data record can be referenced from any program
        /// that describes the data record as external.
        /// * If a program is contained within another program, both programs can refer to
        /// data that possesses the global attribute either in the containing program or in
        /// any program that directly or indirectly contains the containing program.
        /// </summary>
        public SyntaxProperty<bool> Global { get; internal set; }

        public bool IsGlobal { get { return Global != null && Global.Value; } }

        /// <summary>
        /// p189:
        /// The JUSTIFIED clause overrides standard positioning rules for receiving items of
        /// category alphabetic, alphanumeric, DBCS, or national.
        ///
        /// You can specify the JUSTIFIED clause only at the elementary level. JUST is an
        /// abbreviation for JUSTIFIED, and has the same meaning.
        /// You cannot specify the JUSTIFIED clause:
        /// * For data items of category numeric, numeric-edited, alphanumeric-edited, or
        /// national-edited
        /// * For edited DBCS items
        /// * For index data items
        /// * For items described as USAGE FUNCTION-POINTER, USAGE POINTER,
        /// USAGE PROCEDURE-POINTER, or USAGE OBJECT REFERENCE
        /// * For external floating-point or internal floating-point items
        /// * With level-66 (RENAMES) and level-88 (condition-name) entries
        ///
        /// p190:
        /// When the JUSTIFIED clause is specified for a receiving item, the data is aligned at
        /// the rightmost character position in the receiving item. Also:
        /// * If the sending item is larger than the receiving item, the leftmost character
        /// positions are truncated.
        /// * If the sending item is smaller than the receiving item, the unused character
        /// positions at the left are filled with spaces. For a DBCS item, each unused
        /// position is filled with a DBCS space (X'4040'); for an item described with usage
        /// NATIONAL, each unused position is filled with the default Unicode space
        /// (NX'0020'); otherwise, each unused position is filled with an alphanumeric space.
        /// If you omit the JUSTIFIED clause, the rules for standard alignment are followed
        /// (see “Alignment rules” on page 166).
        /// The JUSTIFIED clause does not affect initial settings as determined by the VALUE
        /// clause.
        /// </summary>
        public SyntaxProperty<bool> IsJustified { get; set; }

        /// <summary>
        /// p190:
        /// A GROUP-USAGE clause with the NATIONAL phrase specifies that the group
        /// item defined by the entry is a national group item. A national group item contains
        /// national characters in all subordinate data items and subordinate group items.
        /// When GROUP-USAGE NATIONAL is specified:
        /// * The subject of the entry is a national group item. The class and category of a
        /// national group are national.
        /// * A USAGE clause must not be specified for the subject of the entry. A USAGE
        /// NATIONAL clause is implied.
        /// * A USAGE NATIONAL clause is implied for any subordinate elementary data
        /// items that are not described with a USAGE NATIONAL clause.
        /// * All subordinate elementary data items must be explicitly or implicitly described
        /// with USAGE NATIONAL.
        /// * Any signed numeric data items must be described with the SIGN IS SEPARATE clause.
        /// * A GROUP-USAGE NATIONAL clause is implied for any subordinate group
        /// items that are not described with a GROUP-USAGE NATIONAL clause.
        /// * All subordinate group items must be explicitly or implicitly described with a
        /// GROUP-USAGE NATIONAL clause.
        /// * The JUSTIFIED clause must not be specified.
        /// Unless stated otherwise, a national group item is processed as though it were an
        /// elementary data item of usage national, class and category national, described with
        /// PICTURE N(m), where m is the length of the group in national character positions.
        ///
        /// p191:
        /// Usage note: When you use national groups, the compiler can ensure proper
        /// truncation and padding of group items for statements such as MOVE and
        /// INSPECT. Groups defined without a GROUP-USAGE NATIONAL clause are
        /// alphanumeric groups. The content of alphanumeric groups, including any national
        /// characters, is treated as alphanumeric data, possibly leading to invalid truncation
        /// or mishandling of national character data.
        ///
        /// Where national group items are processed as groups
        /// Name qualification:
        /// The name of a national group item can be used to qualify the names of
        /// elementary data items and subordinate group items in the national group. The
        /// rules of qualification for a national group are the same as the rules of
        /// qualification for an alphanumeric group.
        /// RENAMES clause:
        /// The rules for a national group item specified in the THROUGH phrase are the
        /// same as the rules for an alphanumeric group item specified in the THROUGH
        /// phrase. The result is an alphanumeric group item.
        /// CORRESPONDING phrase:
        /// A national group item is processed as a group in accordance with the rules of
        /// the CORRESPONDING phrase. Elementary data items within a national group
        /// are processed the same as they would be if defined within an alphanumeric
        /// group.
        /// INITIALIZE statement:
        /// A national group item is processed as a group in accordance with the rules of
        /// the INITIALIZE statement. Elementary items within the national group are
        /// initialized the same as they would be if defined within an alphanumeric
        /// group.
        /// XML GENERATE statement:
        /// A national group item specified in the FROM phrase is processed as a group in
        /// accordance with the rules of the XML GENERATE statement. Elementary items
        /// within the national group are processed the same as they would be if defined
        /// within an alphanumeric group.
        /// </summary>
        public SyntaxProperty<bool> IsGroupUsageNational { get; set; }

        /// <summary>
        /// p191:
        /// The DATA DIVISION language elements used for table handling are the OCCURS
        /// clause and the INDEXED BY phrase.
        ///
        /// For the INDEXED BY phrase description, see “INDEXED BY phrase” on page 194.
        ///
        /// The OCCURS clause specifies tables whose elements can be referred to by indexing
        /// or subscripting. It also eliminates the need for separate entries for repeated data
        /// items.
        ///
        /// Formats for the OCCURS clause include fixed-length tables and variable-length
        /// tables.
        ///
        /// The subject of an OCCURS clause is the data-name of the data item that contains
        /// the OCCURS clause. Except for the OCCURS clause itself, data description clauses
        /// used with the subject apply to each occurrence of the item described.
        ///
        /// Whenever the subject of an OCCURS clause or any data-item subordinate to it is
        /// referenced, it must be subscripted or indexed, with the following exceptions:
        /// - When the subject of the OCCURS clause is used as the subject of a SEARCH
        ///   statement
        /// - When the subject or a subordinate data item is the object of the
        ///   ASCENDING/DESCENDING KEY phrase
        /// - When the subordinate data item is the object of the REDEFINES clause
        ///
        /// When subscripted or indexed, the subject refers to one occurrence within the table,
        /// unless the ALL subscript is used in an intrinsic function.
        ///
        /// The OCCURS clause cannot be specified in a data description entry that:
        /// - Has a level number of 01, 66, 77, or 88.
        /// - Describes a redefined data item. (However, a redefined item can be subordinate
        ///   to an item that contains an OCCURS clause.)
        ///
        /// p192:
        /// Fixed-length tables are specified using the OCCURS clause.
        ///
        /// Because seven subscripts or indexes are allowed, six nested levels and one
        /// outermost level of the format-1 OCCURS clause are allowed.
        /// </summary>
        public bool IsTableOccurence { get { return MaxOccurencesCount != null || HasUnboundedNumberOfOccurences != null; } }

        /// <summary>
        /// p192 (Fixed-length tables):
        /// The exact number of occurrences. [It] must be greater than zero.
        ///
        /// p195 (Variable-length tables):
        /// The minimum number of occurrences.
        /// The value (..) must be greater than or equal to zero, and it must
        /// also be less than the value of [MaxOccurencesCount].
        /// If (..) omitted, a value of 1 is assumed (..).
        /// </summary>
        public IntegerValue MinOccurencesCount { get; set; }

        /// <summary>
        /// p195 (Variable-length tables):
        /// The maximum number of occurrences.
        /// [It] must be greater than [MinOccurencesCount].
        /// ---
        /// Fixed-length tables are specified using the OCCURS clause.
        /// The length of the subject item is fixed. Only the number of repetitions of the subject
        /// item is variable.
        /// UNBOUNDED => Int32.MaxValue
        /// Unbounded maximum number of occurrences.
        /// </summary>
        public IntegerValue MaxOccurencesCount { get; set; }

        /// <summary>
        /// unbounded table 
        /// A table with OCCURS integer-1 to UNBOUNDED instead of specifying integer-2 as the upper bound.
        /// Unbounded group
        /// A group that contains at least one unbounded table.
        /// You can define unbounded groups only in the LINKAGE
        /// SECTION. Either alphanumeric groups or national groups can be
        /// unbounded.
        /// </summary>
        public SyntaxProperty<bool> HasUnboundedNumberOfOccurences { get; set; }

        /// <summary>
        /// p195:
        /// You can specify variable-length tables by using the OCCURS DEPENDING ON clause.
        ///
        /// p196:
        /// The OCCURS DEPENDING ON clause specifies variable-length tables.
        ///
        ///   data-name-1
        ///   Identifies the object of the OCCURS DEPENDING ON clause; that is, the
        ///   data item whose current value represents the current number of
        ///   occurrences of the subject item. The contents of items whose occurrence
        ///   numbers exceed the value of the object are undefined.
        ///   The object of the OCCURS DEPENDING ON clause (data-name-1) must
        ///   describe an integer data item.
        ///   The object of the OCCURS DEPENDING ON clause must not occupy any
        ///   storage position within the range of the table (that is, any storage position
        ///   from the first character position in the table through the last character
        ///   position in the table).
        ///   The object of the OCCURS DEPENDING ON clause cannot be variably
        ///   located; the object cannot follow an item that contains an OCCURS
        ///   DEPENDING ON clause.
        ///   If the OCCURS clause is specified in a data description entry included in a
        ///   record description entry that contains the EXTERNAL clause, data-name-1,
        ///   if specified, must reference a data item that possesses the external attribute.
        ///   data-name-1 must be described in the same DATA DIVISION as the subject
        ///   of the entry.
        ///   If the OCCURS clause is specified in a data description entry subordinate
        ///   to one that contains the GLOBAL clause, data-name-1, if specified, must be
        ///   a global name. data-name-1 must be described in the same DATA DIVISION
        ///   as the subject of the entry.
        ///
        /// p197:
        /// All data-names used in the OCCURS clause can be qualified; they cannot be
        /// subscripted or indexed.
        /// At the time that the group item, or any data item that contains a subordinate
        /// OCCURS DEPENDING ON item or that follows but is not subordinate to the
        /// OCCURS DEPENDING ON item, is referenced, the value of the object of the
        /// OCCURS DEPENDING ON clause must fall within the range integer-1 through
        /// integer-2, if identifier-2 is specified.
        /// The object of an OCCURS DEPENDING ON clause cannot be a nonsubordinate
        /// item that follows a complex ODO item.
        /// Any nonsubordinate item that follows an item described with an OCCURS
        /// DEPENDING ON clause is a variably located item. That is, its location is affected by
        /// the value of the OCCURS DEPENDING ON object.
        /// </summary>
        public NumericVariable OccursDependingOn { get; set; }

        /// <summary>
        /// p192:
        /// ASCENDING KEY and DESCENDING KEY phrases
        /// Data is arranged in ascending or descending order, depending on the keyword
        /// specified, according to the values contained in data-name-2. The data-names are
        /// listed in their descending order of significance.
        ///
        /// p193:
        /// The order is determined by the rules for comparison of operands (see “Relation
        /// conditions” on page 259). The ASCENDING KEY and DESCENDING KEY data
        /// items are used in OCCURS clauses and the SEARCH ALL statement for a binary
        /// search of the table element.
        ///
        ///   data-name-2
        ///   Must be the name of the subject entry or the name of an entry subordinate
        ///   to the subject entry. data-name-2 can be qualified.
        ///   If data-name-2 names the subject entry, that entire entry becomes the
        ///   ASCENDING KEY or DESCENDING KEY and is the only key that can be
        ///   specified for this table element.
        ///   If data-name-2 does not name the subject entry, then data-name-2:
        ///   - Must be subordinate to the subject of the table entry itself
        ///   - Must not be subordinate to, or follow, any other entry that contains an
        ///     OCCURS clause
        //    - Must not contain an OCCURS clause
        ///   data-name-2 must not have subordinate items that contain OCCURS
        ///   DEPENDING ON clauses.
        /// </summary>
        public TableSortingKey[] TableSortingKeys { get; set; }

        /// <summary>
        /// p194:
        /// The INDEXED BY phrase specifies the indexes that can be used with a table. A
        /// table without an INDEXED BY phrase can be referred to through indexing by
        /// using an index-name associated with another table.
        ///
        /// Indexes normally are allocated in static memory associated with the program that
        /// contains the table. Thus indexes are in the last-used state when a program is
        /// reentered. However, in the following cases, indexes are allocated on a
        /// per-invocation basis. Thus you must set the value of the index on every entry for
        /// indexes on tables in the following sections:
        /// - The LOCAL-STORAGE SECTION
        /// - The WORKING-STORAGE SECTION of a class definition (object instance
        ///   variables)
        /// - The LINKAGE SECTION of:
        ///   – Methods
        ///   – Programs compiled with the RECURSIVE clause
        ///   – Programs compiled with the THREAD option
        ///
        /// Indexes specified in an external data record do not possess the external attribute.
        ///
        ///   index-name-1
        ///   Each index-name specifies an index to be created by the compiler for use
        ///   by the program. These index-names are not data-names and are not
        ///   identified elsewhere in the COBOL program; instead, they can be regarded
        ///   as private special registers for the use of this object program only. They are
        ///   not data and are not part of any data hierarchy.
        ///
        ///   Unreferenced index names need not be uniquely defined.
        ///
        ///   In one table entry, up to 12 index-names can be specified.
        ///
        ///   If a data item that possesses the global attribute includes a table accessed
        ///   with an index, that index also possesses the global attribute. Therefore, the
        ///   scope of an index-name is the same as that of the data-name that names
        ///   the table in which the index is defined.
        /// </summary>
        public SymbolDefinition[] Indexes { get; set; }

        /// <summary>
        /// p221:
        /// The SIGN clause specifies the position and mode of representation of the
        /// operational sign for the signed numeric item to which it applies.
        /// The SIGN clause is required only when an explicit description of the properties or
        /// position of the operational sign is necessary.
        ///
        /// p222:
        /// The SIGN clause can be specified only for the following items:
        /// - An elementary numeric data item of usage DISPLAY or NATIONAL that is
        ///   described with an S in its picture character string, or
        /// - A group item that contains at least one such elementary entry as a subordinate
        ///   item
        /// When the SIGN clause is specified at the group level, that SIGN clause applies
        /// only to subordinate signed numeric elementary data items of usage DISPLAY or
        /// NATIONAL. Such a group can also contain items that are not affected by the SIGN
        /// clause. If the SIGN clause is specified for a group or elementary entry that is
        /// subordinate to a group item that has a SIGN clause, the SIGN clause for the
        /// subordinate entry takes precedence for that subordinate entry.
        /// The SIGN clause is treated as documentation for external floating-point items.
        /// When the SIGN clause is specified without the SEPARATE phrase, USAGE
        /// DISPLAY must be specified explicitly or implicitly. When SIGN IS SEPARATE is
        /// specified, either USAGE DISPLAY or USAGE NATIONAL can be specified.
        /// If you specify the CODE-SET clause in an FD entry, any signed numeric data
        /// description entries associated with that file description entry must be described
        /// with the SIGN IS SEPARATE clause.
        /// If the SEPARATE CHARACTER phrase is not specified, then:
        /// - The operational sign is presumed to be associated with the LEADING or
        ///   TRAILING digit position, whichever is specified, of the elementary numeric data
        ///   item. (In this instance, specification of SIGN IS TRAILING is the equivalent of
        ///   the standard action of the compiler.)
        /// - The character S in the PICTURE character string is not counted in determining
        ///   the size of the item (in terms of standard data format characters).
        /// If the SEPARATE CHARACTER phrase is specified, then:
        /// - The operational sign is presumed to be the LEADING or TRAILING character
        ///   position, whichever is specified, of the elementary numeric data item. This
        ///   character position is not a digit position.
        /// - The character S in the PICTURE character string is counted in determining the
        ///   size of the data item (in terms of standard data format characters).
        /// - + is the character used for the positive operational sign.
        /// - - is the character used for the negative operational sign.
        /// </summary>
        public SyntaxProperty<bool> SignIsSeparate { get; set; }

        /// <summary>
        /// p221:
        /// The SIGN clause specifies the position and mode of representation of the
        /// operational sign for the signed numeric item to which it applies.
        ///
        /// p222:
        /// The operational sign is presumed to be associated with the LEADING or
        /// TRAILING digit position, whichever is specified, of the elementary numeric data
        /// item. (In this instance, specification of SIGN IS TRAILING is the equivalent of
        /// the standard action of the compiler.)
        /// </summary>
        public SyntaxProperty<SignPosition> SignPosition { get; set; }

        /// <summary>
        /// p223:
        /// The SYNCHRONIZED clause specifies the alignment of an elementary item on a
        /// natural boundary in storage.
        /// The SYNCHRONIZED clause is never required, but can improve performance on
        /// some systems for binary items used in arithmetic.
        /// The SYNCHRONIZED clause can be specified for elementary items and for
        /// level-01 group items, in which case every elementary item within the group item is
        /// synchronized.
        ///
        /// When specified, the LEFT and the RIGHT phrases are syntax checked but have no
        /// effect on the execution of the program.
        /// The length of an elementary item is not affected by the SYNCHRONIZED clause.
        /// </summary>
        public SyntaxProperty<bool> IsSynchronized { get; set; }

        /// <summary>
        /// p228:
        /// The USAGE clause specifies the format in which data is represented in storage.
        ///
        /// p229:
        /// The USAGE clause can be specified for a data description entry with any
        /// level-number other than 66 or 88.
        ///
        /// p230:
        /// When specified at the group level, the USAGE clause applies to each elementary
        /// item in the group. The usage of elementary items must not contradict the usage of
        /// a group to which the elementary items belongs.
        /// A USAGE clause must not be specified in a group level entry for which a
        /// GROUP-USAGE NATIONAL clause is specified.
        /// When a GROUP-USAGE NATIONAL clause is specified or implied for a group
        /// level entry, USAGE NATIONAL must be specified or implied for every elementary
        /// item within the group.
        /// When the USAGE clause is not specified at either the group or elementary level, a
        /// usage clause is implied with:
        /// - Usage DISPLAY when the PICTURE clause contains only symbols other than G
        ///   or N
        /// - Usage NATIONAL when the PICTURE clause contains only one or more of the
        ///   symbol N and the NSYMBOL(NATIONAL) compiler option is in effect
        /// - Usage DISPLAY-1 when the PICTURE clause contains one or more of the symbol
        ///   N and the NSYMBOL(DBCS) compiler option is in effect
        /// NATIVE is treated as a comment in all phrases for which NATIVE is
        /// shown in the USAGE clause.
        /// </summary>
        public SyntaxProperty<DataUsage> Usage { get; set; }

        /// <summary>
        /// p234:
        /// A data item defined with the OBJECT REFERENCE phrase is an object reference. An
        /// object reference data item is a 4-byte elementary item.
        ///
        ///   class-name-1
        ///   An optional class name.
        ///   You must declare class-name-1 in the REPOSITORY paragraph in the
        ///   configuration section of the containing class or outermost program.
        ///   If specified, class-name-1 indicates that data-name-1 always refers to an
        ///   object-instance of class class-name-1 or a class derived from class-name-1.
        ///   Important: The programmer must ensure that the referenced object meets
        ///   this requirement; violations are not diagnosed.
        ///   If class-name-1 is not specified, the object reference can refer to an object of
        ///   any class. In this case, data-name-1 is a universal object reference.
        ///   You can specify data-name-1 within an alphanumeric group item without
        ///   affecting the semantics of the group item. There is no conversion of values
        ///   or other special handling of the object references when statements are
        ///   executed that operate on the group. The group continues to behave as an
        ///   alphanumeric group item.
        ///
        /// An object reference can be defined in any section of the DATA DIVISION of a
        /// factory definition, object definition, method, or program.
        /// An object-reference data item can be used in only:
        /// - A SET statement (format 7 only)
        /// - A relation condition
        /// - An INVOKE statement
        /// - The USING or RETURNING phrase of an INVOKE statement
        /// - The USING or RETURNING phrase of a CALL statement
        /// - A program procedure division or ENTRY statement USING or RETURNING phrase
        /// - A method procedure division USING or RETURNING phrase
        ///
        /// Object-reference data items:
        /// - Are ignored in CORRESPONDING operations
        /// - Are unaffected by INITIALIZE statements
        /// - Can be the subject or object of a REDEFINES clause
        /// - Cannot be a conditional variable
        /// - Can be written to a file (but upon subsequent reading of the record the content
        ///   of the object reference is undefined)
        ///
        /// A VALUE clause for an object-reference data item can contain only NULL or NULLS.
        /// You can use the SYNCHRONIZED clause with the USAGE OBJECT REFERENCE
        /// clause to obtain efficient alignment of the object-reference data item.
        /// The JUSTIFIED, PICTURE, and BLANK WHEN ZERO clauses cannot be used to
        /// describe group or elementary items defined with the USAGE OBJECT REFERENCE clause.
        /// </summary>
        public SymbolReference ObjectReferenceClass { get; set; }

        /// <summary>
        /// Specifies the initial value of a data item.
        ///
        /// p237:
        /// The VALUE clause specifies the initial contents of a data item or the values
        /// associated with a condition-name. The use of the VALUE clause differs depending
        /// on the DATA DIVISION section in which it is specified.
        ///
        /// A VALUE clause that is used in the FILE SECTION or the LINKAGE SECTION in
        /// an entry other than a condition-name entry is syntax checked, but has no effect on
        /// the execution of the program.
        ///
        /// In the WORKING-STORAGE SECTION and the LOCAL-STORAGE SECTION, the
        /// VALUE clause can be used in condition-name entries or in specifying the initial
        /// value of any data item. The data item assumes the specified value at the beginning
        /// of program execution. If the initial value is not explicitly specified, the value is
        /// unpredictable.
        ///
        /// p237:
        /// Initialization is independent of any BLANK WHEN ZERO or JUSTIFIED clause that is specified.
        ///
        /// A format-1 VALUE clause specified in a data description entry that contains or is
        /// subordinate to an OCCURS clause causes every occurrence of the associated data
        /// item to be assigned the specified value. Each structure that contains the
        /// DEPENDING ON phrase of the OCCURS clause is assumed to contain the
        /// maximum number of occurrences for the purposes of VALUE initialization.
        ///
        /// The VALUE clause must not be specified for a data description entry that contains
        /// or is subordinate to an entry that contains either an EXTERNAL or a REDEFINES
        /// clause. This rule does not apply to condition-name entries.
        ///
        /// A format-1 VALUE clause can be specified for an elementary data item or for a
        /// group item. When the VALUE clause is specified at the group level, the group area
        /// is initialized without consideration for the subordinate entries within the group.
        /// In addition, a VALUE clause must not be specified for subordinate entries
        /// within the group.
        ///
        /// For group items, the VALUE clause must not be specified if any subordinate
        /// entries contain a JUSTIFIED or SYNCHRONIZED clause.
        ///
        /// If the VALUE clause is specified for an alphanumeric group, all subordinate items
        /// must be explicitly or implicitly described with USAGE DISPLAY.
        ///
        /// The VALUE clause must not conflict with other clauses in the data description
        /// entry or in the data description of that entry's hierarchy.
        ///
        /// p238:
        /// The functions of the editing characters in a PICTURE clause are ignored in
        /// determining the initial value of the item described. However, editing characters are
        /// included in determining the size of the item. Therefore, any editing characters
        /// must be included in the literal. For example, if the item is defined as PICTURE
        /// +999.99 and the value is to be +12.34, then the VALUE clause should be specified
        /// as VALUE "+012.34".
        ///
        /// A VALUE clause cannot be specified for external floating-point items.
        /// A data item cannot contain a VALUE clause if the prior data item contains an
        /// OCCURS clause with the DEPENDING ON phrase.
        /// </summary>
        public Value InitialValue { get; set; }

        /// <summary>
        /// p242:
        /// This format assigns an invalid address as the initial value of an item defined as
        /// USAGE POINTER, USAGE PROCEDURE POINTER, or USAGE FUNCTION-POINTER.
        /// It also assigns an invalid object reference as the initial value
        /// of an item defined as USAGE OBJECT REFERENCE.
        ///
        /// VALUE IS NULL can be specified only for elementary items described
        /// implicitly or explicitly as USAGE POINTER, USAGE PROCEDURE-POINTER,
        /// USAGE FUNCTION-POINTER, or USAGE OBJECT REFERENCE.
        /// </summary>
        public bool IsInitialValueNull { get { return InitialValue.NullPointerValue != null; } }
    }

    /// <summary>
    /// A data description entry specifies the characteristics of a data item.
    /// Sets of data description entries are called record description entries.
    /// Data description entries that define independent data items do not make up a
    /// record. These entries are known as data item description entries.
    /// Data description entries have three general formats :
    /// Format 1: data description entry
    /// </summary>
    public class DataDescriptionEntry: CommonDataDescriptionAndDataRedefines, ITypedCodeElement
	{
		public DataDescriptionEntry(): base(CodeElementType.DataDescriptionEntry) { }



        /// <summary>
		/// p188:
		/// The EXTERNAL clause specifies that the storage associated with a data item is
		/// associated with the run unit rather than with any particular program or method
		/// within the run unit.
		/// An external data item can be referenced by any program or method in the run unit
		/// that describes the data item. References to an external data item from different
		/// programs or methods using separate descriptions of the data item are always to
		/// the same data item. In a run unit, there is only one representative of an external
		/// data item.
		/// The EXTERNAL clause can be specified only on data description entries whose
		/// level-number is 01. It can be specified only on data description entries that are in
		/// the WORKING-STORAGE SECTION of a program or method. It cannot be
		/// specified in LINKAGE SECTION or FILE SECTION data description entries. Any
		/// data item described by a data description entry subordinate to an entry that
		/// describes an external record also attains the external attribute. Indexes in an
		/// external data record do not possess the external attribute.
		/// The data contained in the record named by the data-name clause is external and
		/// can be accessed and processed by any program or method in the run unit that
		/// describes and, optionally, redefines it. This data is subject to the following rules:
		/// * If two or more programs or methods within a run unit describe the same
		/// external data record, each record-name of the associated record description
		/// entries must be the same, and the records must define the same number of
		/// bytes. However, a program or method that describes an external record can
		/// contain a data description entry including the REDEFINES clause that redefines
		/// the complete external record, and this complete redefinition need not occur
		/// identically in other programs or methods in the run unit.
		/// * Use of the EXTERNAL clause does not imply that the associated data-name is a
		/// global name.
		/// </summary>
		public SyntaxProperty<bool> External { get; internal set; }
		public bool IsExternal { get { return External != null && External.Value; } }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor,  Filler,
                                                                Picture,
                                                                UserDefinedDataType,
                                                                DataType,
                                                                IsBlankWhenZero,
                                                                External,
                                                                Global,
                                                                IsJustified,
                                                                IsGroupUsageNational,
                                                                MinOccurencesCount,
                                                                MaxOccurencesCount,
                                                                HasUnboundedNumberOfOccurences,
                                                                OccursDependingOn,
                                                                SignIsSeparate,
                                                                SignPosition,
                                                                IsSynchronized,
                                                                Usage,
                                                                ObjectReferenceClass,
                                                                InitialValue)
                   && this.ContinueVisitToChildren(astVisitor, TableSortingKeys, Indexes);
        }
	}
        
    /// <summary>
    /// Description of the data stored in a special register
    /// </summary>
    public class SpecialRegisterDescriptionEntry : DataDescriptionEntry
    {
        public SpecialRegisterDescriptionEntry(Token specialRegisterName, string storageAreaNameOrFileName) : base() {
            // Generate a unique symbol name for this special register
            var generatedSymbolName = new GeneratedSymbolName(specialRegisterName, specialRegisterName.Text + "-" + storageAreaNameOrFileName);
            DataName = new SymbolDefinition(generatedSymbolName, SymbolType.DataName);

            SpecialRegisterName = specialRegisterName;
        }

        public Token SpecialRegisterName { get; private set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, SpecialRegisterName);
        }
    }

    /// <summary>
    /// Description of the data returned by a function call
    /// </summary>
    public class FunctionCallResultDescriptionEntry : DataDescriptionEntry
    {
        public FunctionCallResultDescriptionEntry(FunctionCall functionCall, int callSiteId) {
            // Generate a unique symbol name for the function call at this specific call site
            var generatedSymbolName = new GeneratedSymbolName(functionCall.FunctionNameToken, functionCall.FunctionName + "-" + callSiteId);
            DataName = new SymbolDefinition(generatedSymbolName, SymbolType.DataName);

            FunctionCall = functionCall;
        }

        public FunctionCall FunctionCall { get; private set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, FunctionCall);
        }
    }

    /// <summary>
    /// TYPECOBOL : data conditions can be declared inline when a user defined function parameter is described
    /// </summary>
    public class ParameterDescriptionEntry : DataDescriptionEntry
    {
        // TODO#245
        // create an interface shared with DataDeclarationEntry
        // that aggregates all the non-illegal stuff like justified,
        // group usage national, blank when zero and so on

        public IList<DataConditionEntry> DataConditions { get; internal set; }
        public SyntaxProperty<bool> Omittable { get; set; }

        public bool IsOmittable {
            get { return Omittable != null && Omittable.Value; }
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, DataConditions);
        }
    }

    /// <summary>
    /// COBOL 2002 : user-defined data type description.
    /// TYPEDEF Clause
    /// The TYPEDEF clause is used to create a new user-defined data type, type-name.    
    /// After defining a new data type using the TYPEDEF clause, data items can be declared as this new data type using the TYPE clause.
    /// The TYPEDEF clause can only be specified for level 01 entries, which can also be group items. 
    /// If a group item is specified, all subordinate items of the group become part of the type declaration. 
    /// No storage is allocated for a type declaration.
    /// The TYPEDEF clause cannot be specified in the same data description entry as the following clauses: 
    /// EXTERNAL, REDEFINES.
    /// </summary>
    public class DataTypeDescriptionEntry : DataDescriptionEntry, IFormalizedCommentable
    {
        public DataTypeDescriptionEntry() : base() { }

        /// <summary>
        /// The name of the new user-defined data type is the subject of the TYPEDEF clause.
        /// Data-name-1 must be specified with the TYPEDEF clause: FILLER cannot be used. 
        /// </summary>
        public SymbolDefinition DataTypeName { get; set; }

        public SyntaxProperty<bool> Strong { get; internal set; }
        public SyntaxProperty<bool> Strict { get; internal set; }
        public AccessModifier Visibility { get; internal set; }

        public RestrictionLevel RestrictionLevel
        {
            get
            {
                return Strong.Value
                    ? RestrictionLevel.STRONG
                    : Strict.Value ? RestrictionLevel.STRICT : RestrictionLevel.WEAK;
            }
        }

        public FormalizedCommentDocumentation FormalizedCommentDocumentation { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, DataTypeName, Strong, Strict);
        }
    }

    /// <summary>
    /// p216:
    /// The REDEFINES clause allows you to use different data description entries to
    /// describe the same computer storage area
    /// </summary>
    public class DataRedefinesEntry: CommonDataDescriptionAndDataRedefines
    {
		public DataRedefinesEntry(): base(CodeElementType.DataRedefinesEntry) { }

		/// <summary>
		/// p216:
		/// The REDEFINES clause allows you to use different data description entries to
		/// describe the same computer storage area.
		/// When specified, the REDEFINES clause must be the first entry following
		/// data-name-1 or FILLER. If data-name-1 or FILLER is not specified, the REDEFINES
		/// clause must be the first entry following the level-number.
		/// data-name-1, FILLER
		/// Identifies an alternate description for the data area identified by
		/// data-name-2; data-name-1 is the redefining item or the REDEFINES subject.
		/// Neither data-name-1 nor any of its subordinate entries can contain a VALUE
		/// clause.
		/// data-name-2
		/// Identifies the redefined item or the REDEFINES object.
		/// The data description entry for data-name-2 can contain a REDEFINES
		/// clause.
		/// The data description entry for data-name-2 cannot contain an OCCURS
		/// clause. However, data-name-2 can be subordinate to an item whose data
		/// description entry contains an OCCURS clause; in this case, the reference to
		/// data-name-2 in the REDEFINES clause must not be subscripted.
		/// Neither data-name-1 nor data-name-2 can contain an OCCURS DEPENDING ON
		/// clause.
		/// data-name-1 and data-name-2 must have the same level in the hierarchy; however,
		/// the level numbers need not be the same. Neither data-name-1 nor data-name-2 can
		/// be defined with level number 66 or 88.
		/// data-name-1 and data-name-2 can each be described with any usage.
		/// Redefinition begins at data-name-1 and ends when a level-number less than or
		/// equal to that of data-name-1 is encountered. No entry that has a level-number
		/// numerically lower than those of data-name-1 and data-name-2 can occur between
		/// these entries.
		///
		/// p217:
		/// If the GLOBAL clause is used in the data description entry that contains the
		/// REDEFINES clause, only data-name-1 (the redefining item) possesses the global
		/// attribute. 
		/// The EXTERNAL clause must not be specified in the same data description entry as
		/// a REDEFINES clause.
		/// If the redefined data item (data-name-2) is declared to be an external data record,
		/// the size of the redefining data item (data-name-1) must not be greater than the size
		/// of the redefined data item. If the redefined data item is not declared to be an
		/// external data record, there is no such constraint.
		/// One or more redefinitions of the same storage area are permitted. The entries that
		/// give the new descriptions of the storage area must immediately follow the
		/// description of the redefined area without intervening entries that define new
		/// character positions. Multiple redefinitions can, but need not, all use the data-name
		/// of the original entry that defined this storage area. 
		/// Also, multiple redefinitions can use the name of the preceding definition.
		/// When more than one level-01 entry is written subordinate to an FD entry, a
		/// condition known as implicit redefinition occurs. That is, the second level-01 entry
		/// implicitly redefines the storage allotted for the first entry. In such level-01 entries,
		/// the REDEFINES clause must not be specified.
		/// When the data item implicitly redefines multiple 01-level records in a file
		/// description (FD) entry, items subordinate to the redefining or redefined item can
		/// contain an OCCURS DEPENDING ON clause.
		/// </summary>
		public SymbolReference RedefinesDataName { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, Filler, RedefinesDataName);
        }
    }

	/// <summary>
	/// The RENAMES clause specifies alternative and possibly overlapping groupings of
	/// elementary data items.
	/// The special level-number 66 must be specified for data description entries that
	/// contain the RENAMES clause.
	/// Format 2: renames
	/// Regroups previously defined items.
	/// All level-66 entries associated with one record must immediately follow the last
	/// data description entry in that record.
	/// </summary>
	public class DataRenamesEntry: DataDefinitionEntry
	{
		public DataRenamesEntry(): base(CodeElementType.DataRenamesEntry) { }

		/// <summary>
		/// Data-name-2, data-name-3
		/// Identify the original grouping of elementary data items; that is, they must
		/// name elementary or group items within the associated level-01 entry and
		/// must not be the same data-name. Both data-names can be qualified.
		/// data-name-2 and data-name-3 can each reference any of the following items:
		/// - An elementary data item
		/// - An alphanumeric group item
		/// - A national group item
		/// When data-name-2 or data-name-3 references a national group item, the
		/// referenced item is processed as a group (not as an elementary data item of
		/// category national).
		/// The OCCURS clause must not be specified in the data entries for
		/// data-name-2 and data-name-3, or for any group entry to which they are
		/// subordinate. In addition, the OCCURS DEPENDING clause must not be
		/// specified for any item defined between data-name-2 and data-name-3.
		/// When the THROUGH phrase is specified:
		/// - data-name-1 defines an alphanumeric group item that includes all the elementary
		///   items that:
		///   – Start with data-name-2 if it is an elementary item, or the first elementary item
		///     within data-name-2 if it is a group item
		///   – End with data-name-3 if it is an elementary item, or the last elementary item
		///     within data-name-3 if it is an alphanumeric group item or national group item
		/// - The storage area occupied by the starting item through the ending item becomes
		///   the storage area occupied by data-name-1.
		/// The leftmost character position in data-name-3 must not precede the leftmost
		/// character position in data-name-2, and the rightmost character position in
		/// data-name-3 must not precede the rightmost character position in data-name-2. This
		/// means that data-name-3 cannot be totally subordinate to data-name-2.
		/// When the THROUGH phrase is not specified:
		/// - The storage area occupied by data-name-2 becomes the storage area occupied by
		///   data-name-1.
		/// - All of the data attributes of data-name-2 become the data attributes for
		///   data-name-1. That is:
		///   – When data-name-2 is an alphanumeric group item, data-name-1 is an
		///     alphanumeric group item.
		///   – When data-name-2 is a national group item, data-name-1 is a national group
		///     item.
		///   – When data-name-2 is an elementary item, data-name-1 is an elementary item.
		/// </summary>
		public SymbolReference RenamesFromDataName { get; set; }
		public SymbolReference RenamesToDataName { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, RenamesFromDataName, RenamesToDataName);
        }

        public override int Length { get { return 1; } }
    }
    
    /// <summary>
    /// Format 3: condition-name
    /// A user-specified name that associates a value, a set of values, or a range of
    /// values with a conditional variable.
    /// Level-88 entries must immediately follow the data description entry for the
    /// conditional variable with which the condition-names are associated.
    /// </summary>
    public class DataConditionEntry: DataDefinitionEntry, ITypedCodeElement
	{
		public DataConditionEntry(): base(CodeElementType.DataConditionEntry) { }

		/// <summary>
		/// condition-name-1
		/// A user-specified name that associates a value with a conditional variable. If
		/// the associated conditional variable requires subscripts or indexes, each
		/// procedural reference to the condition-name must be subscripted or indexed
		/// as required for the conditional variable.
		/// Condition-names are tested procedurally in condition-name conditions.
		/// </summary>
		public SymbolDefinition ConditionName { get { return DataName; } }

		/// <summary>
		/// p239:
		/// Associates the condition-name with a single value.
		/// The class of literal-1 must be a valid class for assignment to the associated
		/// conditional variable.
		/// </summary>
		public Value[] ConditionValues { get; set; }

		/// <summary>
		/// p239:
		/// This format associates a value, values, or ranges of values with a condition-name.
		/// Each such condition-name requires a separate level-88 entry. Level-number 88 and
		/// the condition-name are not part of the format-2 VALUE clause itself. They are
		/// included in the format only for clarity.
		/// </summary>
		public ValuesRange[] ConditionValuesRanges { get; set; }

		public override DataType DataType { get { return DataType.Boolean; } }
		public override int Length { get { return 1; } }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, ConditionName, DataType)
                   && this.ContinueVisitToChildren(astVisitor, ConditionValues, ConditionValuesRanges);
        }
    }

	/// <summary>
	/// literal-1 THROUGH literal-2
	///   Associates the condition-name with at least one range of values. When the
	///   THROUGH phrase is used, literal-1 must be less than literal-2. For details,
	///   see “Rules for condition-name entries.”
	///
	///   literal-1 and literal-2 must be of the same class. The class of literal-1 and
	///   literal-2 must be a valid class for assignment to the associated conditional
	///   variable.
	///
	///   When literal-1 and literal-2 are DBCS literals, the range of DBCS values
	///   specified by the THROUGH phrase is based on the binary collating
	///   sequence of the hexadecimal values of the DBCS characters.
	///
	///   When literal-1 and literal-2 are national literals, the range of national
	///   character values specified by the THROUGH phrase is based on the binary
	///   collating sequence of the hexadecimal values of the national characters
	///   represented by the literals.
	///
	///   If the associated conditional variable is of class DBCS, literal-1 and literal-2
	///   must be DBCS literals. The figurative constant SPACE or the figurative
	///   constant ALL DBCS-literal can be specified.
	///
	///   If the associated conditional variable is of class national, literal-1 and
	///   literal-2 must be either both national literals or both alphanumeric literals
	///   for a given condition-name. The figurative constants ZERO, SPACE,
	///   QUOTE, HIGH-VALUE, LOW-VALUE, symbolic-character, ALL
	///   national-literal, or ALL literal can be specified.
	/// </summary>
	public class ValuesRange : IVisitable
	{
		public ValuesRange(Value minValue, Value maxValue)
		{
			MinValue = minValue;
			MaxValue = maxValue;
		}

		public Value MinValue { get; private set; }
		public Value MaxValue { get; private set; }
	    public bool AcceptASTVisitor(IASTVisitor astVisitor) {
	        return astVisitor.Visit(this) && this.ContinueVisitToChildren(astVisitor, MinValue, MaxValue);
	    }
	}

	/// <summary>
	/// ASCENDING KEY and DESCENDING KEY phrases
	/// Data is arranged in ascending or descending order, depending on the keyword specified.
	/// </summary>
	public enum SortDirection
	{
		None,
		Ascending,
		Descending
	}

	public class TableSortingKey : IVisitable
	{
		public TableSortingKey(SymbolReference sortKey, SyntaxProperty<SortDirection> sortDirection)
		{
			SortKey = sortKey;
			SortDirection = sortDirection;
		}

		public SymbolReference SortKey { get; private set; }

		public SyntaxProperty<SortDirection> SortDirection { get; private set; }
	    public bool AcceptASTVisitor(IASTVisitor astVisitor) {
	        return astVisitor.Visit(this) && this.ContinueVisitToChildren(astVisitor, SortKey, SortDirection);
	    }
	}

	/// <summary>
	/// The operational sign is presumed to be associated with the LEADING or
	/// TRAILING digit position, whichever is specified, of the elementary numeric data
	/// item.
	/// </summary>
	public enum SignPosition
	{
		None,
		Leading,
		Trailing
	}

	/// <summary>
	/// The USAGE clause specifies the format in which data is represented in storage.
	/// </summary>
	public enum DataUsage
	{
		None,
		/// <summary>
		/// p230: BINARY
		/// p231: COMPUTATIONAL or COMP (binary)
		/// p231: COMPUTATIONAL-4 or COMP-4 (binary)
		/// </summary>
		Binary,
		/// <summary>
		/// p231: COMPUTATIONAL-5 or COMP-5 (native binary)
		/// </summary>
		NativeBinary,
		/// <summary>
		/// p231: PACKED-DECIMAL
		/// p231: COMPUTATIONAL-3 or COMP-3 (internal decimal)
		/// </summary>
		PackedDecimal,
		/// <summary>
		//// p231: COMPUTATIONAL-1 or COMP-1 (floating-point)
		/// </summary>
		FloatingPoint,
		/// <summary>
		/// p231: COMPUTATIONAL-2 or COMP-2 (long floating-point)
		/// </summary>
		LongFloatingPoint,
		/// <summary>
		/// p232: DISPLAY phrase 
		/// </summary>
		Display,
		/// <summary>
		/// p233: DISPLAY-1 phrase
		/// </summary>
		DBCS,
		/// <summary>
		/// p233: FUNCTION-POINTER phrase 
		/// </summary>
		FunctionPointer,
		/// <summary>
		/// p233: INDEX phrase 
		/// Index data item
		/// An index data item is a data item that can hold the value of an index.
		/// You define an index data item by specifying the USAGE IS INDEX clause in a data
		/// description entry. The name of an index data item is a data-name. An index data
		/// item can be used anywhere a data-name or identifier can be used, unless stated
		/// otherwise in the rules of a particular statement. You can use the SET statement to
		/// save the value of an index (referenced by index-name) in an index data item.
		/// </summary>
		Index,
		/// <summary>
		/// p234: NATIONAL phrase
		/// </summary>
		National,
		/// <summary>
		/// p234: OBJECT REFERENCE phrase 
		/// </summary>
		ObjectReference,
		/// <summary>
		/// p235: POINTER phrase
		/// </summary>
		Pointer,
		/// <summary>
		/// p236: PROCEDURE-POINTER phrase 
		/// </summary>
		ProcedurePointer
	}
}
