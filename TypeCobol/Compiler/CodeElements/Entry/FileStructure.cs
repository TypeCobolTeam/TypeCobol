using System;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
	/// <summary>
	/// The ORGANIZATION clause identifies the logical structure of the file.
	/// The logical structure is established at the time the file is created and cannot subsequently be changed.
	/// </summary>
	public enum FileRecordsOrganization
	{
		/// <summary>
		/// A predecessor-successor relationship among the records in the file is established
		/// by the order in which records are placed in the file when it is created or extended.
		/// </summary>
		Sequential,
		/// <summary>
		/// The position of each logical record in the file is determined by indexes
		/// created with the file and maintained by the system.
		/// The indexes are based on embedded keys within the file's records.
		/// </summary>
		Indexed,
		/// <summary>
		/// The position of each logical record in the file is determined by its relative record number.
		/// </summary>
		Relative,
		/// <summary>
		/// A predecessor-successor relationship among the records in the file is established
		/// by the order in which records are placed in the file when it is created or extended.
		//// A record in a LINE SEQUENTIAL file can consist only of printable characters.
		/// </summary>
		LineSequential
	}

	/// <summary>
	/// The FILE-CONTROL paragraph specifies file organization, and other information.
	/// </summary>
	public abstract class FileStructure
	{
		/// <summary>
		/// The ORGANIZATION clause identifies the logical structure of the file. 
		/// The logical structure is established at the time the file is created and cannot subsequently be changed.
		/// ORGANIZATION IS SEQUENTIAL (format 1) 
		/// A predecessor-successor relationship among the records in the file is established by the order in which records are placed in the file when it is created or extended. 
		/// ORGANIZATION IS INDEXED (format 2) 
		/// The position of each logical record in the file is determined by indexes created with the file and maintained by the system. 
		/// The indexes are based on embedded keys within the file's records. 
		/// ORGANIZATION IS RELATIVE (format 3) 
		/// The position of each logical record in the file is determined by its relative record number. 
		/// ORGANIZATION IS LINE SEQUENTIAL (format 4)
		/// A predecessor-successor relationship among the records in the file is established by the order in which records are placed in the file when it is created or extended. 
		/// A record in a LINE SEQUENTIAL file can consist only of printable characters.
		/// </summary>
		public SyntaxProperty<FileRecordsOrganization> RecordsOrganization { get; set; }
	}

	/// <summary>
	/// ORGANIZATION IS SEQUENTIAL (format 1) 
	/// A predecessor-successor relationship among the records in the file is established by the order in which records are placed in the file when it is created or extended. 
	/// </summary>
	public class SequentialFileStructure : FileStructure
	{
		// RecordsOrganization = FileRecordsOrganization.Sequential

		/// <summary>
		/// The PADDING CHARACTER clause specifies a character to be used for block padding on sequential files. 
		/// The PADDING CHARACTER clause is syntax checked, but has no effect on the execution of the program.
		/// </summary>
		public CharacterVariable PaddingCharacter { get; set; }

		/// <summary>
		/// The RECORD DELIMITER clause indicates the method of determining the length of a variable-length record on an external medium. 
		/// It can be specified only for variable-length records. 
		/// Can be any COBOL word.
		/// The RECORD DELIMITER clause is syntax checked, but has no effect on the execution of the program.
		/// </summary>
		public Token RecordDelimiter { get; set; }

		/// <summary>
		/// The PASSWORD clause controls access to files. 
		/// Password data item. 
		/// Must be defined in the WORKING-STORAGE SECTION of the DATA DIVISION as a data item of category alphabetic, alphanumeric, or alphanumeric-edited.
		/// The first eight characters are used as the password; a shorter field is padded with blanks to eight characters. 
		/// When the PASSWORD clause is specified, at object time the PASSWORD data item must contain a valid password for this file before the file can be successfully opened.
		/// Format 1 considerations:
		/// The PASSWORD clause is not valid for QSAM sequential files.
		/// </summary>
		public SymbolReference Password { get; set; }
	}

	/// <summary>
	/// ORGANIZATION IS INDEXED (format 2) 
	/// The position of each logical record in the file is determined by indexes created with the file and maintained by the system. 
	/// The indexes are based on embedded keys within the file's records. 
	/// </summary>
	public class IndexedFileStructure : FileStructure
	{
		// RecordsOrganization = FileRecordsOrganization.Indexed

		/// <summary>
		/// The RECORD KEY clause (format 2) specifies the data item within the record that is the prime RECORD KEY for an indexed file. 
		/// The values contained in the prime RECORD KEY data item must be unique among records in the file.
		/// data-name-2 must be described within a record description entry associated with the file. 
		/// Regardless of the category of the key data item, the key is treated as an alphanumeric item. 
		/// The collation order of the key is determined by the item's binary value order when the key is used for locating a record or for setting the file position indicator associated with the file.
		/// data-name-2 must not reference a group item that contains a variable-occurrence data item. 
		/// If the indexed file contains variable-length records, data-name-2 need not be contained within the minimum record size specified for the file. That is, data-name-2 can exceed the minimum record size, but this is not recommended. 
		/// The data description of data-name-2 and its relative location within the record must be the same as those used when the file was defined.
		/// If the file has more than one record description entry, data-name-2 need be described in only one of those record description entries. The identical character positions referenced by data-name-2 in any one record description entry are implicitly referenced as keys for all other record description entries for that file.
		/// </summary>
		public SymbolReference RecordKey { get; set; }

		/// <summary>
		/// The PASSWORD clause controls access to files. 
		/// Password data item. 
		/// Must be defined in the WORKING-STORAGE SECTION of the DATA DIVISION as a data item of category alphabetic, alphanumeric, or alphanumeric-edited.
		/// The first eight characters are used as the password; a shorter field is padded with blanks to eight characters. 
		/// When the PASSWORD clause is specified, at object time the PASSWORD data item must contain a valid password for this file before the file can be successfully opened.
		/// Format 2 considerations :
		/// The PASSWORD clause, if specified, must immediately follow the RECORD KEY or ALTERNATE RECORD KEY data-name with which it is associated.
		/// For indexed files that have been completely predefined to VSAM, only the PASSWORD data item for the RECORD KEY need contain the valid password before the file can be successfully opened at file creation time.
		/// For any other type of file processing (including the processing of dynamic calls at file creation time through a COBOL runtime subroutine), every PASSWORD data item for the file must contain a valid password before the file can be successfully opened, regardless of whether all paths to the data are used in this object program.
		/// </summary>
		public SymbolReference Password { get; set; }

		/// <summary>
		///  The ALTERNATE RECORD KEY clause (format 2) specifies a data item within the record that provides an alternative path to the data in an indexed file. 
		/// </summary>
		public AlternateRecordKey[] AlternateRecordKeys { get; set; }
	}

	/// <summary>
	///  The ALTERNATE RECORD KEY clause (format 2) specifies a data item within the record that provides an alternative path to the data in an indexed file. 
	/// </summary>
	public class AlternateRecordKey
	{
		/// <summary>
		/// data-name-3 must be described within a record description entry associated with the file. 
		/// Regardless of the category of the key data item, the key is treated as an alphanumeric item. 
		/// The collation order of the key is determined by the item's binary value order when the key is used for locating a record or for setting the file position indicator associated with the file. 
		/// data-name-3 must not reference a group item that contains a variable-occurrence data item. 
		/// If the indexed file contains variable-length records, data-name-3 need not be contained within the minimum record size specified for the file. That is, data-name-3 can exceed the minimum record size, but this is not recommended. 
		/// If the file has more than one record description entry, data-name-3 need be described in only one of these record description entries. The identical character positions referenced by data-name-3 in any one record description entry are implicitly referenced as keys for all other record description entries of that file. 
		/// The data description of data-name-3 and its relative location within the record must be the same as those used when the file was defined. The number of alternate record keys for the file must also be the same as that used when the file was created. 
		/// The leftmost character position of data-name-3 must not be the same as the leftmost character position of the prime RECORD KEY or of any other ALTERNATE RECORD KEY.
		/// </summary>
		public SymbolReference RecordKey { get; set; }

		/// <summary>
		/// If the DUPLICATES phrase is not specified, the values contained in the ALTERNATE RECORD KEY data item must be unique among records in the file.
		/// If the DUPLICATES phrase is specified, the values contained in the ALTERNATE RECORD KEY data item can be duplicated within any records in the file. 
		/// In sequential access, the records with duplicate keys are retrieved in the order in which they were placed in the file. 
		/// In random access, only the first record written in a series of records with duplicate keys can be retrieved.
		/// </summary>        
		public SyntaxProperty<bool> AllowDuplicates { get; set; }

		/// <summary>
		/// The PASSWORD clause controls access to files. 
		/// Password data item. 
		/// Must be defined in the WORKING-STORAGE SECTION of the DATA DIVISION as a data item of category alphabetic, alphanumeric, or alphanumeric-edited.
		/// The first eight characters are used as the password; a shorter field is padded with blanks to eight characters. 
		/// When the PASSWORD clause is specified, at object time the PASSWORD data item must contain a valid password for this file before the file can be successfully opened.
		/// Format 2 considerations :
		/// The PASSWORD clause, if specified, must immediately follow the RECORD KEY or ALTERNATE RECORD KEY data-name with which it is associated.
		/// For indexed files that have been completely predefined to VSAM, only the PASSWORD data item for the RECORD KEY need contain the valid password before the file can be successfully opened at file creation time.
		/// For any other type of file processing (including the processing of dynamic calls at file creation time through a COBOL runtime subroutine), every PASSWORD data item for the file must contain a valid password before the file can be successfully opened, regardless of whether all paths to the data are used in this object program.
		/// </summary>
		public SymbolReference Password { get; set; }
	}

	/// <summary>
	/// ORGANIZATION IS RELATIVE (format 3) 
	/// The position of each logical record in the file is determined by its relative record number. 
	/// </summary>
	public class RelativeFileStructure : FileStructure
	{
		// RecordsOrganization = FileRecordsOrganization.Relative

		/// <summary>
		/// The RELATIVE KEY clause (format 3) identifies a data-name that specifies the relative record number for a specific logical record within a relative file. 
		/// data-name-4 Must be defined as an unsigned integer data item whose description does not contain the PICTURE symbol P. 
		/// data-name-4 must not be defined in a record description entry associated with this relative file. That is, the RELATIVE KEY is not part of the record. 
		/// data-name-4 is required for ACCESS IS SEQUENTIAL only when the START statement is to be used. It is always required for ACCESS IS RANDOM and ACCESS IS DYNAMIC.
		/// When the START statement is executed, the system uses the contents of the RELATIVE KEY data item to determine the record at which sequential processing is to begin.
		/// If a value is placed in data-name-4, and a START statement is not executed, the value is ignored and processing begins with the first record in the file. 
		/// If a relative file is to be referenced by a START statement, you must specify the RELATIVE KEY clause for that file. 
		/// </summary>
		public SymbolReference RelativeKey { get; set; }

		/// <summary>
		/// The PASSWORD clause controls access to files. 
		/// Password data item. 
		/// Must be defined in the WORKING-STORAGE SECTION of the DATA DIVISION as a data item of category alphabetic, alphanumeric, or alphanumeric-edited.
		/// The first eight characters are used as the password; a shorter field is padded with blanks to eight characters. 
		/// When the PASSWORD clause is specified, at object time the PASSWORD data item must contain a valid password for this file before the file can be successfully opened.
		/// </summary>
		public SymbolReference Password { get; set; }
	}
}
