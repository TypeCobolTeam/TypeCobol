﻿using System;
using JetBrains.Annotations;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using Object = System.Object;
using String = System.String;

namespace TypeCobol.Compiler.CodeElements
{
	public class DataType : ICobolLanguageLevel, IVisitable {
		public string Name { get; private set; }
        public RestrictionLevel RestrictionLevel { get; internal set; }
		public CobolLanguageLevel CobolLanguageLevel  { get; private set; }

		public DataType([NotNull] string name, RestrictionLevel restrictionLevel = RestrictionLevel.WEAK, CobolLanguageLevel cobolLanguageLevel = CobolLanguageLevel.Cobol85) {
			Name = name;
			if (name == null) throw new ArgumentNullException();
		    this.RestrictionLevel = restrictionLevel;
            this.CobolLanguageLevel = cobolLanguageLevel;
		}

		public override string ToString() { return Name; }

		public override int GetHashCode() { return Name.GetHashCode(); }
	    public bool AcceptASTVisitor(IASTVisitor astVisitor) {
	        return astVisitor.Visit(this);
	    }

	    public override bool Equals(object obj) {
			var other = obj as DataType;
			if (other == null) return false;
			return other == this;
		}

		public static bool operator ==(DataType x, DataType y)
        {
            //Data instance for Cobol85 are unique so we can compare reference
            if (Object.ReferenceEquals(x, y)) return true;
            if (x?.CobolLanguageLevel == CobolLanguageLevel.Cobol85 ||
                y?.CobolLanguageLevel == CobolLanguageLevel.Cobol85) return false;

            if (Object.ReferenceEquals(x, null) || Object.ReferenceEquals(y, null)) return false;
            return x.Name.Equals(y.Name, StringComparison.OrdinalIgnoreCase);
        }
		public static bool operator !=(DataType x, DataType y) {
			return !(x == y);
		}



		public static DataType CreateCustom(string name, RestrictionLevel restrictionLevel = RestrictionLevel.STRONG, CobolLanguageLevel cobolLanguageLevel = CobolLanguageLevel.Cobol2002) {
			foreach(var builtin in BuiltInCustomTypes)
				if (builtin.Name.Equals(name, StringComparison.InvariantCultureIgnoreCase))
					return builtin;
			return new DataType(name, restrictionLevel, cobolLanguageLevel);
		}

		public static DataType Create(string picture) {
			var basic = new char[]{'.','Z','+','-','*','D'/*,'B'*/,'C'/*,'S'*/};
			return doCreate(picture, basic);
		}

        /// <summary>
        /// Allows to create a numeric DataType for TYPEDEF with a usage clause that makes the Data Numeric. 
        /// </summary>
        /// <param name="usage"></param>
        /// <returns></returns>
	    public static DataType Create(DataUsage usage)
        {
            if (usage == DataUsage.Binary || usage == DataUsage.NativeBinary || usage == DataUsage.FloatingPoint ||
                usage == DataUsage.LongFloatingPoint || usage == DataUsage.PackedDecimal)
            {
                return DataType.Numeric;
            }
            return DataType.Unknown;
        }
        public static DataType Create(string picture, char[] currencies) {
			var basic = new char[]{'.','Z','+','-','*','D'/*,'B'*/,'C'/*,'S'*/};
			var all = new char[basic.Length + currencies.Length];
			basic.CopyTo(all, 0);
			currencies.CopyTo(all, basic.Length);
			return doCreate(picture, all);
		}
		private static DataType doCreate(string picture, char[] numericEditingSpecificChars) {
			//if (picture.Length > 50) return DataType.Unknown;// it's not our job to detect this
			char[] editingBase = new char[]{'B','0','/'};
			char[] editingNumeric = new char[editingBase.Length + numericEditingSpecificChars.Length];
			editingBase.CopyTo(editingNumeric, 0);
			numericEditingSpecificChars.CopyTo(editingNumeric, editingBase.Length);

			picture = remove(picture, '(',')');
			char[] chars = distinct(picture.ToUpper());
			if (contains(chars, 'E'))
				return DataType.FloatingPoint;// ±?E±99
			if (contains(chars, new char[]{'X'}))
				if (contains(chars, editingBase))
					 return DataType.AlphanumericEdited;
				else return DataType.Alphanumeric;
			if (contains(chars, new char[]{'A'}))
				return DataType.Alphabetic;
			if (contains(chars, new char[]{'G','N'}))
				return DataType.DBCS;
			if (contains(chars, editingNumeric))
				return DataType.NumericEdited;
			if (contains(chars, new char[]{'9','S','V','P'}))
				return DataType.Numeric;
			return DataType.Unknown;
		}
		/// <summary>Remove from string expr all substrings between characters begin and end.</summary>
		/// <param name="expr">string to be analyzed</param>
		/// <param name="begin">first character of the the substring to be removed</param>
		/// <param name="end">last character of the substrings to be removed</param>
		/// <returns>Resulting string</returns>
		private static string remove(string expr, char begin, char end) {
			string regex = string.Format("\\{0}.*?\\{1}", begin, end);
			return new System.Text.RegularExpressions.Regex(regex).Replace(expr, "");
		}
		// we don't want a dependency to Linq just for these
		private static bool contains(char[] array, char c) {
			return Array.IndexOf(array, c) > -1;
		}
		private static bool contains(char[] array, char[] chars) {
			foreach(char c in chars)
				if (contains(array, c)) return true;
			return false;
		}
		/// <summary>
		/// Return all distinct characters composing the input string.
		/// Even if present more than once in the input string, each character is only present once in the result array.
		/// A character not present in the input string won't be present in the result array.
		/// </summary>
		/// <param name="input">String to be analyzed</param>
		/// <returns>Distinct characters composing input.</returns>
		private static char[] distinct(string input) {
			System.Collections.Generic.HashSet<char> set = new System.Collections.Generic.HashSet<char>(input);
			char[] result = new char[set.Count];
			set.CopyTo(result);
			return result;
		}



		public static readonly DataType Unknown            = new DataType("?");
		public static readonly DataType Omitted            = new DataType("Omitted");
		public static readonly DataType Alphabetic         = new DataType("Alphabetic");
		public static readonly DataType Numeric            = new DataType("Numeric");
		public static readonly DataType NumericEdited      = new DataType("NumericEdited");
		public static readonly DataType Alphanumeric       = new DataType("Alphanumeric");
		public static readonly DataType AlphanumericEdited = new DataType("AlphanumericEdited");
		public static readonly DataType DBCS               = new DataType("DBCS");
		public static readonly DataType FloatingPoint      = new DataType("FloatingPoint");
		public static readonly DataType Occurs             = new DataType("Array");
        public static readonly DataType Level88            = new DataType("Level88");

        // [TYPECOBOL]
        //Boolean is marked CobolLanguageLevel.TypeCobol instead of Cobol2002 because it has a special behavior (with move and set) 
        public static readonly DataType Boolean            = new DataType("BOOL", RestrictionLevel.STRONG, CobolLanguageLevel.TypeCobol);
	    //Date is marked CobolLanguageLevel.TypeCobol instead of Cobol2002 because it has a special behavior: its property are private 
	    public static readonly DataType Date = new DataType("DATE", RestrictionLevel.STRONG, CobolLanguageLevel.TypeCobol);
	    //Currency is marked CobolLanguageLevel.TypeCobol instead of Cobol2002 because it has a special behavior: its property are private 
	    public static readonly DataType Currency = new DataType("CURRENCY", RestrictionLevel.STRONG, CobolLanguageLevel.TypeCobol);
        //String built in type
        public static readonly DataType String = new DataType("STRING", RestrictionLevel.STRONG, CobolLanguageLevel.TypeCobol);

        public static Nodes.TypeDefinition CreateBuiltIn(DataType type)
        {
            var dataTypeDescriptionEntry = CreateBuiltInDataTypeDescriptionEntry(type);
            Nodes.TypeDefinition typeDefinition;
            if (type == DataType.Date)
            {
                typeDefinition = new Nodes.TypeDefinition(dataTypeDescriptionEntry);
                typeDefinition.Add(CreateData(5, "YYYY", '9', 4, typeDefinition));
                typeDefinition.Add(CreateData(5, "MM", '9', 2, typeDefinition));
                typeDefinition.Add(CreateData(5, "DD", '9', 2, typeDefinition));
            }
            else if (type == DataType.Currency)
            {
                dataTypeDescriptionEntry.Picture = new GeneratedAlphanumericValue(string.Format("{0}({1})", 'X', 3));
                var tokenLine = TokensLine.CreateVirtualLineForInsertedToken(dataTypeDescriptionEntry.Line, "01 CURRENCY TYPEDEF STRICT PUBLIC PIC X(03).");
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.LevelNumber, 0, 1, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.UserDefinedWord, 3, 10, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.TYPEDEF, 12, 18, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.STRICT, 20, 25, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.PUBLIC, 27, 32, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.PIC, 34, 36, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.PictureCharacterString, 38, 42, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.PeriodSeparator, 43, 43, tokenLine));
                typeDefinition = new Nodes.TypeDefinition(dataTypeDescriptionEntry);
            }
            else // Boolean and String
            {
                typeDefinition = new Nodes.TypeDefinition(dataTypeDescriptionEntry);
            }
            typeDefinition.SetFlag(Node.Flag.NodeIsIntrinsic, true); //Mark BuiltIn Type as Instrinsic
            return typeDefinition;
        }

        private static DataTypeDescriptionEntry CreateBuiltInDataTypeDescriptionEntry(DataType type)
        {
            return new DataTypeDescriptionEntry
            {
                Visibility = AccessModifier.Public,
                LevelNumber = new GeneratedIntegerValue(1),
                DataName = new SymbolDefinition(new GeneratedAlphanumericValue(type.Name), SymbolType.DataName),
                DataType = type
            };
        }

        private static Nodes.DataDescription CreateData(int level, string name, char type, int length, TypeDefinition parentTypeDef) {
			var data = new DataDescriptionEntry();
			data.LevelNumber = new GeneratedIntegerValue(level);
			data.DataName = new SymbolDefinition(new GeneratedAlphanumericValue(name), SymbolType.DataName);
			data.Picture = new GeneratedAlphanumericValue(string.Format("{0}({1})", type, length));
			data.DataType = DataType.Create(data.Picture.Value);
            var node = new Nodes.DataDescription(data);
            node.ParentTypeDefinition = parentTypeDef;
			return node;
		}

		public static readonly DataType[] BuiltInCustomTypes = { DataType.Boolean, DataType.Date, DataType.Currency, DataType.String};
        // [/TYPECOBOL]
    }
    public enum RestrictionLevel
    {
        WEAK = 0,
        STRICT = 1,
        STRONG = 2
    }
}
