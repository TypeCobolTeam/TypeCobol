using JetBrains.Annotations;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Types;

namespace TypeCobol.Compiler.CodeElements
{
    public class DataType : ICobolLanguageLevel, IVisitable, IEquatable<DataType>
    {
        public string Name { get; }
        public RestrictionLevel RestrictionLevel { get; internal set; }
        public CobolLanguageLevel CobolLanguageLevel  { get; }

        public DataType([NotNull] string name, RestrictionLevel restrictionLevel = RestrictionLevel.WEAK, CobolLanguageLevel cobolLanguageLevel = CobolLanguageLevel.Cobol85) {
            Name = name;
            if (name == null) throw new ArgumentNullException();
            this.RestrictionLevel = restrictionLevel;
            this.CobolLanguageLevel = cobolLanguageLevel;
        }

        public override string ToString() { return Name; }

        public bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }

        public override bool Equals(object? obj) {
            return Equals(obj as DataType);
        }

        public bool Equals(DataType? dataType)
        {
            if (object.ReferenceEquals(this, dataType)) return true;
            if (object.ReferenceEquals(null, dataType)) return false;

            return string.Equals(Name, dataType.Name, StringComparison.OrdinalIgnoreCase)
                   && RestrictionLevel.Equals(dataType.RestrictionLevel)
                   && CobolLanguageLevel.Equals(dataType.CobolLanguageLevel);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = 13;
                hashCode = (hashCode * 397) ^ Name.GetHashCode();
                hashCode = (hashCode * 397) ^ RestrictionLevel.GetHashCode();
                hashCode = (hashCode * 397) ^ CobolLanguageLevel.GetHashCode();

                return hashCode;
            }
        }

        public static bool operator ==(DataType? x, DataType? y)
        {
            return x?.Equals(y) ?? object.ReferenceEquals(y, null);
        }
        public static bool operator !=(DataType? x, DataType? y)
        {
            return !(x == y);
        }

        public static DataType CreateCustom(string name, RestrictionLevel restrictionLevel = RestrictionLevel.STRONG, CobolLanguageLevel cobolLanguageLevel = CobolLanguageLevel.Cobol2002) {
            foreach(var builtin in BuiltInCustomTypes)
                if (builtin.Name.Equals(name, StringComparison.InvariantCultureIgnoreCase))
                    return builtin;
            return new DataType(name, restrictionLevel, cobolLanguageLevel);
        }
        
        public static DataType Create(PictureValidator.Result pictureValidatorResult)
        {
            switch (pictureValidatorResult.Category)
            {
                case PictureCategory.Invalid:
                    return DataType.Unknown;
                case PictureCategory.Alphabetic:
                    return DataType.Alphabetic;
                case PictureCategory.Alphanumeric:
                    return DataType.Alphanumeric;
                case PictureCategory.AlphanumericEdited:
                    return DataType.AlphanumericEdited;
                case PictureCategory.DBCS:
                    return DataType.DBCS;
                case PictureCategory.National:
                    return DataType.National;
                case PictureCategory.NationalEdited:
                    return DataType.NationalEdited;
                case PictureCategory.ExternalFloatingPoint:
                    return DataType.FloatingPoint;
                case PictureCategory.Numeric:
                    return DataType.Numeric;
                case PictureCategory.NumericEdited:
                    return DataType.NumericEdited;
                default:
                    //Exception to detect new PictureCategory not handled here
                    throw new NotImplementedException("Unknown pictureValidatorResult.Category: " + pictureValidatorResult.Category);
            }
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
        
        public static readonly DataType Unknown            = new DataType("?");
        public static readonly DataType Omitted            = new DataType("Omitted");
        public static readonly DataType Alphabetic         = new DataType("Alphabetic");
        public static readonly DataType Numeric            = new DataType("Numeric");
        public static readonly DataType NumericEdited      = new DataType("NumericEdited");
        public static readonly DataType Alphanumeric       = new DataType("Alphanumeric");
        public static readonly DataType AlphanumericEdited = new DataType("AlphanumericEdited");
        public static readonly DataType National           = new DataType("National");
        public static readonly DataType NationalEdited     = new DataType("NationalEdited");
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

        public static readonly DataType[] BuiltInCustomTypes = { DataType.Boolean, DataType.Date, DataType.Currency, DataType.String };

        private static Nodes.TypeDefinition CreateBuiltIn(DataType type)
        {
            var dataTypeDescriptionEntry = CreateBuiltInDataTypeDescriptionEntry(type);
            Nodes.TypeDefinition typeDefinition;
            if (type == DataType.Date)
            {
                typeDefinition = new Nodes.TypeDefinition(dataTypeDescriptionEntry);
                typeDefinition.Add(CreateNumericData(5, "YYYY", 4, typeDefinition));
                typeDefinition.Add(CreateNumericData(5, "MM", 2, typeDefinition));
                typeDefinition.Add(CreateNumericData(5, "DD", 2, typeDefinition));
                typeDefinition.SemanticData = Compiler.Symbols.Builtins.Date;
            }
            else if (type == DataType.Currency)
            {
                dataTypeDescriptionEntry.Picture = new GeneratedAlphanumericValue("X(03)");
                dataTypeDescriptionEntry.PictureValidationResult = new PictureValidator.Result(
                    new[] { new Character(SC.X, 3) },
                    null,
                    PictureCategory.Alphanumeric,
                    0,
                    0,
                    false,
                    0,
                    3);
                var tokenLine = TokensLine.CreateVirtualLineForInsertedToken(dataTypeDescriptionEntry.Line, "01 CURRENCY TYPEDEF STRICT PUBLIC PIC X(03).", Text.ColumnsLayout.FreeTextFormat);
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.LevelNumber, 0, 1, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.UserDefinedWord, 3, 10, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.TYPEDEF, 12, 18, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.STRICT, 20, 25, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.PUBLIC, 27, 32, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.PIC, 34, 36, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.PictureCharacterString, 38, 42, tokenLine));
                dataTypeDescriptionEntry.ConsumedTokens.Add(new Token(TokenType.PeriodSeparator, 43, 43, tokenLine));
                typeDefinition = new Nodes.TypeDefinition(dataTypeDescriptionEntry);
                typeDefinition.SemanticData = Compiler.Symbols.Builtins.Currency;
            }
            else if (type == DataType.Boolean)
            {
                typeDefinition = new Nodes.TypeDefinition(dataTypeDescriptionEntry);
                typeDefinition.SemanticData = Compiler.Symbols.Builtins.Boolean;
            }
            else if (type == DataType.String)
            {
                typeDefinition = new Nodes.TypeDefinition(dataTypeDescriptionEntry);
                typeDefinition.SemanticData = Compiler.Symbols.Builtins.String;
            }
            else
            {
                throw new NotSupportedException($"Unsupported built-in type '{type.Name}'.");
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
                DataType = type,
                Strong = new SyntaxProperty<bool>(true, null)
            };
        }

        private static Nodes.DataDescription CreateNumericData(int level, string name, int length, TypeDefinition parentTypeDef) {
            var data = new DataDescriptionEntry();
            data.LevelNumber = new GeneratedIntegerValue(level);
            data.DataName = new SymbolDefinition(new GeneratedAlphanumericValue(name), SymbolType.DataName);
            string pictureCharacterString = $"9({length})";
            data.Picture = new GeneratedAlphanumericValue(pictureCharacterString);
            data.PictureValidationResult = new PictureValidator.Result(
                new[] { new Character(SC.NINE, length) },
                null,
                PictureCategory.Numeric,
                length,
                length,
                false,
                0,
                length);
            data.DataType = DataType.Numeric;
            var node = new Nodes.DataDescription(data);
            node.ParentTypeDefinition = parentTypeDef;
            return node;
        }

        public static readonly TypeDefinition BooleanType = CreateBuiltIn(Boolean);
        public static readonly TypeDefinition DateType = CreateBuiltIn(Date);
        public static readonly TypeDefinition CurrencyType = CreateBuiltIn(Currency);
        public static readonly TypeDefinition StringType = CreateBuiltIn(String);

        public static readonly TypeDefinition[] BuiltInCustomTypeDefinitions = { BooleanType, DateType, CurrencyType, StringType };
        // [/TYPECOBOL]
    }
    public enum RestrictionLevel
    {
        WEAK = 0,
        STRICT = 1,
        STRONG = 2
    }
}
