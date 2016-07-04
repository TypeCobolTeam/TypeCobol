using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Compile-time value given by a syntax literal
    ///  OR
    /// Runtime value read in a storage area
    /// </summary>
    public abstract class VariableBase
    {
        public VariableBase(StorageDataType dataType, StorageArea storageArea)
        {
            DataType = dataType;
            StorageArea = storageArea;
        }

        public StorageDataType DataType { get; private set; }

        public StorageArea StorageArea { get; private set; }
    }

    public enum StorageDataType
    {
        Any,
        Integer,
        Numeric,
        Character,
        Alphanumeric,
        ProgramName,
        ProgramNameOrProgramEntry,
        ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointer,
        ClassNameOrObjectReference,
        MethodName
    }

    public class IntegerVariable : VariableBase
    {
        public IntegerVariable(StorageArea storageArea) : 
            base(StorageDataType.Integer, storageArea)
        { }

        public IntegerVariable(IntegerValue value) :
            base(StorageDataType.Integer, null)
        {
            Value = value;
        }

        public IntegerValue Value { get; private set; }
    }

    public class NumericVariable : VariableBase
    {
        public NumericVariable(StorageArea storageArea) :
            base(StorageDataType.Numeric, storageArea)
        { }

        public NumericVariable(NumericValue value) :
            base(StorageDataType.Numeric, null)
        {
            Value = value;
        }

        public NumericValue Value { get; private set; }
    }

    public class CharacterVariable : VariableBase
    {
        public CharacterVariable(StorageArea storageArea) :
            base(StorageDataType.Character, storageArea)
        { }

        public CharacterVariable(CharacterValue value) :
            base(StorageDataType.Character, null)
        {
            Value = value;
        }

        public CharacterValue Value { get; private set; }
    }

    public class AlphanumericVariable : VariableBase
    {
        public AlphanumericVariable(StorageArea storageArea) :
            base(StorageDataType.Alphanumeric, storageArea)
        { }

        public AlphanumericVariable(AlphanumericValue value) :
            base(StorageDataType.Alphanumeric, null)
        {
            Value = value;
        }

        public AlphanumericVariable(RepeatedCharacterValue repeadtedValue) :
            base(StorageDataType.Alphanumeric, null)
        {
            RepeatedCharacterValue = repeadtedValue;
        }

        public AlphanumericValue Value { get; private set; }

        public RepeatedCharacterValue RepeatedCharacterValue { get; private set; }
    }

    public class SymbolReferenceVariable : VariableBase
    {
        public SymbolReferenceVariable(StorageDataType symbolType, StorageArea storageArea) :
            base(symbolType, storageArea)
        { }

        public SymbolReferenceVariable(StorageDataType symbolType, SymbolReference symbolReference) :
            base(symbolType, null)
        {
            SymbolReference = symbolReference;
        }

        public SymbolReference SymbolReference { get; private set; }
    }

    public class Variable : VariableBase
    {
        public Variable(StorageArea storageArea) :
            base(StorageDataType.Any, storageArea)
        { }

        public Variable(NumericValue value) :
            base(StorageDataType.Any, null)
        {
           NumericValue = value;
        }

        public Variable(AlphanumericValue value) :
            base(StorageDataType.Any, null)
        {
            AlphanumericValue = value;
        }

        public Variable(RepeatedCharacterValue repeatedValue) :
            base(StorageDataType.Any, null)
        {
            RepeatedCharacterValue = repeatedValue;
        }

        public Variable(SymbolReference symbolReference) :
            base(StorageDataType.Any, null)
        {
            SymbolReference = symbolReference;
        }

        protected Variable() :
            base(StorageDataType.Any, null)
        { }

        public NumericValue NumericValue { get; private set; }

        public AlphanumericValue AlphanumericValue { get; private set; }

        public RepeatedCharacterValue RepeatedCharacterValue { get; private set; }

        public SymbolReference SymbolReference { get; private set; }
    }

    public class VariableOrExpression : Variable
    {
        public VariableOrExpression(StorageArea storageArea) :
            base(storageArea)
        { }

        public VariableOrExpression(NumericValue value) :
            base(value)
        { }

        public VariableOrExpression(AlphanumericValue value) :
            base(value)
        { }

        public VariableOrExpression(RepeatedCharacterValue repeatedValue) :
            base(repeatedValue)
        { }

        public VariableOrExpression(SymbolReference symbolReference) :
            base(symbolReference)
        { }

        public VariableOrExpression(ArithmeticExpression arithmeticExpression) 
        {
            ArithmeticExpression = arithmeticExpression;
        }

        public ArithmeticExpression ArithmeticExpression { get; private set; }
    }

    public class BooleanValueOrExpression
    {
        public BooleanValueOrExpression(BooleanValue booleanValue)
        {
            BooleanValue = booleanValue;
        }

        public BooleanValueOrExpression(ConditionalExpression conditionalExpression)
        {
            Expression = conditionalExpression;
        }

        public BooleanValue BooleanValue { get; private set; }

        public ConditionalExpression Expression { get; private set; }
    }    

    public class ReceivingStorageArea : VariableBase
    {
        public ReceivingStorageArea(StorageDataType dataType, StorageArea storageArea) : 
            base(dataType, storageArea)
        {
            storageArea.IsReadFrom = false;
            storageArea.IsWrittenTo = true;
        }

        public DataSourceType DataSourceType { get; set; }

        public StorageArea[] SendingStorageAreas { get; set; }
    }
    
    public enum DataSourceType
    {
        ComputeExpression,
        MoveFromStorageArea,
        ReadFromDatabase,
        ReadFromFile,
        ReadFromInputDevice,
        ReadFromSystemCall,
        ReceiveFromCalledProgram,
        ReceiveFromCallingProgram
    }
}
