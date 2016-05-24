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
        public VariableBase(VariableDataType dataType, StorageArea storageArea)
        {
            DataType = dataType;
            StorageArea = storageArea;
        }

        public VariableDataType DataType { get; private set; }

        public StorageArea StorageArea { get; private set; }
    }

    public enum VariableDataType
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
            base(VariableDataType.Integer, storageArea)
        { }

        public IntegerVariable(IntegerValue value) :
            base(VariableDataType.Integer, null)
        {
            Value = value;
        }

        public IntegerValue Value { get; private set; }
    }

    public class NumericVariable : VariableBase
    {
        public NumericVariable(StorageArea storageArea) :
            base(VariableDataType.Numeric, storageArea)
        { }

        public NumericVariable(NumericValue value) :
            base(VariableDataType.Numeric, null)
        {
            Value = value;
        }

        public NumericValue Value { get; private set; }
    }

    public class CharacterVariable : VariableBase
    {
        public CharacterVariable(StorageArea storageArea) :
            base(VariableDataType.Character, storageArea)
        { }

        public CharacterVariable(CharacterValue value) :
            base(VariableDataType.Character, null)
        {
            Value = value;
        }

        public CharacterValue Value { get; private set; }
    }

    public class AlphanumericVariable : VariableBase
    {
        public AlphanumericVariable(StorageArea storageArea) :
            base(VariableDataType.Alphanumeric, storageArea)
        { }

        public AlphanumericVariable(AlphanumericValue value) :
            base(VariableDataType.Alphanumeric, null)
        {
            Value = value;
        }

        public AlphanumericVariable(RepeatedCharacterValue repeadtedValue) :
            base(VariableDataType.Alphanumeric, null)
        {
            RepeatedCharacterValue = repeadtedValue;
        }

        public AlphanumericValue Value { get; private set; }

        public RepeatedCharacterValue RepeatedCharacterValue { get; private set; }
    }

    public class SymbolReferenceVariable : VariableBase
    {
        public SymbolReferenceVariable(VariableDataType symbolType, StorageArea storageArea) :
            base(symbolType, storageArea)
        { }

        public SymbolReferenceVariable(VariableDataType symbolType, SymbolReference symbolReference) :
            base(symbolType, null)
        {
            SymbolReference = symbolReference;
        }

        public SymbolReference SymbolReference { get; private set; }
    }

    public class Variable : VariableBase
    {
        public Variable(StorageArea storageArea) :
            base(VariableDataType.Any, storageArea)
        { }

        public Variable(NumericValue value) :
            base(VariableDataType.Any, null)
        {
           NumericValue = value;
        }

        public Variable(AlphanumericValue value) :
            base(VariableDataType.Any, null)
        {
            AlphanumericValue = value;
        }

        public Variable(RepeatedCharacterValue repeadtedValue) :
            base(VariableDataType.Any, null)
        {
            RepeatedCharacterValue = repeadtedValue;
        }

        public Variable(SymbolReference symbolReference) :
            base(VariableDataType.Any, null)
        {
            SymbolReference = symbolReference;
        }

        public NumericValue NumericValue { get; private set; }

        public AlphanumericValue AlphanumericValue { get; private set; }

        public RepeatedCharacterValue RepeatedCharacterValue { get; private set; }

        public SymbolReference SymbolReference { get; private set; }
    }
}
