using System.Collections.Generic;
using JetBrains.Annotations;

namespace TypeCobol.Compiler.CodeElements {

    /// <summary>
    /// Compile-time value given by a syntax literal
    ///  OR
    /// Runtime value read in a storage area
    /// 
    /// 
    /// 
    /// VariableBase
    ///    Variable
    ///       VariableOrExpression
    ///    ReceivingStorageArea
    ///    SymbolReferenceVariable
    ///    IntegerVariable
    ///    NumericVariable
    ///    CharacterVariable
    ///    AlphanumericVariable
    /// </summary>
    public abstract class VariableBase : IVisitable {
        protected VariableBase(StorageDataType dataType, [CanBeNull] StorageArea storageArea) {
		    DataType = dataType;
		    StorageArea = storageArea;
	    }

	    public StorageDataType DataType { get; private set; }

        [CanBeNull]
        public StorageArea StorageArea { get; private set; }

        /// <summary>
        /// Checks which kind of data is stored in the variable :
        /// - if it is a literal value : returns null
        /// - if it is a complex expression : returns null
        /// - if it is a simple symbol reference or storage area : returns the main symbol reference
        /// </summary>
        public virtual SymbolReference MainSymbolReference { get { return StorageArea != null ? StorageArea.SymbolReference : null; } }

	    public override string ToString() {
		    if (StorageArea != null) return StorageArea.ToString();
		    return base.ToString();
	    }

        public virtual bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, StorageArea)
                && this.ContinueVisitToChildren(astVisitor, MainSymbolReference);
        }
    }

    public enum StorageDataType {
	    Any,
        Condition,
	    Integer,
	    Numeric,
	    Character,
	    Alphanumeric,
	    ProgramName,
	    ProgramNameOrProgramEntry,
	    ProgramNameOrProgramEntryOrProcedurePointerOrFunctionPointer,
	    ProcedurePointerOrFunctionPointerOrTCFunctionName,
	    ClassNameOrObjectReference,
	    MethodName
    }

    public class IntegerVariable: VariableBase {
	    public IntegerVariable([NotNull] StorageArea storageArea): base(StorageDataType.Integer, storageArea) { }
	    public IntegerVariable([NotNull] IntegerValue value): base(StorageDataType.Integer, null) { Value = value; }

	    public IntegerValue Value { get; private set; }

	    public override string ToString() {
		    if (Value != null) return Value.ToString();
		    return base.ToString();
	    }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, Value);
        }
    }

    public class NumericVariable: VariableBase {
	    public NumericVariable(StorageArea storageArea): base(StorageDataType.Numeric, storageArea) { }
	    public NumericVariable(NumericValue value): base(StorageDataType.Numeric, null) { Value = value; }

	    public NumericValue Value { get; private set; }

        public override string ToString() {
		    if (Value != null) return Value.ToString();
		    return base.ToString();
	    }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, Value);
        }
    }

    public class CharacterVariable: VariableBase {
	    public CharacterVariable(StorageArea storageArea): base(StorageDataType.Character, storageArea) { }
	    public CharacterVariable(CharacterValue value): base(StorageDataType.Character, null) { Value = value; }

	    public CharacterValue Value { get; private set; }

        public override string ToString() {
		    if (Value != null) return Value.ToString();
		    return base.ToString();
	    }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, Value);
        }
    }

    public class AlphanumericVariable: VariableBase {
	    public AlphanumericVariable(StorageArea storageArea): base(StorageDataType.Alphanumeric, storageArea) { }
	    public AlphanumericVariable(AlphanumericValue value): base(StorageDataType.Alphanumeric, null) { Value = value; }
	    public AlphanumericVariable(RepeatedCharacterValue value): base(StorageDataType.Alphanumeric, null) {
		    RepeatedCharacterValue = value;
	    }

	    public AlphanumericValue Value { get; private set; }
	    public RepeatedCharacterValue RepeatedCharacterValue { get; private set; }

        public override string ToString() {
		    if (RepeatedCharacterValue != null) return RepeatedCharacterValue.Value;
		    //the previous line will always return an exception. should be:
		    //return RepeatedCharacterValue.GetValueInContext(???);
		    if (Value != null) return Value.ToString();
		    return base.ToString();
	    }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, Value, RepeatedCharacterValue);
        }
    }

    public class SymbolReferenceVariable : VariableBase {
	    public SymbolReferenceVariable(StorageDataType symbolType, StorageArea storageArea): base(symbolType, storageArea) { }
	    public SymbolReferenceVariable(StorageDataType symbolType, SymbolReference symbolReference): base(symbolType, null) {
		    SymbolReference = symbolReference;
	    }

	    public SymbolReference SymbolReference { get; private set; }

        public override SymbolReference MainSymbolReference { get { return SymbolReference ?? base.MainSymbolReference; } }

        public override string ToString() { return MainSymbolReference.ToString(); }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, SymbolReference)
                && this.ContinueVisitToChildren(astVisitor, MainSymbolReference);
        }
    }



    public class Variable: VariableBase {

	    protected Variable(): base(StorageDataType.Any, null) { }

	    public Variable(StorageArea reference): base(StorageDataType.Any, reference) { }

	    public Variable(NumericValue value): base(StorageDataType.Any, null) { NumericValue = value; }
	    public Variable(AlphanumericValue value): base(StorageDataType.Any, null) { AlphanumericValue = value; }
	    public Variable(RepeatedCharacterValue value): base(StorageDataType.Any, null) { RepeatedCharacterValue = value; }
	    public Variable(SymbolReference reference): base(StorageDataType.Any, null) { SymbolReference = reference; }

	    public NumericValue NumericValue { get; private set; }
	    public AlphanumericValue AlphanumericValue { get; private set; }
	    public RepeatedCharacterValue RepeatedCharacterValue { get; private set; }
	    public SymbolReference SymbolReference { get; private set; }

	    public bool IsLiteral { get { return NumericValue != null || AlphanumericValue != null; } }

        public override SymbolReference MainSymbolReference { get { return SymbolReference ?? base.MainSymbolReference; } }

        public override string ToString()
        {
            return ToString(false);
        }
        public new string ToString(bool bUseToString) {
		    if (NumericValue != null) return NumericValue.Value.ToString();
		    try {
                if (SymbolReference != null)
                {
                    return bUseToString ? SymbolReference.ToString() : SymbolReference.Name;
                }
                if (StorageArea != null)
                {
                    return bUseToString ? StorageArea.SymbolReference.ToString() : StorageArea.SymbolReference.Name;
                }
			    //these should be: return XXXValue.GetValueInContext(???);
			    if (AlphanumericValue != null) return AlphanumericValue.Token.SourceText;
			    if (RepeatedCharacterValue != null) return RepeatedCharacterValue.Token.SourceText;
		    } catch(System.InvalidOperationException) { }
		    return base.ToString();
	    }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, NumericValue,
                       AlphanumericValue, RepeatedCharacterValue, SymbolReference);
        }
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

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, ArithmeticExpression);
        }
    }

    public class BooleanValueOrExpression : IVisitable
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

        public bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, BooleanValue, Expression);
        }
    }    

    public class ReceivingStorageArea: VariableBase {
	    public ReceivingStorageArea(StorageDataType dataType, [NotNull] StorageArea storageArea): base(dataType, storageArea) {
		    storageArea.IsReadFrom = false;
		    storageArea.IsWrittenTo = true;
	    }

	    public DataSourceType DataSourceType { get; set; }
	    public StorageArea[] SendingStorageAreas { get; set; }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>) SendingStorageAreas);
        }
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
