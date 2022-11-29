using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using JetBrains.Annotations;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Base class for a storage area in memory where the program can read or write data.
    /// Used for both SQL and Cobol.
    /// </summary>
    public abstract class BaseStorageArea
    {
        protected BaseStorageArea(StorageAreaKind kind)
        {
            Kind = kind;
            IsReadFrom = true;
            IsWrittenTo = false;
        }

        public StorageAreaKind Kind { get; protected set; }

        [CanBeNull]
        public SymbolReference SymbolReference { get; set; }

        /// <summary>
        /// True if this storage area is read from by the program
        /// </summary>
        public bool IsReadFrom { get; set; }

        /// <summary>
        /// True if this storage area is written to by the program
        /// </summary>
        public bool IsWrittenTo { get; set; }

        public override string ToString()
        {
            if (SymbolReference != null) return SymbolReference.ToString();
            return base.ToString();
        }
    }

    /// <summary>
    /// Represents a storage area in a Cobol program.
    /// Most often represented by the "identifier" rule in the Cobol grammar.
    /// </summary>
    public abstract class StorageArea : BaseStorageArea, IVisitable
    {
        protected StorageArea(StorageAreaKind kind)
            : base(kind)
        {

        }

        /// <summary>
        /// Optional reference modification : enables to delimit only a subset of the original storage area 
        /// </summary>
        public void ApplyReferenceModifier(ReferenceModifier referenceModifier)
        {
            ReferenceModifier = referenceModifier;
        }

        /// <summary>
        /// Reference modification defines a data item by specifying a leftmost character and
        /// optional length for the data item.
        /// </summary>
        public ReferenceModifier ReferenceModifier { get; private set; }

        public virtual bool NeedDeclaration => true;

        public virtual StorageArea GetStorageAreaThatNeedDeclaration => this;

        public virtual bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this) &&
                   this.ContinueVisitToChildren(astVisitor, SymbolReference, ReferenceModifier);
        }
    }

    /// <summary>
    /// Reference modification defines a data item by specifying a leftmost character and
    /// optional length for the data item.
    /// </summary>
    public class ReferenceModifier : IVisitable
    {
        public ReferenceModifier(ArithmeticExpression leftmostCharacterPosition, ArithmeticExpression length)
        {
            LeftmostCharacterPosition = leftmostCharacterPosition;
            Length = length;
        }

        public ArithmeticExpression LeftmostCharacterPosition { get; private set; }

        public ArithmeticExpression Length { get; private set; }

        public virtual bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return astVisitor.Visit(this) && this.ContinueVisitToChildren(astVisitor, LeftmostCharacterPosition, Length);
        }
    }

    public enum StorageAreaKind
    {
        DataOrCondition, // includes implicitely defined special registers
        DataOrConditionOrIndex,
        DataOrConditionOrAlternativeSymbolReference,
        Index,
        StorageAreaPropertySpecialRegister, // allocated on reference
        FilePropertySpecialRegister, // one per file description with LINAGE clause
        FunctionCallResult // allocated on reference
    }

	/// <summary>
	/// Storage area for a data symbol or condition symbol defined in the program.
	/// Also used for the storage area allocated by the compiler for the implicitely 
	/// defined special registers (see list in a comment just below).
	/// </summary>
	public class DataOrConditionStorageArea: StorageArea {
        public DataOrConditionStorageArea(SymbolReference symbolReference, bool isPartOfFunctionArgument)
            : this(symbolReference, new SubscriptExpression[0], isPartOfFunctionArgument)
        {

        }

		public DataOrConditionStorageArea(SymbolReference symbolReference, SubscriptExpression[] subscripts, bool isPartOfFunctionArgument)
            : base(StorageAreaKind.DataOrCondition)
        {
			SymbolReference = symbolReference;
			Subscripts = subscripts;
            IsPartOfFunctionArgument = isPartOfFunctionArgument;
        }

        public bool IsPartOfFunctionArgument { get; }

        [NotNull]
		public SubscriptExpression[] Subscripts { get; }


        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, Subscripts);
        }

	    public override string ToString() {
			return ToString(false);
		}

        /// <summary>
        /// For indexes, stores the computed hash of the corresponding IndexDefinition
        /// Used by Codegen only.
        /// </summary>
        public string Hash { get; set; }

        public string ToString(bool onlySubscript)
        {
            var str = new System.Text.StringBuilder();
            if (SymbolReference != null)
            {
                if (!onlySubscript)
                {
                    if (Hash != null)
                    {
                        var symbolReference = SymbolReference.IsQualifiedReference
                            ? ((QualifiedSymbolReference) SymbolReference).First
                            : SymbolReference;
                        str.Append(Hash + symbolReference.Name);
                    }
                    else
                    {
                        str.Append(SymbolReference.Name);
                    }
                }
                if (Subscripts.Length > 0)
                {
                    str.Append('(');
                    foreach (var subscript in Subscripts)
                    {
                        str.Append(subscript);
                        if (Subscripts.LastOrDefault() != subscript)
                            str.Append(", ");
                    }
                    str.Append(')');
                }
                if (ReferenceModifier != null)
                {
                    str.Append("(")
                        .Append(GetExpressionToAppend(ReferenceModifier.LeftmostCharacterPosition))
                        .Append(":")
                        .Append(GetExpressionToAppend(ReferenceModifier.Length))
                        .Append(')');
                }
            }
            return str.ToString();
        }

	    private string GetExpressionToAppend(ArithmeticExpression expression)
	    {
	        if (expression == null)
	            return string.Empty;
	        var expressionToAppend = expression.ToString();
	        if (expression.NodeType == ExpressionNodeType.NumericVariable &&
	            (expression as NumericVariableOperand)?.NumericVariable.MainSymbolReference != null)
	        {
	            expressionToAppend = ((NumericVariableOperand) expression).NumericVariable
	                .MainSymbolReference.ToString();
	        }

	        return expressionToAppend;
	    }
    }

    /// <summary>
    /// Implicitely defined special registers :
    ///
    ///        statements operations
    ///        - XML parsing
    ///            XML-*
    ///        - INSPECT, UNSTRING
    ///            TALLY
    ///        - SORT MERGE
    ///            SORT-*
    ///        - debugging declarative procedure
    ///            DEBUG-ITEM 
    ///
    ///        environment communication
    ///            RETURN-CODE
    ///            JNIENVPTR
    ///
    ///        compiler metadata
    ///            WHEN-COMPILED
    ///
    ///        constants
    ///            SHIFT-OUT
    ///            SHIFT-IN       
    /// </summary>
    public class IntrinsicStorageArea : StorageArea
    {
        public IntrinsicStorageArea(SymbolReference symbolReference) : base(StorageAreaKind.DataOrCondition) {
            this.SymbolReference = symbolReference;
        }

        public override bool NeedDeclaration {
            get { return false; }
        }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this);
        }
    }

    /// <summary>Subscript used to reference a specific table element</summary>
    public class SubscriptExpression : IVisitable {
	    public SubscriptExpression(ArithmeticExpression numericExpression) {
		    NumericExpression = numericExpression;
	    }

	    public SubscriptExpression(Token allToken) {
		    ALL = new SyntaxProperty<bool>(true, allToken);
	    }

	    public ArithmeticExpression NumericExpression { get; private set; }
	    public SyntaxProperty<bool> ALL { get; private set; }


        public override string ToString() {
		    if (NumericExpression != null) return NumericExpression.ToString();
		    if (ALL != null) return ALL.Token.SourceText;
		    return base.ToString();
	    }

        public virtual bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, ALL, NumericExpression);
        }
    }

	/// <summary>Storage area for an index</summary>
	public class IndexStorageArea : StorageArea {
        public IndexStorageArea(SymbolReference indexNameReference): base(StorageAreaKind.Index) {
			SymbolReference = indexNameReference;
        }

	    public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
	        return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this);
	    }
    }
    
    /* Special registers holding properties of other storage areas or symbols

    storage area properties
        ADDRESS OF
        LENGTH OF 

    file properties
        LINAGE-COUNTER
    */

    /// <summary>
    /// Specific storage area allocated by the compiler to hold
    /// a property describing another storage area
    /// </summary>
	public class StorageAreaPropertySpecialRegister: StorageArea {
        public StorageAreaPropertySpecialRegister(Token specialRegisterName, StorageArea storageAreaReference)
				: base(StorageAreaKind.StorageAreaPropertySpecialRegister) {
			SpecialRegisterName = specialRegisterName;
			OtherStorageAreaReference = storageAreaReference;
            
            // This is both a storage area definition and a reference to the same storage area
            var storageAreaName = storageAreaReference != null ? storageAreaReference.ToString() : "null";
            DataDescriptionEntry = new SpecialRegisterDescriptionEntry(specialRegisterName, storageAreaName);
            SymbolReference = new SymbolReference(DataDescriptionEntry.DataName);
        }

		public Token SpecialRegisterName { get; private set; }

		public StorageArea OtherStorageAreaReference { get; private set; }

        /// <summary>
        /// A mention to this kind of special register in the code is both a storage area definition
		/// and a reference to the same storage area
        /// </summary>
        public SpecialRegisterDescriptionEntry DataDescriptionEntry { get; private set; }

        public override bool NeedDeclaration {
            get { return OtherStorageAreaReference != null && OtherStorageAreaReference.NeedDeclaration; }
        }

        public override StorageArea GetStorageAreaThatNeedDeclaration
        {
            get { return OtherStorageAreaReference?.GetStorageAreaThatNeedDeclaration ?? this; }
        }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, SpecialRegisterName, OtherStorageAreaReference, DataDescriptionEntry);
        }

        public override string ToString() {
			var str = new System.Text.StringBuilder();
			if (SpecialRegisterName != null) str.Append(SpecialRegisterName.TokenType).Append('(');
			if (OtherStorageAreaReference != null) str.Append(OtherStorageAreaReference);
			if (SpecialRegisterName != null) str.Append(')');
			if (str.Length > 0) return str.ToString();
			return base.ToString();
		}
    }

	/// <summary>
	/// Specific storage area allocated by the compiler to hold
	/// a property describing another storage area
	/// </summary>
	public class FilePropertySpecialRegister : StorageArea {
        public FilePropertySpecialRegister(Token specialRegisterName, [NotNull] SymbolReference fileNameReference)
				: base(StorageAreaKind.FilePropertySpecialRegister) {
			SpecialRegisterName = specialRegisterName;
			FileNameReference = fileNameReference;

            // This is both a storage area definition and a reference to the same storage area
            DataDescriptionEntry = new SpecialRegisterDescriptionEntry(specialRegisterName, fileNameReference.ToString());
            SymbolReference = new SymbolReference(DataDescriptionEntry.DataName);

        }

		public Token SpecialRegisterName { get; private set; }

		public SymbolReference FileNameReference { get; private set; }

        /// <summary>
        /// A mention to this kind of special register in the code is both a storage area definition
		/// and a reference to the same storage area
        /// </summary>
        public SpecialRegisterDescriptionEntry DataDescriptionEntry { get; private set; }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this) 
                && this.ContinueVisitToChildren(astVisitor, SpecialRegisterName, FileNameReference, DataDescriptionEntry);
        }
    }

	/// <summary>
	/// Call to a Cobol intrinsic function OR a TypeCobol user defined function.
	///  AND
	/// Storage area allocated by the compiler to hold the result of the call.
	/// </summary>
	public class FunctionCallResult : StorageArea {

        // Static counter which will only increase during a compilation session
        private static int callSiteCounter;

        public FunctionCallResult(FunctionCall functionCall)
				: base(StorageAreaKind.FunctionCallResult) {
			FunctionCall = functionCall;

            // This is both a storage area definition and a reference to the same storage area
            int uniqueCounter = Interlocked.Increment(ref callSiteCounter);
            DataDescriptionEntry = new FunctionCallResultDescriptionEntry(functionCall, uniqueCounter);
            SymbolReference = new SymbolReference(DataDescriptionEntry.DataName);
        }

        /// <summary>
        /// Each individual function call site in the code is both a storage area definition
		/// and a reference to the same storage area
        /// </summary>
        public FunctionCallResultDescriptionEntry DataDescriptionEntry { get; private set; }
 
        /// <summary>
        /// Cobol intrinsic function call OR TypeCobol user defined function call
        /// </summary>
        public FunctionCall FunctionCall { get; private set; }

	    public override bool NeedDeclaration {
	        get { return FunctionCall.NeedDeclaration; }
	    }

	    public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this) 
                && this.ContinueVisitToChildren(astVisitor, FunctionCall, DataDescriptionEntry);
        }
    }



    /// <summary>Common properties for noth types of function calls : list of expressions as arguments</summary>
    public abstract class FunctionCall : IVisitable {
        protected FunctionCall(FunctionCallType type, CallSiteParameter[] arguments) {
		    Type = type;
            Arguments = arguments;
        }

	    public FunctionCallType Type { get; private set; }
        [CanBeNull]
	    public abstract string FunctionName { get; }
        [CanBeNull]
        public abstract string Namespace { get; }
        [CanBeNull]
        public abstract Token FunctionNameToken { get; }
	    public virtual CallSiteParameter[] Arguments { get; private set; }

        public virtual IProfile BuildProfile(Node node)
        {
            return ArgumentsProfile.Create(node, Arguments, null, null);
        }
     
        public virtual bool NeedDeclaration {
            get { return true; }
        }

        public virtual bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, FunctionNameToken)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>) Arguments);
        }


	    protected class ArgumentsProfile : IProfile
        {
            private static readonly TypeInfo _Unknown = new TypeInfo() { DataType = DataType.Unknown };
            private static readonly TypeInfo _Omitted = new TypeInfo() { DataType = DataType.Omitted };
            private static readonly TypeInfo _Numeric = new TypeInfo() { DataType = DataType.Numeric };
            private static readonly TypeInfo _Alphanumeric = new TypeInfo() { DataType = DataType.Alphanumeric };

            public static ArgumentsProfile Create(Node node, IEnumerable<CallSiteParameter> inputParameters, IEnumerable<CallSiteParameter> inoutParameters, IEnumerable<CallSiteParameter> outputParameters)
            {
                var inputs = inputParameters?.Select(Convert).ToList();
                var inouts = inoutParameters?.Select(Convert).ToList();
                var outputs = outputParameters?.Select(Convert).ToList();
                //NOTE : Returning parameter for functions is not supported yet
                return new ArgumentsProfile(inputs, inouts, outputs, null);

                TypeInfo Convert(CallSiteParameter parameter)
                {
                    if (parameter.IsOmitted)
                    {
                        return _Omitted;
                    }

                    var variable = parameter.StorageAreaOrValue;
                    if (variable != null)
                    {
                        if (variable.IsLiteral)
                        {
                            if (variable.NumericValue != null)
                            {
                                return _Numeric;
                            }
                            if (variable.AlphanumericValue != null)
                            {
                                return _Alphanumeric;
                            }
                        }
                        else if (variable.StorageArea != null)
                        {
                            var storageArea = variable.StorageArea;
                            if (storageArea.Kind == StorageAreaKind.StorageAreaPropertySpecialRegister)
                            {
                                //Special case LENGTH OF / ADDRESS OF
                                return _Numeric;
                            }
                            if (node?.GetDataDefinitionFromStorageAreaDictionary(storageArea) is DataDescription data)
                            {
                                return new TypeInfo() {DataType = data.DataType, TypeDefinition = data.TypeDefinition};
                            }
                        }
                    }

                    return _Unknown;
                }
            }

            public IList<TypeInfo> Inputs { get; }
            public IList<TypeInfo> Inouts { get; }
            public IList<TypeInfo> Outputs { get; }
            public TypeInfo Returning { get; }

            private ArgumentsProfile(IList<TypeInfo> inputs, IList<TypeInfo> inouts, IList<TypeInfo> outputs, TypeInfo returning)
            {
                Inputs = inputs ?? new List<TypeInfo>();
                Inouts = inouts ?? new List<TypeInfo>();
                Outputs = outputs ?? new List<TypeInfo>();
                Returning = returning;
            }
        }
    }

	/// <summary>Call to an intrinsic function</summary>
	public class IntrinsicFunctionCall: FunctionCall {
		public IntrinsicFunctionCall([CanBeNull] ExternalName intrinsicFunctionName, CallSiteParameter[] arguments)
			: base(FunctionCallType.IntrinsicFunctionCall, arguments) {
			IntrinsicFunctionName = intrinsicFunctionName;
		}

        [CanBeNull]
        public ExternalName IntrinsicFunctionName { get; private set; }
        public override string FunctionName { get { return IntrinsicFunctionName?.Name; } }
		public override Token FunctionNameToken { get { return IntrinsicFunctionName?.NameLiteral.Token; } }

        public override bool NeedDeclaration
        {
            get { return false; }
        }

        public override string Namespace
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this) 
                && this.ContinueVisitToChildren(astVisitor, IntrinsicFunctionName, FunctionNameToken);
        }
    }

	/// <summary>Call to a TypeCobol user defined function</summary>
	public class UserDefinedFunctionCall: FunctionCall {
		public UserDefinedFunctionCall([CanBeNull] SymbolReference functionName, CallSiteParameter[] arguments)
			: base(FunctionCallType.UserDefinedFunctionCall, arguments) {
			UserDefinedFunctionName = functionName;
		}

        [CanBeNull]
        public SymbolReference UserDefinedFunctionName { get; private set;  }
		public override string FunctionName { get { return UserDefinedFunctionName?.Name; } }
		public override Token FunctionNameToken { get { return UserDefinedFunctionName?.NameLiteral.Token; } }

        public override string Namespace { get { return (UserDefinedFunctionName as QualifiedSymbolReference) == null ? null : ((QualifiedSymbolReference)UserDefinedFunctionName).Tail.Name; } }

        public override bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this) 
                && this.ContinueVisitToChildren(astVisitor, UserDefinedFunctionName, FunctionNameToken);
        }

        
    }

	public class ProcedureCall: FunctionCall {
		public ProcedureCall(SymbolReference name, List<CallSiteParameter> inputs, List<CallSiteParameter> inouts, List<CallSiteParameter> outputs)
			: base(FunctionCallType.UserDefinedFunctionCall, null) {
			ProcedureName = name;
			
            InputParameters = inputs ?? new List<CallSiteParameter>();
            InoutParameters = inouts ?? new List<CallSiteParameter>();
            OutputParameters = outputs ?? new List<CallSiteParameter>();

		}

		public SymbolReference ProcedureName { get; private set; }
		public override string FunctionName { get { return ProcedureName.Name; } }
		public override Token FunctionNameToken { get { return ProcedureName.NameLiteral.Token; } }
        
        public List<CallSiteParameter> InputParameters { get; private set; }
        public List<CallSiteParameter> InoutParameters { get; private set; }
        public List<CallSiteParameter> OutputParameters { get; private set; }
       
		private List<CallSiteParameter> _cache;
		public override CallSiteParameter[] Arguments {
			get {
				if (_cache == null) {
					_cache = new List<CallSiteParameter>();
					_cache.AddRange(InputParameters);
					_cache.AddRange(InoutParameters);
					_cache.AddRange(OutputParameters);
				}
				return _cache.ToArray();
			}
		}

        public override string Namespace { get { return (ProcedureName as QualifiedSymbolReference) == null ? null : ((QualifiedSymbolReference) ProcedureName).Tail.Name; } }

        public override IProfile BuildProfile(Node node)
        {
            return ArgumentsProfile.Create(node, InputParameters, InoutParameters, OutputParameters);
        }
		

        public override bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return base.AcceptASTVisitor(astVisitor) && astVisitor.Visit(this) 
                && this.ContinueVisitToChildren(astVisitor, ProcedureName, FunctionNameToken)
                && this.ContinueVisitToChildren(astVisitor, InputParameters, InoutParameters, OutputParameters);
        }
    }

	public enum FunctionCallType {
		IntrinsicFunctionCall,
		UserDefinedFunctionCall
	}

	public class GroupCorrespondingImpact : IVisitable {
		public StorageArea SendingGroupItem { get; set; }
		public StorageArea ReceivingGroupItem { get; set; }
		public bool ReceivingGroupIsAlsoSending { get; set; }
	    public bool AcceptASTVisitor(IASTVisitor astVisitor) {
	        return astVisitor.Visit(this) && this.ContinueVisitToChildren(astVisitor, SendingGroupItem, ReceivingGroupItem);
	    }
	}
}