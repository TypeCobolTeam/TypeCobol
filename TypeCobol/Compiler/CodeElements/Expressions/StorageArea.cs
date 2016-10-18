using System;
using System.Collections.Generic;
using System.Threading;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Represents a storage area in memory where the program can read or write data.
    /// Most often represented by the "identifier" rule in the Cobol grammar.
    /// </summary>
    public abstract class StorageArea
    {
        public StorageArea(StorageAreaKind kind)
        {
            Kind = kind;
            IsReadFrom = true;
            IsWrittenTo = false;
        }

        public StorageAreaKind Kind { get; protected set;  }

        public SymbolReference SymbolReference { get; protected set; }

        /// <summary>
        /// True if this storage area is read from by the program
        /// </summary>
        public bool IsReadFrom { get; set; }

        /// <summary>
        /// True if this storage area is written to by the program
        /// </summary>
        public bool IsWrittenTo { get; set; }

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

        public override string ToString()
        {
            if (SymbolReference != null) return SymbolReference.ToString();
            return base.ToString();
        }
    }

    /// <summary>
    /// Reference modification defines a data item by specifying a leftmost character and
    /// optional length for the data item.
    /// </summary>
    public class ReferenceModifier
    {
        public ReferenceModifier(ArithmeticExpression leftmostCharacterPosition, ArithmeticExpression length)
        {
            LeftmostCharacterPosition = leftmostCharacterPosition;
            Length = length;
        }

        public ArithmeticExpression LeftmostCharacterPosition { get; private set; }

        public ArithmeticExpression Length { get; private set; }
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
		public DataOrConditionStorageArea(SymbolReference symbolReference)
				: base(StorageAreaKind.DataOrCondition) {
			SymbolReference = symbolReference;
			Subscripts = new List<SubscriptExpression>();
		}

		public DataOrConditionStorageArea(SymbolReference subscriptedSymbolReference, SubscriptExpression[] subscripts)
				: base(StorageAreaKind.DataOrCondition) {
			SymbolReference = subscriptedSymbolReference;
			Subscripts = new List<SubscriptExpression>(subscripts);
		}

		public List<SubscriptExpression> Subscripts { get; private set; }

        /// <summary>Ambiguities in the grammar in the first phase of parsing</summary>
		public SymbolType AlternativeSymbolType {
			set {
				if (value == SymbolType.IndexName) Kind = StorageAreaKind.DataOrConditionOrIndex;
                else Kind = StorageAreaKind.DataOrConditionOrAlternativeSymbolReference;
				alternativeSymbolType = value;
			}
			get { return alternativeSymbolType; }
		}
		private SymbolType alternativeSymbolType;

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			str.Append(SymbolReference.Name);
			if (Subscripts != null)
			foreach(var subscript in Subscripts)
				str.Append('(').Append(subscript.ToString()).Append(')');
			return str.ToString();
		}
	}

    /* Implicitely defined special registers :

        statements operations
        - XML parsing
            XML-*
        - INSPECT, UNSTRING
            TALLY
        - SORT MERGE
            SORT-*
        - debugging declarative procedure
            DEBUG-ITEM 

        environment communication
            RETURN-CODE
            JNIENVPTR

        compiler metadata
            WHEN-COMPILED

        constants
            SHIFT-OUT
            SHIFT-IN             
        */

    /// <summary>Subscript used to reference a specific table element</summary>
    public class SubscriptExpression {
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
		    if (ALL != null) return "ALL";
		    return base.ToString();
	    }
    }

	/// <summary>Storage area for an index</summary>
	public class IndexStorageArea : StorageArea {
		public IndexStorageArea(SymbolReference indexNameReference): base(StorageAreaKind.Index) {
			SymbolReference = indexNameReference;
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

        public override string ToString() {
			var str = new System.Text.StringBuilder();
			if (SpecialRegisterName != null) str.Append(SpecialRegisterName.TokenType).Append('(');
			if (OtherStorageAreaReference != null) str.Append(OtherStorageAreaReference.ToString());
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
		public FilePropertySpecialRegister(Token specialRegisterName, SymbolReference fileNameReference)
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
	}

    /// <summary>
    /// Common properties for noth types of function calls : list of expressions as arguments
    /// </summary>
    public abstract class FunctionCall
    {
        public FunctionCall(FunctionCallType type, VariableOrExpression[] arguments)
        {
            Type = type;
            Arguments = arguments;
        }

        public FunctionCallType Type { get; private set; }

        public abstract string FunctionName { get; }
        public abstract Token FunctionNameToken { get; }

        public VariableOrExpression[] Arguments { get; private set; }
    }

    /// <summary>
    /// Call to an intrinsic function
    /// </summary>
    public class IntrinsicFunctionCall : FunctionCall
    {
        public IntrinsicFunctionCall(ExternalName intrinsicFunctionName, VariableOrExpression[] arguments) : base(FunctionCallType.IntrinsicFunctionCall, arguments)
        {
            IntrinsicFunctionName = intrinsicFunctionName;
        }

        public ExternalName IntrinsicFunctionName { get; private set; }

        public override string FunctionName { get { return IntrinsicFunctionName.Name; } }

        public override Token FunctionNameToken { get { return IntrinsicFunctionName.NameLiteral.Token; } }
    }

    /// <summary>
    /// Call to a TypeCobol user defined function
    /// </summary>
    public class UserDefinedFunctionCall : FunctionCall
    {
        public UserDefinedFunctionCall(SymbolReference functionName, VariableOrExpression[] arguments) : base(FunctionCallType.IntrinsicFunctionCall, arguments)
        {
            UserDefinedFunctionName = functionName;
        }

        public SymbolReference UserDefinedFunctionName { get; private set;  }

        public override string FunctionName { get { return UserDefinedFunctionName.Name; } }

        public override Token FunctionNameToken { get { return UserDefinedFunctionName.NameLiteral.Token; } }
    }

    public enum FunctionCallType
    {
        IntrinsicFunctionCall,
        UserDefinedFunctionCall
    }

    public class GroupCorrespondingImpact
    {
        public StorageArea SendingGroupItem { get; set; }
        public StorageArea ReceivingGroupItem { get; set; }
        public bool ReceivingGroupIsAlsoSending { get; set; }
    }
}
