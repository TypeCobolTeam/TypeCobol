using System;
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
        }

        public StorageAreaKind Kind { get; protected set;  }

        /// <summary>
        /// True if this storage area is written to by the program
        /// </summary>
        public bool IsReceivingField { get; set; }

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
        IntrinsicFunctionCallResult // allocated on reference
    }

    /// <summary>
    /// Storage area for a data symbol or condition symbol defined in the program.
    /// Also used for the storage area allocated by the compiler for the implicitely 
    /// defined special registers (see list in a comment just below).
    /// </summary>
    public class DataOrConditionStorageArea : StorageArea
    {
        public DataOrConditionStorageArea(SymbolReference symbolReference) :
            base(StorageAreaKind.DataOrCondition)
        {
            SymbolReference = symbolReference;
        }

        public DataOrConditionStorageArea(SymbolReference subscriptedSymbolReference, SubscriptExpression[] subscripts) :
            base(StorageAreaKind.DataOrCondition)
        {
            SymbolReference = subscriptedSymbolReference;
            Subscripts = subscripts;
        }

        public SymbolReference SymbolReference { get; private set; }

        public SubscriptExpression[] Subscripts { get; private set; }

        /// <summary>
        /// Ambiguities in the grammar in the first phase of parsing
        /// </summary>
        public SymbolType AlternativeSymbolType
        {
            set
            {
                if(value == SymbolType.IndexName)
                {
                    Kind = StorageAreaKind.DataOrConditionOrIndex;
                }
                else
                {
                    Kind = StorageAreaKind.DataOrConditionOrAlternativeSymbolReference;
                }
                alternativeSymbolType = value;
            }
            get { return alternativeSymbolType; }
        }
        private SymbolType alternativeSymbolType;
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

    /// <summary>
    /// Subscript used to reference a specific table element
    /// </summary>
    public class SubscriptExpression
    {
        public SubscriptExpression(ArithmeticExpression numericExpression)
        {
            NumericExpression = numericExpression;
        }

        public SubscriptExpression(Token allToken)
        {
            ALL = new SyntaxProperty<bool>(true, allToken);
        }

        public ArithmeticExpression NumericExpression { get; private set; }

        public SyntaxProperty<bool> ALL { get; private set; }
    }

    /// <summary>
    /// Storage area for an index
    /// </summary>
    public class IndexStorageArea : StorageArea
    {
        public IndexStorageArea(SymbolReference indexNameReference) :
            base(StorageAreaKind.Index)
        {
            SymbolReference = indexNameReference;
        }

        public SymbolReference SymbolReference { get; private set; }
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
    public class StorageAreaPropertySpecialRegister : StorageArea
    {
        public StorageAreaPropertySpecialRegister(Token specialRegisterName, StorageArea storageAreaReference) :
            base(StorageAreaKind.StorageAreaPropertySpecialRegister)
        {
            SpecialRegisterName = specialRegisterName;
            StorageAreaReference = storageAreaReference;
        }

        public Token SpecialRegisterName { get; private set; }

        public StorageArea StorageAreaReference { get; private set; }
    }

    /// <summary>
    /// Specific storage area allocated by the compiler to hold
    /// a property describing another storage area
    /// </summary>
    public class FilePropertySpecialRegister : StorageArea
    {
        public FilePropertySpecialRegister(Token specialRegisterName, SymbolReference fileNameReference) :
            base(StorageAreaKind.FilePropertySpecialRegister)
        {
            SpecialRegisterName = specialRegisterName;
            SymbolReference = fileNameReference;
        }

        public Token SpecialRegisterName { get; private set; }

        public SymbolReference SymbolReference { get; private set; }
    }

    /// <summary>
    /// Call to a Cobol intrinsic function.
    ///  AND
    /// Storage area allocated by the compiler to hold the result of the call.
    /// </summary>
    public class IntrinsicFunctionCallResultStorageArea : StorageArea
    {
        public IntrinsicFunctionCallResultStorageArea(ExternalName intrinsicFunctionName, Expression[] arguments) :
            base(StorageAreaKind.IntrinsicFunctionCallResult)
        {
            IntrinsicFunctionName = intrinsicFunctionName;
            Arguments = arguments;
        }

        public ExternalName IntrinsicFunctionName { get; private set; }

        public Expression[] Arguments { get; private set; }
    }
}
