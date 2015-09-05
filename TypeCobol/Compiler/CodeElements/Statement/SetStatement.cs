using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;


namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Base class for  7 differents forms of set statement:
    /// 
    /// Format 1: SET for basic table handling
    /// set index-name or identifier(numeric integer item) TO index-name or identifier(numeric integer item) or positive integer
    ///         List<Expression> ReceivingFields    //index-name or identifier(numeric integer item)
    ///         Expression SendingField             //index-name or identifier(numeric integer item) or positive integer
    /// 
    /// Format 2: SET for adjusting indexes
    /// set index-name UP|DOWN BY identifier(numeric integer item) or positive integer
    ///         List<Identifier> ReceivingIndexs    //index-name
    ///         SyntaxBoolean UpBy
    ///         Expression SendingField             //identifier(numeric integer item) or positive integer
    /// 
    /// Format 3: SET for external switches
    /// set externalSwitches to ON|OFF
    ///         List<SetExternalSwitch> SetExternalSwitches
    /// 
    /// Format 4: SET for condition-names (to true)
    /// set condition-names to true
    ///          List<Identifier> ConditionIdentifiers //identifier
    /// 
    /// Format 5: SET for USAGE IS POINTER data items
    ///         List<Expression> ReceivingFields   //identifier(pointer), address of
    ///         Expression SendingField            //identifier, address of, null, nulls
    /// 
    /// Format 6: SET for procedure-pointer and function-pointer data items
    /// set procedureOrFunctionPointer to procedureOrFunctionPointer | (ENTRY identifier|literal) | NULL | pointer
    ///         List<Expression> ReceivingFields   //index-name or identifier(numeric integer item)
    ///         Expression SendingField            //index-name or identifier(numeric integer item) or positive integer
    /// 
    /// Format 7: SET for USAGE OBJECT REFERENCE data items
    /// set objectReference to objectReference | NULL | SELF
    ///         List<Expression> ReceivingFields   //index-name or identifier(numeric integer item)
    ///         Expression SendingField            //index-name or identifier(numeric integer item) or positive integer
    /// 
    /// 
    /// Format 1, 5, 6, 7 can be ambiguous
    /// </summary>
    public class SetStatement : CodeElement
    {
        

        public SetStatement() : base(CodeElementType.SetStatement)
        { }
    }

    
}
