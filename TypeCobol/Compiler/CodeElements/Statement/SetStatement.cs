using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Base class for  7 differents forms of set statement:
    /// 
    /// Format 1: SET for basic table handling
    /// Format 2: SET for adjusting indexes
    /// Format 3: SET for external switches
    /// Format 4: SET for condition-names (to true)
    /// Format 5: SET for USAGE IS POINTER data items
    /// Format 6: SET for procedure-pointer and function-pointer data items
    /// Format 7: SET for USAGE OBJECT REFERENCE data items
    /// 
    /// Format 1, 5, 6, 7 can be ambiguous
    /// </summary>
    public abstract class SetStatement : StatementElement
    {
        public SetStatement(StatementType statementType) : base(CodeElementType.SetStatement, statementType)
        { }
    }

    /// <summary>
    /// Format 1: SET for basic table handling
    /// set index-name or identifier(numeric integer item) TO index-name or identifier(numeric integer item) or positive integer
    ///         List<Expression> Receiving    //index-name or identifier(numeric integer item)
    ///         Expression Sending             //index-name or identifier(numeric integer item) or positive integer
    /// 
    /// Format 5: SET for USAGE IS POINTER data items
    ///         List<Expression> Receiving   //identifier(pointer), address of
    ///         Expression Sending            //identifier, address of, null, nulls
    /// 
    /// Format 6: SET for procedure-pointer and function-pointer data items
    /// set procedureOrFunctionPointer to procedureOrFunctionPointer | (ENTRY identifier|literal) | NULL | pointer
    ///         List<Expression> Receiving   //index-name or identifier(numeric integer item)
    ///         Expression Sending            //index-name or identifier(numeric integer item) or positive integer
    /// 
    /// Format 7: SET for USAGE OBJECT REFERENCE data items
    /// set objectReference to objectReference | NULL | SELF
    ///         List<Expression> Receiving   //index-name or identifier(numeric integer item)
    ///         Expression Sending            //index-name or identifier(numeric integer item) or positive integer
    /// </summary>
    public class SetStatementForAssignation : SetStatement
    {
        public SetStatementForAssignation() : base(StatementType.SetStatementForAssignation)
        { }

        /// <summary>
        /// index-name, identifier(numeric integer item), pointer, procedure-pointer, function-pointer, object reference id
        /// </summary>
        public List<Expression> Receiving { get; set; }
        IList<Expression> Receiving.Expressions { get { return Receiving; } }
        /// <summary>
        /// index-name, identifier, positive integer, address of, null, nulls, entry identifier|literal, object reference id, pointer,
        /// procedure-pointer, function-pointer,
        /// </summary>
        public Expression Sending { get; set; }
        Expression Sending.Expression { get { return Sending; } }

        public override string ToString()
        {
            if (Receiving == null && Sending == null)
            {
                return base.ToString();
            }
            var sb = new System.Text.StringBuilder("Set ");
            if (Receiving != null)
            {
                foreach (Expression receivingField in Receiving)
                {
                    sb.Append(' ');
                    sb.Append(receivingField);
                }
            }
            sb.Append(" TO ");
            if (Sending != null)
            {
                sb.AppendLine(Sending.ToString());
            }
            return sb.ToString();
        }

        ICollection<Identifier> IdentifierUser.Identifiers
        {
            get
            {
                var identifiers = new List<Identifier>();
                if (Sending is Identifier) identifiers.Add(Sending as Identifier);
                foreach (var expression in Receiving)
                    if (expression is Identifier)
                        identifiers.Add(expression as Identifier);
                return identifiers;
            }
        }

        /// <summary>
        /// Regarding the sending element, only one of the pair elements is not null:
        /// either we know its qualified name, or its type.
        /// </summary>
        ICollection<System.Tuple<System.Tuple<QualifiedName, DataType>, QualifiedName>> SymbolWriter.Symbols
        {
            get
            {
                var list = new List<System.Tuple<System.Tuple<QualifiedName, DataType>, QualifiedName>>();
                var sending = new System.Tuple<QualifiedName, DataType>(IdentifierUtils.GetQualifiedName(Sending), IdentifierUtils.GetDataType(Sending));
                foreach (var r in Receiving)
                {
                    var receiving = IdentifierUtils.GetQualifiedName(r);
                    list.Add(new System.Tuple<System.Tuple<QualifiedName, DataType>, QualifiedName>(sending, receiving));
                }
                return list;
            }
        }
        public bool IsUnsafe { get { return true; } }

    }

    /// <summary>
    /// Format 2: SET for adjusting indexes
    /// set index-name UP|DOWN BY identifier(numeric integer item) or positive integer
    ///         List<Identifier> ReceivingIndexs    //index-name
    ///         SyntaxBoolean UpBy
    ///         Expression Sending             //identifier(numeric integer item) or positive integer
    /// </summary>
    public class SetStatementForIndexes : SetStatement
    {
        public SetStatementForIndexes() : base(StatementType.SetStatementForIndexes)
        { }

        /// <summary>
        ///     index-name
        /// </summary>
        public List<Index> ReceivingIndexs { get; set; }

        public bool UpBy { get; set; }
        public bool DownBy { get; set; }

        /// <summary>
        ///     identifier(numeric integer item) or positive integer
        /// </summary>
        public Expression SendingField { get; set; }

        public override string ToString()
        {
            if (ReceivingIndexs == null && !UpBy && SendingField == null)
            {
                return base.ToString();
            }
            var sb = new StringBuilder("Set ");
            if (ReceivingIndexs != null)
            {
                foreach (var receivingIndex in ReceivingIndexs)
                {
                    sb.Append(' ');
                    sb.Append(receivingIndex);
                }
            }

            if (UpBy) sb.Append(" UP BY ");
            else if (DownBy) sb.Append(" DOWN BY ");
            else sb.Append(" ");
            if (SendingField != null) sb.Append(SendingField);

            sb.AppendLine(" ");
            return sb.ToString();
        }
    }

    /// <summary>
    /// Format 3: SET for external switches
    /// set externalSwitches to ON|OFF
    ///         List<SetExternalSwitch> SetExternalSwitches
    /// </summary>
    internal class SetStatementForSwitches : SetStatement
    {
        public SetStatementForSwitches() : base(StatementType.SetStatementForSwitches)
        { }

        /// <summary>
        /// 
        /// </summary>
        public List<SetExternalSwitch> SetExternalSwitches { get; set; }


        public override string ToString()
        {
            if (SetExternalSwitches == null)
            {
                return base.ToString();
            }
            var sb = new StringBuilder("SET ");
            foreach (SetExternalSwitch externalSwitch in SetExternalSwitches)
            {
                if (externalSwitch.MnemonicForEnvironmentNames != null)
                {
                    foreach (var mnemonicForEnvironmentName in externalSwitch.MnemonicForEnvironmentNames)
                    {
                        sb.Append(' ');
                        sb.Append(mnemonicForEnvironmentName);
                    }
                }
                if (externalSwitch.ToOn) sb.AppendLine(" TO ON");
                else if (externalSwitch.ToOff) sb.AppendLine(" TO OFF");
                else sb.AppendLine("");
            }
            return sb.ToString();
        }
    }

    public class SetExternalSwitch
    {
        public List<MnemonicForEnvironmentName> MnemonicForEnvironmentNames { get; set; }
        public bool ToOn { get; set; }
        public bool ToOff { get; set; }

        //TO avoid creating a new StringBuilder and as SetExternalSwitch is only used by SetStatementForSwitches
        //This CodeElement doesn't define a ToString() method
        //public override string ToString()
    }

    /// <summary>
    /// Format 4: SET for condition-names (to true)
    /// set condition-names to true
    ///          List<Identifier> ConditionIdentifiers //identifier
    /// </summary>
    internal class SetStatementForConditions : SetStatement
    {
        public SetStatementForConditions() : base(StatementType.SetStatementForConditions)
        { }

        /// <summary>
        ///     identifier (condition-name)
        /// </summary>
        public List<Identifier> ConditionIdentifiers { get; set; }


        public override string ToString()
        {
            if (ConditionIdentifiers == null)
            {
                return base.ToString();
            }
            var sb = new StringBuilder(base.ToString());
            foreach (Identifier conditionIdentifier in ConditionIdentifiers)
            {
                sb.Append(' ');
                sb.Append(conditionIdentifier);
            }
            sb.AppendLine(" TO TRUE");
            return sb.ToString();
        }
    }
}
