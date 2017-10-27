namespace TypeCobol.Compiler.CodeElements
{

    using JetBrains.Annotations;
    using System.Collections.Generic;
    using System.Text;
    using TypeCobol.Compiler.CodeElements.Expressions;
    using TypeCobol.Compiler.Scanner;

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
    public abstract class SetStatement : StatementElement, VariableWriter
    {
        protected SetStatement(StatementType statementType) : base(CodeElementType.SetStatement, statementType) { }

        public bool IsUnsafe { get { return false; } }

        protected IDictionary<QualifiedName, object> variables;
        public virtual IDictionary<QualifiedName, object> Variables
        {
            get { return new Dictionary<QualifiedName, object>(); }
        }
        public virtual IDictionary<QualifiedName, object> VariablesWritten
        {
            [NotNull]
            get
            {
                var written = new Dictionary<QualifiedName, object>();
                foreach (var v in Variables) if (v.Value != null) written.Add(v.Key, v.Value);
                return written;
            }
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && astVisitor.VisitVariableWriter(this);
        }
    }

    /// <summary>
    /// Format 1: SET for basic table handling
    /// set index-name or identifier(numeric integer item) TO index-name or identifier(numeric integer item) or positive integer
    ///
    /// Format 5: SET for USAGE IS POINTER data items
    /// set (address of)? identifier(pointer) TO (address of)? identifier | NULL
    ///
    /// Format 6: SET for procedure-pointer and function-pointer data items
    /// set procedureOrFunctionPointer to procedureOrFunctionPointer | (ENTRY identifier|literal) | NULL | pointer
    ///
    /// Format 7: SET for USAGE OBJECT REFERENCE data items
    /// set objectReference to objectReference | NULL | SELF
    /// </summary>
    public class SetStatementForAssignment : SetStatement
    {
        public SetStatementForAssignment() : base(StatementType.SetStatementForAssignment) { }

        /// <summary>index-name, identifier(numeric integer item), pointer, procedure-pointer, function-pointer, object reference id</summary>
        public ReceivingStorageArea[] ReceivingStorageAreas { get; set; }

        /// <summary>
        /// index-name, identifier, positive integer, address of, null, nulls, entry identifier|literal, object reference id, pointer,
        /// procedure-pointer, function-pointer,
        /// </summary>
        public SetSendingVariable SendingVariable { get; set; }

        public override string ToString()
        {
            if (ReceivingStorageAreas == null && SendingVariable == null) return base.ToString();
            var str = new StringBuilder("SET ");
            foreach (var receivingField in ReceivingStorageAreas)
            {
                str.Append(' ');
                str.Append(receivingField);
            }
            if (ReceivingStorageAreas.Length < 1) str.Append('?');
            str.Append(" TO ");
            if (SendingVariable != null) str.AppendLine(SendingVariable.ToString());
            else str.Append('?');
            return str.ToString();
        }



        public override IDictionary<QualifiedName, object> Variables
        {
            [NotNull]
            get
            {
                if (variables != null) return variables;
                variables = new Dictionary<QualifiedName, object>();

                var sending = SendingItem as QualifiedName;
                if (sending != null) variables.Add(sending, null);

                if (ReceivingStorageAreas != null)
                    foreach (var item in ReceivingStorageAreas)
                    {
                        var name = new URI(item.StorageArea.SymbolReference.Name);
                        if (variables.ContainsKey(name))
                            if (item.StorageArea is DataOrConditionStorageArea) continue; // same variable with (presumably) different subscript
                            else throw new System.ArgumentException(name + " already written, but not subscripted?");
                        else variables.Add(name, SendingItem);
                    }

                return variables;
            }
        }

        private object SendingItem
        {
            [CanBeNull]
            get
            {
                if (SendingVariable == null) return null;
                return SendingVariable.Value;
            }
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, SendingVariable)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>)ReceivingStorageAreas);
        }
    }

    /// <summary>Sending field for SET statement for assignation</summary>
    public class SetSendingVariable : IVisitable
    {
        /// <summary>integerVariableOrIndex1 - identifier can also be an index name	(Format 1 + 5)</summary>
        public IntegerVariable IntegerVariableOrIndex { get; set; }
        /// <summary>nullPointerValue - pointer data item (Format 5 + 6 + 7)</summary>
        public NullPointerValue NullPointerValue { get; set; }
        /// <summary>procedure pointer, function pointer or a pointer data item (Format 6 (+NULL|NULLS))</summary>
        public SymbolReferenceVariable ProgramNameOrProgramEntryVariable { get; set; }

        /// <summary>object reference id (Format 7 (+NULL))</summary>
        public Token SelfObjectIdentifier { get; set; }

        public override string ToString()
        {
            if (IntegerVariableOrIndex != null) return IntegerVariableOrIndex.ToString();
            if (NullPointerValue != null) return NullPointerValue.ToString();
            if (ProgramNameOrProgramEntryVariable != null) return ProgramNameOrProgramEntryVariable.ToString();
            if (SelfObjectIdentifier != null) return SelfObjectIdentifier.ToString();
            return string.Empty;
        }

        public bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, IntegerVariableOrIndex,
                                                               NullPointerValue,
                                                               ProgramNameOrProgramEntryVariable,
                                                               SelfObjectIdentifier);
        }

        public object Value
        {
            get
            {
                if (IntegerVariableOrIndex != null)
                {
                    if (IntegerVariableOrIndex.Value != null) return IntegerVariableOrIndex.Value.Value;

                    if (IntegerVariableOrIndex.StorageArea != null &&
                             IntegerVariableOrIndex.StorageArea.SymbolReference != null)
                        return IntegerVariableOrIndex.StorageArea.SymbolReference.Name;
                    return null;
                }
                if (NullPointerValue != null) return NullPointerValue.ToString();
                if (ProgramNameOrProgramEntryVariable != null && ProgramNameOrProgramEntryVariable.StorageArea != null)
                {
                    var program = ProgramNameOrProgramEntryVariable.StorageArea.SymbolReference;
                    if (program != null) return new URI(program.Name);
                }
                if (SelfObjectIdentifier != null) return SelfObjectIdentifier.ToString();
                return null;
            }
        }
    }

    /// <summary>
    /// Format 2: SET for adjusting indexes
    /// set index-name UP|DOWN BY identifier(numeric integer item) or positive integer
    /// </summary>
    public class SetStatementForIndexes : SetStatement
    {
        public SetStatementForIndexes() : base(StatementType.SetStatementForIndexes) { }

        /// <summary>index-name</summary>
        public ReceivingStorageArea[] ReceivingIndexes { get; set; }

        /// <summary>UP | DOWN</summary>
        public SyntaxProperty<IndexIncrementDirection> IncrementDirection { get; set; }

        /// <summary>identifier(numeric integer item) or positive integer</summary>
        public IntegerVariable SendingVariable { get; set; }

        public override string ToString()
        {
            var str = new StringBuilder("SET ");
            if (ReceivingIndexes != null)
            {
                foreach (var receivingIndex in ReceivingIndexes)
                {
                    str.Append(' ');
                    str.Append(receivingIndex);
                }
            }
            if (IncrementDirection != null)
            {
                if (IncrementDirection.Value == IndexIncrementDirection.Up) str.Append(" UP BY ");
                else if (IncrementDirection.Value == IndexIncrementDirection.Down) str.Append(" DOWN BY ");
            }
            else str.Append(" ");
            if (SendingVariable != null) str.Append(SendingVariable);
            return str.ToString();
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, SendingVariable, IncrementDirection)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>)ReceivingIndexes);
        }
    }

    public enum IndexIncrementDirection
    {
        Up,
        Down
    }

    /// <summary> Format 3: SET for external switches: set externalSwitches to ON|OFF</summary>
    internal class SetStatementForSwitches : SetStatement
    {
        public SetStatementForSwitches() : base(StatementType.SetStatementForSwitches) { }

        /// <summary>mnemonicForUPSISwitchNameReference+ TO (ON | OFF)</summary>
        public IList<SetUPSISwitchInstruction> SetUPSISwitchInstructions { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                    && this.ContinueVisitToChildren(astVisitor, SetUPSISwitchInstructions);
        }

        public override string ToString()
        {
            var str = new StringBuilder("SET ");
            var map = new Dictionary<UPSISwitchPosition, List<SymbolReference>>();
            map[UPSISwitchPosition.On] = new List<SymbolReference>();
            map[UPSISwitchPosition.Off] = new List<SymbolReference>();
            foreach (var instruction in SetUPSISwitchInstructions)
            {
                if (instruction.SwitchPosition != null)
                    map[instruction.SwitchPosition.Value].Add(instruction.MnemonicForUPSISwitchName);
            }
            var names = map[UPSISwitchPosition.On];
            foreach (var name in names) str.Append(' ').Append(name);
            if (names.Count > 0) str.Append(" TO ON");
            names = map[UPSISwitchPosition.Off];
            if (names.Count > 0 && map[UPSISwitchPosition.On].Count > 0) str.AppendLine().Append("    ");
            foreach (var name in names) str.Append(' ').Append(name);
            if (names.Count > 0) str.Append(" TO OFF");
            if ((map[UPSISwitchPosition.On].Count == 0) && (map[UPSISwitchPosition.Off].Count == 0)) str.Append("?");
            return str.ToString();
        }
    }

    public class SetUPSISwitchInstruction : IVisitable
    {
        public SymbolReference MnemonicForUPSISwitchName { get; set; }
        public SyntaxProperty<UPSISwitchPosition> SwitchPosition { get; set; }

        public bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, MnemonicForUPSISwitchName, SwitchPosition);
        }
    }

    public enum UPSISwitchPosition
    {
        On,
        Off
    }

    /// <summary>
    /// Format 4: SET for condition-names (to true)
    /// set condition-names to true
    ///          List<Identifier> ConditionIdentifiers //identifier
    /// </summary>
    internal class SetStatementForConditions : SetStatement
    {
        public SetStatementForConditions() : base(StatementType.SetStatementForConditions) { }

        /// <summary>identifier (condition-name)</summary>
        public ReceivingStorageArea[] Conditions { get; set; }

        /// <summary>Always TRUE in COBOL, can be either TRUE or FALSE in TypeCobol.</summary>
        public BooleanValue SendingValue { get; set; }

        public override string ToString()
        {
            var str = new StringBuilder("SET");
            foreach (var condition in Conditions) str.Append(' ').Append(condition);
            str.Append(" TO ").Append(SendingValue.Value.ToString()).AppendLine();
            return str.ToString();
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, SendingValue)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>)Conditions);
        }

        private bool? SendingItem
        {
            get
            {
                if (SendingValue == null) return null;
                return SendingValue.Value;
            }
        }
        public override IDictionary<QualifiedName, object> Variables
        {
            get
            {
                if (variables != null) return variables;
                variables = new Dictionary<QualifiedName, object>();
                //			variables.Add(new URI(SendingValue.Value.ToString()), null);
                foreach (var item in Conditions) variables.Add(new URI(item.MainSymbolReference.Name), SendingItem);
                return variables;
            }
        }
    }

    internal class SetStatementPartial : SetStatement
    {
        public SetStatementPartial() : base(StatementType.SetStatementPartial)
        {
        }
    }

}
