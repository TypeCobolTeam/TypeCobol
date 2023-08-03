using System;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements {

    using JetBrains.Annotations;
    using System.Collections.Generic;

    /// <summary>p369: The MOVE statement transfers data from one area of storage to one or more other areas.</summary>
    public abstract class MoveStatement : StatementElement, VariableWriter
    {
        protected MoveStatement(StatementType statementType) : base(CodeElementType.MoveStatement, statementType) { }
    // [TYPECOBOL]
        public SyntaxProperty<bool> Unsafe { get; set; }
        public bool IsUnsafe { get { return Unsafe != null && Unsafe.Value; } }
    // [/TYPECOBOL]

        protected IDictionary<StorageArea, object> variables;
        protected FunctionCall _functions = null;

        public abstract IDictionary<StorageArea, object> Variables { get; }
        public virtual  IDictionary<StorageArea,object> VariablesWritten {
            [NotNull]
            get {
                return Variables;
            }
        }

        public abstract FunctionCall FunctionCall { get; }
        
        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && astVisitor.VisitVariableWriter(this)
                   && this.ContinueVisitToChildren(astVisitor, Unsafe) //Order is important here, as unsafe is part of VariableWriter interface
                   && this.ContinueVisitToChildren(astVisitor, FunctionCalls); //Order is important here, as FunctionsCall is part of VariableWriter interface
        }
    }

    /// <summary>
    ///  p369: Format 1: MOVE statement
    ///
    /// When format 1 (no CORRESPONDING) is specified:
    /// * All identifiers can reference alphanumeric group items, national group items, or
    /// elementary items.
    /// * When one of identifier-1 or identifier-2 references a national group item and the
    /// other operand references an alphanumeric group item, the national group is
    /// processed as a group item; in all other cases, the national group item is
    /// processed as an elementary data item of category national.
    /// * The data in the sending area is moved into the data item referenced by each
    /// identifier-2 in the order in which the identifier-2 data items are specified in the
    /// MOVE statement. See “Elementary moves” on page 370 and “Group moves” on page 374 below.
    /// </summary>
    public class MoveSimpleStatement : MoveStatement {
        public MoveSimpleStatement(Variable sendingVariable, ReceivingStorageArea[] receivingStorageAreas, BooleanValue sendingBoolean)
                : base(StatementType.MoveSimpleStatement) {
            SendingVariable = sendingVariable;
            SendingBoolean = sendingBoolean;
            //StorageAreaWrites is set with receivingStorageAreas by CobolCodeElementBuilder
        }

        /// <summary>The sending area.</summary>
        public Variable SendingVariable { get; private set; }
    // [TYPECOBOL]
        public BooleanValue SendingBoolean { get; private set; }
    // [/TYPECOBOL]
        /// <summary>The receiving areas. Must not reference an intrinsic function.</summary>
        //public override IList<ReceivingStorageArea> StorageAreaWrites { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, SendingVariable, SendingBoolean);
        }

        public override IDictionary<StorageArea, object> Variables {
            [NotNull]
            get
            {
                if (variables != null) return variables;
                variables = new Dictionary<StorageArea, object>();

                if (StorageAreaWrites != null)
                {
                    foreach (var writeStorage in StorageAreaWrites)
                    {
                        if (writeStorage?.StorageArea == null) continue;

                        if (variables.ContainsKey(writeStorage.StorageArea))
                            if (writeStorage.StorageArea is DataOrConditionStorageArea)
                                continue; // same variable with (presumably) different subscript
                            else throw new ArgumentException(writeStorage.StorageArea + " already written, but not subscripted?");
                        else variables.Add(writeStorage.StorageArea, SendingItem);
                    }
                }

                return variables;
            }
        }
        

        public object SendingItem {
            [CanBeNull]
            get {
                if (SendingVariable != null) {
                    if (SendingVariable.IsLiteral) {
                        if (SendingVariable.NumericValue != null) return SendingVariable.NumericValue.Value;
                        if (SendingVariable.AlphanumericValue != null) return SendingVariable.AlphanumericValue.Value;
                        throw new System.NotSupportedException();
                    }
                    if(SendingVariable.MainSymbolReference != null)
                        return new URI(SendingVariable.MainSymbolReference.Name);
                    return null;
                }
                if (SendingBoolean != null) return SendingBoolean.Value;
                return null;
            }
        }

        public override FunctionCall FunctionCall {
            [NotNull]
            get {
                if (_functions != null) return _functions;

                FunctionCallResult sending = null;
                if (SendingVariable != null) sending = SendingVariable.StorageArea as FunctionCallResult;
                if (sending != null) return sending.FunctionCall;
                return _functions;
            }
        }
    }

    /// <summary>
    /// p369: Format 2: MOVE statement with CORRESPONDING phrase
    ///
    /// When format 2 (with CORRESPONDING) is specified:
    /// * Both identifiers must be group items.
    /// * A national group item is processed as a group item (and not as an elementary
    /// data item of category national).
    /// * Selected items in identifier-1 are moved to identifier-2 according to the rules for
    /// the “CORRESPONDING phrase” on page 281. The results are the same as if
    /// each pair of CORRESPONDING identifiers were referenced in a separate MOVE
    /// statement.
    /// </summary>
    public class MoveCorrespondingStatement : MoveStatement {
        public MoveCorrespondingStatement() : base(StatementType.MoveCorrespondingStatement) { }

        /// <summary>
        /// identifier-1
        /// The sending group item.
        /// </summary>
        public DataOrConditionStorageArea FromGroupItem { get; set; }

        /// <summary>
        /// identifier-2
        /// The receiving group item.
        /// </summary>
        public DataOrConditionStorageArea ToGroupItem { get; set; }


        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, FromGroupItem, ToGroupItem);
        }

        public override IDictionary<StorageArea, object> Variables {
            [NotNull]
            get {
                if (variables != null) return variables;

                variables = new Dictionary<StorageArea, object>();
                if (ToGroupItem != null)
                    variables.Add(ToGroupItem, FromGroupItem);
                return variables;
            }
        }

        public override FunctionCall FunctionCall {
            [NotNull]
            get {
                return _functions;
            }
        }
    }



}
