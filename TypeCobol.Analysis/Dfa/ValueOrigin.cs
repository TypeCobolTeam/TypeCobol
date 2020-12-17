using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis.Dfa
{
    /// <summary>
    /// Describes the possible values for a variable through a tree.
    /// </summary>
    public class ValueOrigin
    {
        /// <summary>
        /// Computes the ValueOrigin from a starting UsePoint.
        /// The Algorithm works like that:
        /// (1) If the UsePoint has no associated UseDef set then return.
        /// (2) The UsePoint has a UseDef set.
        /// (2.1) For each Definition Point of the UseDef set
        /// (2.1.1) Only take in account those DefinitionPoints associated to a MOVE or a SET instruction or initial value assignment.
        /// (2.1.1.1) a MOVE statement
        /// (2.1.1.1.1)If the MoveStatement is a SimpleMoveStatement and the sending variable is a Literal alphabetic value
        ///     then append a value origin path with this value.
        /// (2.1.1.1.2)If the MoveStatement is a SimpleMoveStatement and the sending variable is an Identifier.
        ///             then look for the UsePoint of the sending identifier in the definition block
        ///             and then recurse to compute its value origin path
        /// (2.1.1.2) a SET statement
        ///     (2.1.1.2.1) The Set instruction is Condition variable set then
        ///                 the value is the first value of the level 88 variable
        /// (2.1.1.3) a DataDescription or Redefines Entry from a Data Definition.
        ///     (2.1.1.3.1) Take the initial value and add it to the origins path.
        /// </summary>
        /// <param name="start">UsePoint to start the analysis.</param>
        /// <param name="dfaBuilder">Data Flow Graph builder.</param>
        /// <returns>New instance of ValueOrigin for the variable of the given UsePoint.</returns>
        public static ValueOrigin ComputeFrom(DfaUsePoint<Node, VariableSymbol> start, DataFlowGraphBuilder<Node, VariableSymbol> dfaBuilder)
        {
            System.Diagnostics.Debug.Assert(start != null);
            System.Diagnostics.Debug.Assert(dfaBuilder != null);
            System.Diagnostics.Debug.Assert(dfaBuilder.IsUseDefSetCalculated);
            var variable = start.Variable;
            System.Diagnostics.Debug.Assert(variable != null);

            if (start.UseDef == null || start.UseDef.Cardinality() == 0)
            {
                //(1) No Definitions ==> This is an uninitialized variable !
                return null;
            }

            //(2) Walk through defs to build the origin chain
            var result = new ValueOrigin(variable)
                         {
                             Origins = new List<ValueOrigin>()
                         };
            int nextDef = -1;
            while ((nextDef = start.UseDef.NextSetBit(nextDef + 1)) >= 0)
            {
                //(2.1)
                var defPoint = dfaBuilder.DefList[nextDef];
                switch (defPoint.Instruction.CodeElement.Type)
                {
                    //(2.1.1) These are the currently supported statements
                    case CodeElementType.MoveStatement:
                        //(2.1.1.1)
                        ProcessMoveStatement((MoveStatement) defPoint.Instruction.CodeElement);
                        break;
                    case CodeElementType.SetStatement:
                        //(2.1.1.2)
                        ProcessSetStatement((SetStatement) defPoint.Instruction.CodeElement);
                        break;
                    case CodeElementType.DataDescriptionEntry:
                    case CodeElementType.DataRedefinesEntry:
                        //(2.1.1.3)
                        ProcessDataDefinition();
                        break;
                }

                void ProcessMoveStatement(MoveStatement move)
                {
                    switch (move.StatementType)
                    {
                        case StatementType.MoveSimpleStatement:
                            MoveSimpleStatement simpleMove = (MoveSimpleStatement) move;
                            if (simpleMove.SendingVariable.IsLiteral && simpleMove.SendingVariable.AlphanumericValue != null)
                            {
                                //(2.1.1.1.1) MOVE 'LITERAL' TO var
                                var literal = simpleMove.SendingVariable.AlphanumericValue;
                                var origin = new ValueOrigin(variable)
                                {
                                    Value = new Value(literal)
                                };
                                result.Origins.Add(origin);
                            }
                            else if (!simpleMove.SendingVariable.IsLiteral && simpleMove.SendingVariable.StorageArea != null)
                            {
                                //(2.1.1.1.2) MOVE anotherVar TO var
                                //Find the corresponding UsePoint and recurse
                                var sendingVariable = defPoint.Instruction.StorageAreaReadsSymbol[simpleMove.SendingVariable.StorageArea];
                                var defBlockData = dfaBuilder.Cfg.AllBlocks[defPoint.BlockIndex].Data;
                                for (int i = defBlockData.UseListFirstIndex; i < defBlockData.UseListFirstIndex + defBlockData.UseCount; i++)
                                {
                                    var usePoint = dfaBuilder.UseList[i];
                                    if (usePoint.Instruction == defPoint.Instruction && usePoint.Variable == sendingVariable)
                                    {
                                        //We have found the correct UsePoint
                                        var origin = ComputeFrom(usePoint, dfaBuilder);
                                        result.Origins.Add(origin);
                                        break;
                                    }
                                }
                            }
                            break;
                        case StatementType.MoveCorrespondingStatement:
                            //Not supported
                            break;
                    }
                }

                void ProcessSetStatement(SetStatement set)
                {
                    switch (set.StatementType)
                    {
                        case StatementType.SetStatementForConditions:
                            //(2.1.1.2.1) SET level88 TO TRUE
                            //We assume Cobol version prior to 6.2 here. SET lvl88 TO FALSE from v6.2 is not supported yet.
                            System.Diagnostics.Debug.Assert(defPoint.UserData != null);
                            var conditionVariable = (VariableSymbol) defPoint.UserData;
                            if (conditionVariable.Value != null && conditionVariable.Value is Value[] conditionValues && conditionValues.Length > 0)
                            {
                                var origin = new ValueOrigin(conditionVariable)
                                {
                                    Value = conditionValues[0]
                                };
                                result.Origins.Add(origin);
                            }
                            break;
                    }
                }

                void ProcessDataDefinition()
                {
                    System.Diagnostics.Debug.Assert(variable.Value is Value);
                    //(2.1.1.3.1) Take the initial value and add it to the origins path.
                    var origin = new ValueOrigin(variable)
                    {
                        Value = (Value) variable.Value
                    };
                    result.Origins.Add(origin);
                }
            }

            return result;
        }

        /// <summary>
        /// Variable set.
        /// </summary>
        [NotNull]
        public VariableSymbol Variable { get; }

        /// <summary>
        /// Value set into the variable. It can be null, in that case the
        /// Origins collection will give the possible assignation paths for this variable.
        /// </summary>
        [CanBeNull]
        public Value Value { get; private set; }
        
        /// <summary>
        /// Possible assignation paths for this variable. If null, check the Value property
        /// which would give the value set into the variable.
        /// </summary>
        [CanBeNull]
        public List<ValueOrigin> Origins { get; private set; }

        private ValueOrigin(VariableSymbol variable)
        {
            Variable = variable;
        }
    }
}
