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
        /// </summary>
        /// <param name="start">UsePoint to start the analysis.</param>
        /// <param name="dfaBuilder">Data Flow Graph builder.</param>
        /// <returns>New instance of ValueOrigin for the variable of the given UsePoint.</returns>
        public static ValueOrigin ComputeFrom(DfaUsePoint<Node, Symbol> start, DataFlowGraphBuilder<Node, Symbol> dfaBuilder)
        {
            System.Diagnostics.Debug.Assert(start != null);
            System.Diagnostics.Debug.Assert(dfaBuilder != null);
            var variable = start.Variable as VariableSymbol;
            System.Diagnostics.Debug.Assert(variable != null);

            if (start.UseDef == null || start.UseDef.Cardinality() == 0)
            {
                //No Definitions ==> try to see if the variable has an Initial Value
                if (variable.Value != null && variable.Value is Value value)
                {
                    return new ValueOrigin(variable)
                    {
                        Value = value
                    };
                }

                //This is an uninitialized variable !
                return null;
            }

            //Walk through defs to build the origin chain
            var result = new ValueOrigin(variable)
                         {
                             Origins = new List<ValueOrigin>()
                         };
            int nextDef = -1;
            while ((nextDef = start.UseDef.NextSetBit(nextDef + 1)) >= 0)
            {
                DfaDefPoint<Node, Symbol> defPoint = dfaBuilder.DefList[nextDef];
                switch (defPoint.Instruction.CodeElement.Type)
                {
                    //Currently supported statements
                    case CodeElementType.MoveStatement:
                        ProcessMoveStatement((MoveStatement) defPoint.Instruction.CodeElement);
                        break;
                    case CodeElementType.SetStatement:
                        ProcessSetStatement((SetStatement) defPoint.Instruction.CodeElement);
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
                                //MOVE 'LITERAL' TO var
                                var literal = simpleMove.SendingVariable.AlphanumericValue;
                                result.Value = new Value(literal);
                                result.Origins = null;
                            }
                            else if (!simpleMove.SendingVariable.IsLiteral && simpleMove.SendingVariable.StorageArea != null)
                            {
                                //MOVE anotherVar TO var
                                //Find the corresponding UsePoint and recurse
                                var sendingVariable = defPoint.Instruction.StorageAreaReadsSymbol[simpleMove.SendingVariable.StorageArea];
                                var defBlockData = dfaBuilder.Cfg.AllBlocks[defPoint.BlockIndex].Data;
                                for (int i = defBlockData.UseListFirstIndex; i < defBlockData.UseListFirstIndex + defBlockData.UseCount; i++)
                                {
                                    if (dfaBuilder.UseList[i].Instruction == defPoint.Instruction && dfaBuilder.UseList[i].Variable == sendingVariable)
                                    {
                                        //We have found the correct UsePoint
                                        var origin = ComputeFrom(dfaBuilder.UseList[i], dfaBuilder);
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
                            //SET level88 TO TRUE
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
