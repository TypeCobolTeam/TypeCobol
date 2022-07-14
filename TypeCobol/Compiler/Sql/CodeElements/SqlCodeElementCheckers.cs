using System.Collections;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Sql.CodeElements.Statements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    class SqlCodeElementCheckers
    {
        internal static void OnCodeElement(SetAssignmentStatement setAssignmentStatement,
            CodeElementsParser.SetAssignmentStatementContext context)
        {
            foreach (var assignment in setAssignmentStatement.Assignments)
            {
                IList<SourceValue> values = new List<SourceValue>();
                IList<TargetVariable> targets = new List<TargetVariable>();
                if (assignment.Values != null)
                {
                    values = assignment.Values;
                }

                if (assignment.Targets != null)
                {
                    targets = assignment.Targets;
                }
                if (values.Count!=targets.Count)
                {
                    DiagnosticUtils.AddError(setAssignmentStatement,
                        "The number of values on the right hand-side must match the number of targets on the left hand-side of the statement.",
                        context);
                }
                //todo  Check that DEFAULT is specified when the corresponding target is a global variable or a transition variable. If DEFAULT is specified for a transition variable in an advanced trigger, then all target variables must be transition variables, and all source values must be specified with the DEFAULT keyword.
            }
        }
    }
}
