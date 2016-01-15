using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    ///     Format 3: SET for external switches
    /// </summary>
    internal class SetStatementForSwitches : SetStatement
    {
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
                if (externalSwitch.ToOn != null && externalSwitch.ToOn.Value)
                {
                    sb.AppendLine(" TO ON");
                }
                else if (externalSwitch.ToOff != null && externalSwitch.ToOff.Value)
                {
                    sb.AppendLine(" TO OFF");
                } else
                {
                    sb.AppendLine("");
                }
            }
            return sb.ToString();
        }
    }

    public class SetExternalSwitch
    {
        public List<MnemonicForEnvironmentName> MnemonicForEnvironmentNames { get; set; }

        public SyntaxBoolean ToOn { get; set; }

        public SyntaxBoolean ToOff { get; set; }


        //TO avoid creating a new StringBuilder and as SetExternalSwitch is only used by SetStatementForSwitches
        //This CodeElement doesn't define a ToString() method
        //public override string ToString()
    }
}