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
		public bool ToOn  { get; set; }
		public bool ToOff { get; set; }

        //TO avoid creating a new StringBuilder and as SetExternalSwitch is only used by SetStatementForSwitches
        //This CodeElement doesn't define a ToString() method
        //public override string ToString()
	}
}