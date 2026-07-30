using System.Collections.Generic;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    public enum HostVariableDirection
    {
        IN,
        OUT
    }

    public class HostVariableBinding
    {
        public string ColumnName { get; set; }
        public string VariableName { get; set; }
        public string IndicatorName { get; set; }
        public HostVariableDirection Direction { get; set; }

        public HostVariableBinding(string variableName, HostVariableDirection direction, string columnName = null, string indicatorName = null)
        {
            VariableName = variableName;
            Direction = direction;
            ColumnName = columnName;
            IndicatorName = indicatorName;
        }
    }
}
