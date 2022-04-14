using System;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The configuration section is an optional section for programs and classes, and can
    /// describe the computer environment on which the program or class is compiled and
    /// executed.
    /// </summary>
    public class ConfigurationSectionHeader : CodeElement
    {
        public ConfigurationSectionHeader() : base(CodeElementType.ConfigurationSectionHeader)
        { }
        public override TextAreaType StartingArea => TextAreaType.AreaA;
    }
}
