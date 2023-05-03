using System.Runtime.CompilerServices;
using System.Text;

namespace TypeCobol.Tools
{
    internal static class ModuleInitializer
    {
        /// <summary>
        /// Called once when the assembly is loaded for the first time. Allows to perform one-time initializations
        /// at assembly level (new C# 9 feature)
        /// </summary>
        [ModuleInitializer]
        internal static void Initialize()
        {
            // Required to retrieve encoding by code page number.
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        }
    }
}
