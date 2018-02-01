using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol
{
    public enum ExecutionStep
    {
        Scanner = 0,
        Preprocessor,
        SyntaxCheck,
        SemanticCheck,
        CrossCheck,
        Generate
    }
}
