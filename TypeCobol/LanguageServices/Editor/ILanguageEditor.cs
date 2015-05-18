using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.TypeChecker;

namespace TypeCobol.LanguageServices.Editor
{
    /// <summary>
    /// Any interactive TypeCobol language editor must implement this interface
    /// </summary>
    public interface ILanguageEditor : IObserver<TokensChangedEvent>, IObserver<IList<CompilationError>>
    {
        /// <summary>
        /// Text document currrently displayed by the editor 
        /// </summary>
        ITextDocument TextDocument { get; }
    }
}
