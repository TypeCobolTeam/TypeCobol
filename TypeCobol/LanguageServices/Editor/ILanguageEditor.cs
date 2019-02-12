using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.TypeChecker;

namespace TypeCobol.LanguageServices.Editor
{
    /// <summary>
    /// Any interactive TypeCobol language editor must implement this interface
    /// </summary>
    public interface ILanguageEditor : IObserver<DocumentChangedEvent<ITokensLine>>, IObserver<IList<CompilationError>>
    {
        /// <summary>
        /// Text document currrently displayed by the editor 
        /// </summary>
        ITextDocument TextDocument { get; set; }
    }
}
