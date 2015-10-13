using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Reactive.Concurrency;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Text;
using TypeCobolStudio.Editor;
using TypeCobol.Compiler.TypeChecker;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Scanner;

namespace TypeCobolStudio
{
    public class TypeCobolCompilerService : IDisposable
    {
        public ITextDocument TextDocument { get; private set; }

        public CompilationUnit CompilationUnit { get; private set; }

        public IScheduler BackgroundCompilationScheduler { get; private set; }

        private TypeCobolCompilerService(TypeCobolEditor textEditor, CompilationProject project, string textName, TypeCobolOptions compilerOptions, IObserver<IList<CompilationError>> errorObserver)
		{
            TextDocument = textEditor.TextDocument;

            // DEMO : TaskPoolScheduler can not be used yet because of Length property access on ITextDocument by CobolCharStream
            BackgroundCompilationScheduler = DispatcherScheduler.Current ;

            // Create a new file compiler for this document
            FileCompiler compiler = null;
            if (textName != null)
            {
                compiler = new FileCompiler(null, textName, project.SourceFileProvider, project, TextDocument, compilerOptions, false);                    
            }
            else
            {
                compiler = new FileCompiler(TextDocument, project.SourceFileProvider, project, compilerOptions, false);
            }
            CompilationUnit = compiler.CompilationResultsForProgram;

            // Compile in the background
            compiler.StartContinuousBackgroundCompilation(400, 400, 900, 2000);

            // Refresh the syntax coloring for rescanned lines
            IObservable<DocumentChangedEvent<ITokensLine>> tokensLinesObservable = 
                Observable.FromEvent<EventHandler<DocumentChangedEvent<ITokensLine>>,DocumentChangedEvent<ITokensLine>>(
                    evtHandler => CompilationUnit.TokensLinesChanged += evtHandler,
                    evtHandler => CompilationUnit.TokensLinesChanged -= evtHandler);
            tokensLinesObservable.Subscribe(textEditor);

            // Listen to all compilation errors found by the incremental compiler => on the dispatcher thread to udpate the UI
            // --> TO DO : there is no way yet to observe compilation errors               
            //compilationErrorsObservable.ObserveOn(DispatcherScheduler.Current).Subscribe(errorObserver);

            // Trigger initial notification after document load
            compiler.StartDocumentProcessing();
		}
    
        public void Dispose()
        {
        }

        public static void AttachIncrementalCompilerToTextEditorDocument(TypeCobolEditor textEditor, CompilationProject project, string textName, TypeCobolOptions compilerOptions, IObserver<IList<CompilationError>> errorObserver)
        {
            var serviceContainer = textEditor.TextArea.TextView.Services;
            if (serviceContainer.GetService(typeof(TypeCobolCompilerService)) != null)
            {
                serviceContainer.RemoveService(typeof(TypeCobolCompilerService));
            }

            TypeCobolCompilerService compilerService = new TypeCobolCompilerService(textEditor, project, textName, compilerOptions, errorObserver);
            serviceContainer.AddService(typeof(TypeCobolCompilerService), compilerService);
        }
    }
}
