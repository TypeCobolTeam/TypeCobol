using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Document;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reactive.Linq;
using System.Reactive.Concurrency;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Text;
using TypeCobolStudio.Editor;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.TypeChecker;
using TypeCobol.Compiler.Directives;

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
           
            // Create a new compilation unit for this document
            if (textName != null)
            {
                CompilationUnit = new CompilationUnit(null, textName, project.SourceFileProvider, project, TextDocument, compilerOptions);                    
            }
            else
            {
                CompilationUnit = new CompilationUnit(TextDocument, project.SourceFileProvider, project, compilerOptions);
            }

            // Compile in the background every 500 ms
            CompilationUnit.SetupCodeAnalysisPipeline(BackgroundCompilationScheduler, 500);

            // Refresh the syntax coloring for rescanned lines
            CompilationUnit.TokensDocument.TokensChangedEventsSource.Subscribe(textEditor);

            // Listen to all compilation errors found by the incremental compiler => on the dispatcher thread to udpate the UI
            CompilationUnit.SemanticsDocument.CompilationErrorsEventsSource.ObserveOn(DispatcherScheduler.Current).Subscribe(errorObserver);

            // Trigger initial notification after document load
            CompilationUnit.StartDocumentProcessing();
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
