package typecobol.editors.eclipse;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

public class Configuration extends SourceViewerConfiguration {
	private Scanner scanner;
	private ContentAssistant assistant;

	public Configuration(final Scanner scanner) {
		this.scanner = scanner;
		this.assistant = createContentAssistant();
	}
	@Override
	public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
		return new String[] { IDocument.DEFAULT_CONTENT_TYPE };
	}

	@Override
	public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer) {
		DefaultDamagerRepairer dr = new DefaultDamagerRepairer(scanner);
		PresentationReconciler reconciler = new PresentationReconciler();
		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
		return reconciler;
	}

	@Override
	public IContentAssistant getContentAssistant(final ISourceViewer viewer) {
		assistant.setInformationControlCreator(getInformationControlCreator(viewer));
	    return assistant;
	}

	private static ContentAssistant createContentAssistant() {
		final IContentAssistProcessor processor = new ContentAssistProcessor();
		final ContentAssistant assistant = new ContentAssistant();
		assistant.setContentAssistProcessor (processor, IDocument.DEFAULT_CONTENT_TYPE);
		assistant.enableAutoActivation(true);
		// set orientation for the content assist proposal
		assistant.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
		return assistant;
	}
}
