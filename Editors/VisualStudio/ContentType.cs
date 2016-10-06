using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace TypeCobol.Editor
{
    /// <summary>
    /// First, declare a content type used to load bits of our extension.
    /// Then map some file extension to our content type (there may be more, but those we'll use for now).
    /// </summary>
    internal static class ContentType
    {
        /// <summary>
        /// Create a content type that all the other pieces of our extension will use to be loaded.
        ///
        /// The majority of components one can provide as extensions to the editor allow a [ContentType] attribute 
        /// for limiting the scope of when the extension will be loaded and activated.
        /// </summary>
        [Export(typeof(ContentTypeDefinition))]
        [Name("TypeCobol")]
        [DisplayName("TypeCobol")]
        [BaseDefinition("text")]
        internal static ContentTypeDefinition TypeCobolContentType = null;

        /// <summary>
        /// Map the .cbl file extension to our content type.
        /// </summary>
        [Export(typeof(FileExtensionToContentTypeDefinition))]
        [ContentType("TypeCobol")]
        [FileExtension(".cbl")]
        internal static FileExtensionToContentTypeDefinition CblFileExtension = null;

        /// <summary>
        /// Map the .cpy file extension to our content type.
        /// </summary>
        [Export(typeof(FileExtensionToContentTypeDefinition))]
        [ContentType("TypeCobol")]
        [FileExtension(".cpy")]
        internal static FileExtensionToContentTypeDefinition CpyFileExtension = null;

        /// <summary>
        /// Map the .cpx file extension to our content type.
        /// </summary>
        [Export(typeof(FileExtensionToContentTypeDefinition))]
        [ContentType("TypeCobol")]
        [FileExtension(".cpx")]
        internal static FileExtensionToContentTypeDefinition CpxFileExtension = null;
    }
}
