using JetBrains.Annotations;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Types
{
    /// <summary>
    /// Contains some helper extension methods on Type objects
    /// </summary>
    public static class TypeExtensions
    {
        public static bool IsNationalOrNationalEdited([CanBeNull] this Type type)
        {
            bool hasPicture = type?.Tag == Type.Tags.Picture;
            if (hasPicture)
            {
                var pictureType = (PictureType)type;
                return pictureType.Category == PictureCategory.National || pictureType.Category == PictureCategory.NationalEdited;
            }

            return false;
        }
    }
}
