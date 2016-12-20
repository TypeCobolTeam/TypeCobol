using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// All level of Cobol language from the oldest to the newest.
    /// 
    /// Please note that IBM Compiler on Z/OS can be accross 2 language levels. If more details are needed about that, a new enum must be created.
    /// </summary>
    public enum CobolLanguageLevel
    {
        Cobol85,
        Cobol2002,
        //Note that Cobol 2014 is currently not used in our project
        Cobol2014,
        TypeCobol
    }

    /// <summary>
    /// Indicates the CobolLanguageLevel required to use by this object.
    /// </summary>
    public interface ICobolLanguageLevel {
        CobolLanguageLevel CobolLanguageLevel { get; }
    }

    public static class LanguageLevelUtils {
        /// <summary>
        /// Return the max CobolLanguageLevel found in the specified Enumerable of ILanguageLevel
        /// The max definition is determined by the values of CobolLanguageLevel enumeration.
        /// </summary>
        /// <param name="languageLevelEnumerable"></param>
        /// <param name="defaultCobolLanguageLevel"></param>
        /// <returns>The max CobolLanguageLevel in the enumerable specified or defaultCobolLanguageLevel if there are no elements</returns>
        public static CobolLanguageLevel GetMaxLanguageLevel([CanBeNull] IEnumerable<ICobolLanguageLevel> languageLevelEnumerable, 
                                                             CobolLanguageLevel defaultCobolLanguageLevel = CobolLanguageLevel.Cobol85) {
            return languageLevelEnumerable != null ? languageLevelEnumerable.Max(ll => ll.CobolLanguageLevel) : defaultCobolLanguageLevel;
        }
    }
}
