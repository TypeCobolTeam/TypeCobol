using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

public static class ScannerUtils
{
    /// <summary>
    /// Look for pattern ':' (cobol word chars)+ ':'
    /// </summary>
    /// <param name="line">The source line</param>
    /// <param name="startIndex">zero based start index in the source line</param>
    /// <param name="lastIndex">zero based last index (included) in the source line</param>
    /// <param name="interpretDoubleColonAsQualifiedNameSeparator">true to interpret :: as TypeCobol qualifier separator</param>
    /// <param name="patternEndIndex">Out last index of the : separator</param>
    /// <returns>true if the region in the line is a Partial Cobol Word, false otherwise</returns>
    public static bool CheckForPartialCobolWordPattern(string line, int startIndex, int lastIndex, 
        bool interpretDoubleColonAsQualifiedNameSeparator, out int patternEndIndex)
    {
        //This method is always called on a starting ':'
        System.Diagnostics.Debug.Assert(line[startIndex] == ':');
        patternEndIndex = -1;

        // match leading spaces if any
        int index = startIndex + 1;
        for (; index <= lastIndex && line[index] == ' '; index++)
        { }

        // match all legal cobol word chars
        for (; index <= lastIndex && CobolChar.IsAllowedInsidePartialCobolWord(line[index]); index++)
        { }

        // match trailing spaces if any
        for (; index <= lastIndex && line[index] == ' '; index++)
        { }

        // next character must be ':'
        if (line.Length > index && line[index] == ':')
        {
            // Empty tag, we only found '::'
            if (index == startIndex + 1 && interpretDoubleColonAsQualifiedNameSeparator)
            {
                return false;
            }

            // character after is another ':'
            if (line.Length > index + 1 && line[index + 1] == ':' && interpretDoubleColonAsQualifiedNameSeparator)
            {
                return false;
            }

            // Tag is properly closed
            patternEndIndex = index;
            return true;
        }

        return false;
    }
}
