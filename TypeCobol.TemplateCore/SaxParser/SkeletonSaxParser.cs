using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.TemplateCore.SaxParser
{
    /// <summary>
    /// This the Sax Parser of a Skeleton file base on the pseudo LL(1) attributed grammar below
    /// 
    /// skeletons ::= SKELETONS_TAG skeleton_list END_SKELETONS_TAG
    /// ;
    /// skeleton_list ::= skeleton(name, var)
    /// | skeleton_list skeleton(name, var)
    /// ;
    /// skeleton(name, var) ::= SKELETON_TAG conditions patterns END_SKELETON_TAG
    /// ;
    /// conditions ::= CONDITIONS_TAG condition_list END_CONDITIONS_TAG
    /// ;
    /// condition_list ::= condition(node, name, level, type, sender, receiver,unsafe,function,definitions,variables,typecobol,visibility,copyname,Usage,isPointerIncrementation,receiverUsage)
    /// | condition_list condition(node, name, level, type, sender, receiver,unsafe,function,definitions,variables,typecobol,visibility,copyname,Usage,isPointerIncrementation,receiverUsage)
    /// ;
    /// condition(node, name, level, type, sender, receiver,unsafe,function,definitions,variables,typecobol,visibility,copyname,Usage,isPointerIncrementation,receiverUsage) :: CONDITION_TAG optionalEndConditionTag;	
    /// ;
    /// optionalEndConditionTag ::= /* empty */
    /// | END_CONDITION_TAG
    /// ;
    /// patterns ::= PATTERNS_TAG pattern_list END_PATTERNS_TAG
    /// ;
    /// pattern_list ::= pattern(name, group, location, action, var, position, deprecated, newline, boolean_property)
    /// | pattern_list pattern(name, group, location, action, var, position, deprecated, newline, boolean_property)
    /// ;
    /// pattern(name, group, location, action, var, position, deprecated, newline, boolean_property) ::= PATTERN_TAG optionalText optionalEndPatternTag
    /// ;
    /// optionalText ::= /* empty */
    /// | Text
    /// ;
    /// optionalEndPatternTag ::= /* empty */
    /// | END_PATTERN_TAG
    /// ;
    /// </summary>
    public class SkeletonSaxParser
    {
    }
}
