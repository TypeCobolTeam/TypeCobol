using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using TypeCobol.TemplateCore.Model;

namespace TypeCobol.TemplateCore.SaxParser
{
    /// <summary>
    /// This is the Sax Parser of a Skeleton file based on the pseudo LL(1) attributed grammar below
    /// =============================================================================================
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
    public class SkeletonSaxParser : SaxParser
    {
        /// <summary>
        /// The Main Skeletons model.
        /// </summary>
        public Skeletons Skeletons
        {
            get;
            private set;
        }
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="skeletonFile">The Skeleton file to parse</param>
        public SkeletonSaxParser(String skeletonFile) : base(skeletonFile)
        {
            //Get the First Token.
            Lookahead = Scanner.NextToken;
            Parse();
        }

        /// <summary>
        /// Constructor with a schema file to validate against.
        /// </summary>
        /// <param name="skeletonFile">The Skeleton XML file</param>
        /// <param name="skeletonSchemaFile">The Skeleton Schema file</param>
        public SkeletonSaxParser(String skeletonFile, String skeletonSchemaFile) : base(skeletonFile, skeletonSchemaFile)
        {

        }

        /// <summary>
        /// Copys the current Token's attributes in the target entity
        /// </summary>
        /// <param name="token">The Token to copy attributes</param>
        /// <param name="entity">The entity into which attributes will be copied.</param>
        private static void CopyTokenAttributes(SaxToken? token, AttributedEntity entity)
        {
            if (token != null && token.Value.attributes != null)
            {
                foreach (var e in token.Value.attributes)
                {
                    Model.Attribute attr = new Model.Attribute();
                    attr.Name = e.Key;
                    attr.Value = e.Value;
                    entity[e.Key] = attr;
                }
            }
        }

        /// <summary>
        /// Parse Skeleton the file according to the grammar.
        /// </summary>
        protected override void Parse()
        {
            //Version
            if (Lookahead == null)
                return;
            ParseSkeletons();       
        }

        /// <summary>
        /// Parse a Skeletons tag
        /// </summary>
        protected void ParseSkeletons()
        {
            SaxToken? token = Match(XmlNodeType.Element, TagNames.Skeletons);
            Skeletons = new Skeletons();
            CopyTokenAttributes(token, Skeletons);
            while (TestLookahead(XmlNodeType.Element, TagNames.Skeleton))
            {   //Parse each single Skeleton
                token = Match(XmlNodeType.Element, TagNames.Skeleton);
                Skeleton skeleton = ParseSkeleton();
                //Add it to the model
                Skeletons.Add(skeleton);
            }
            token = Match(XmlNodeType.EndElement, TagNames.Skeletons);
        }

        /// <summary>
        /// Parse a Single Skeleton        
        /// </summary>
        /// <returns>The Parsed skeleton</returns>
        protected Skeleton ParseSkeleton()
        {
            SaxToken? token = Match(XmlNodeType.Element, TagNames.Skeleton);
            Skeleton skeleton = new Skeleton();
            CopyTokenAttributes(token, skeleton);
            //Parse Conditions
            if(TestLookahead(XmlNodeType.Element, TagNames.Condition))
            {
                skeleton.Conditions = ParseConditions();
            }
            if (TestLookahead(XmlNodeType.Element, TagNames.Patterns))
            {                
            }
            //Parse Patterns
            token = Match(XmlNodeType.EndElement, TagNames.Skeleton);
            return skeleton;
        }

        /// <summary>
        /// Parse Conditions
        /// </summary>
        /// <returns>The Conditions instance object</returns>
        protected Conditions ParseConditions()
        {
            SaxToken? token = Match(XmlNodeType.Element, TagNames.Conditions);
            Conditions conditions = new Conditions();
            CopyTokenAttributes(token, conditions);
            while (TestLookahead(XmlNodeType.Element, TagNames.Condition))
            {
                Condition condition = ParseCondition();
                conditions.Add(condition);
            }
            token = Match(XmlNodeType.EndElement, TagNames.Conditions);
            return conditions;
        }

        /// <summary>
        /// Parse a single condition.
        /// </summary>
        /// <returns>The Parsed condition</returns>
        protected Condition ParseCondition()
        {
            SaxToken? token = Match(XmlNodeType.Element, TagNames.Condition);
            Condition condition = new Condition();
            CopyTokenAttributes(token, condition);
            OptionalMatch(XmlNodeType.Element, TagNames.Condition);
            return condition;
        }

        /// <summary>
        /// Parse Patterns
        /// </summary>
        /// <returns>The Patterns instance object</returns>
        protected Patterns ParsePatterns()
        {
            SaxToken? token = Match(XmlNodeType.Element, TagNames.Patterns);
            Patterns patterns = new Patterns();
            CopyTokenAttributes(token, patterns);
            while (TestLookahead(XmlNodeType.Element, TagNames.Pattern))
            {
                Pattern pattern = ParsePattern();
                patterns.Add(pattern);
            }
            token = Match(XmlNodeType.EndElement, TagNames.Conditions);
            return patterns;
        }

        /// <summary>
        /// Parse a single pattern.
        /// </summary>
        /// <returns>The Parsed pattern</returns>
        protected Pattern ParsePattern()
        {
            SaxToken? token = Match(XmlNodeType.Element, TagNames.Pattern);
            Pattern pattern = new Pattern();
            pattern.Code = token.Value.text;
            CopyTokenAttributes(token, pattern);
            OptionalMatch(XmlNodeType.Element, TagNames.Pattern);
            return pattern;
        }
    }
}
