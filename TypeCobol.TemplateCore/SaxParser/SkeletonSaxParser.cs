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
    /// =============================================================================================
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
    /// conditions ::= /*empty*/
    /// | CONDITIONS_TAG condition_list END_CONDITIONS_TAG
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
        /// Xml version
        /// </summary>
        public string Version { get; private set; }
        /// <summary>
        /// Xml encoding
        /// </summary>
        public string Encoding { get; private set; }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="skeletonFile">The Skeleton file to parse</param>
        public SkeletonSaxParser(String skeletonFile) : base(skeletonFile)
        {
            //Get the First Token.
            Lookahead = Scanner.NextToken;
        }

        /// <summary>
        /// Constructor with a schema file to validate against.
        /// </summary>
        /// <param name="skeletonFile">The Skeleton XML file</param>
        /// <param name="skeletonSchemaFile">The Skeleton Schema file</param>
        public SkeletonSaxParser(String skeletonFile, String skeletonSchemaFile) : base(skeletonFile, skeletonSchemaFile)
        {
            //Get the First Token.
            Lookahead = Scanner.NextToken;
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
                    entity.Attributes[e.Key] = attr;
                }
            }
        }

        /// <summary>
        /// Validate the Skeleton File by reading all tokens.
        /// </summary>
        public override bool Validate()
        {
            while (this.Scanner.NextToken != null)
                ;
            return ValidationWarningCount == 0 && ValidationErrorCount == 0;
        }
        /// <summary>
        /// Parse Skeleton the file according to the grammar.
        /// </summary>
        public override void Parse()
        {
            //Get the First Token.
            //Lookahead = Scanner.NextToken;
            //Version
            if (Lookahead == null)
                return;
            ParseVersion();
            ParseSkeletons();       
        }

        /// <summary>
        /// Match the Version and the Encoding
        /// </summary>
        protected void ParseVersion()
        {
            if (Lookahead.Value.type == XmlNodeType.XmlDeclaration)
            {
                if (Lookahead.Value.attributes.ContainsKey(AttributeNames.Version))
                    Version = Lookahead.Value.attributes[AttributeNames.Version];
                if (Lookahead.Value.attributes.ContainsKey(AttributeNames.Encoding))
                    Encoding = Lookahead.Value.attributes[AttributeNames.Encoding];
                NextToken();
            }
            else
            {
                throw new ParsingException(Resource.ParserErrorMissingVersionEncoding);
            }
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
            if(TestLookahead(XmlNodeType.Element, TagNames.Conditions))
            {
                skeleton.Conditions = ParseConditions();
            }
            //Parse Patterns
            if (TestLookahead(XmlNodeType.Element, TagNames.Patterns))
            {
                skeleton.Patterns = ParsePatterns();
            }            
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
            OptionalMatch(XmlNodeType.EndElement, TagNames.Condition);
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
            token = Match(XmlNodeType.EndElement, TagNames.Patterns);
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
            if (TestLookahead(XmlNodeType.Text))
            {
                SaxToken? textToken = Match(XmlNodeType.Text);
                pattern.Code = textToken.Value.value;
            }            
            CopyTokenAttributes(token, pattern);
            OptionalMatch(XmlNodeType.EndElement, TagNames.Pattern);
            return pattern;
        }
    }
}
