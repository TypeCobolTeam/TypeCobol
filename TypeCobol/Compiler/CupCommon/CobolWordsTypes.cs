using JetBrains.Annotations;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CupCommon
{
    /// <summary>
    /// Type that represents a list of tokens.
    /// </summary>
    public class TokenList : List<Token> {};

    /// <summary>
    /// 
    /// </summary>
    public class CupReplaceOperation
    {
        public List<Token> From { get; }
        public List<Token> By { get; }

        public Token Leading { get; }
        public Token Trailing { get; }

        public CupReplaceOperation(List<Token> from, List<Token> by, Token leading, Token trailing)
        {
            From = from;
            By = by;
            Leading = leading;
            Trailing = trailing;
        }
    }

    public class CupReplaceOperations : List<CupReplaceOperation>
    {
        public CupReplaceOperations()
        {
        }

        public CupReplaceOperations([NotNull] IEnumerable<CupReplaceOperation> collection) : base(collection)
        {
        }
    }

    /// <summary>
    /// Representation of:
    /// allFigurativeConstant: ALL figurativeConstant
    /// </summary>
    public class AllFigurativeConstant
    {
        public Token All { get; private set; }
    
        public Token FigurativeConstant { get; private set; }

        public AllFigurativeConstant(Token fc)
        {
            All = null;
            FigurativeConstant = fc;
        }

        public AllFigurativeConstant(Token all, Token fc)
        {
            All = all;
            FigurativeConstant = fc;
        }

        /// <summary>
        /// Determine if the ALL token is present or not.
        /// </summary>
        public bool IsAllFigurativeConstant => All != null;
    }

    /// <summary>
    /// Representation of:
    /// qualifiedParagraphNameReference : paragraphNameReference (IN | OF) sectionNameReference

    /// </summary>
    public class QualifiedParagraphNameReference
    {
        public Token ParagraphNameReference { get; private set; }
        public Token InOf { get; private set; }
        public Token SectionNameReference { get; private set; }

        public QualifiedParagraphNameReference(Token paragraphNameReference, Token in_of, Token sectionNameReference)
        {
            this.ParagraphNameReference = paragraphNameReference;
            this.InOf = in_of;
            this.SectionNameReference = sectionNameReference;
        }
    }

    /// <summary>
    /// Representation of
    /// 
    /// procedureName:
    /// paragraphNameReferenceOrSectionNameReference
    /// |    qualifiedParagraphNameReference
    /// /// 
    /// 
    /// </summary>
    public class ProcedureName
    {
        public Token ParagraphNameReferenceOrSectionName { get; private set; }
        public QualifiedParagraphNameReference QualifiedParagraphName { get; private set; }

        /// <summary>
        /// Constructor of
        /// procedureName: paragraphNameReferenceOrSectionNameReference
        /// </summary>
        /// <param name="paragraphNameReferenceOrSectionName"></param>
        public ProcedureName(Token paragraphNameReferenceOrSectionName)
        {
            this.ParagraphNameReferenceOrSectionName = paragraphNameReferenceOrSectionName;
        }

        /// <summary>
        /// Constructor of
        /// 
        /// procedureName: qualifiedParagraphNameReference
        /// </summary>
        /// <param name="qualifiedParagraphName"></param>
        public ProcedureName(QualifiedParagraphNameReference qualifiedParagraphName)
        {
            this.QualifiedParagraphName = qualifiedParagraphName;
        }

        public bool IsQualifiedParagraphName => this.QualifiedParagraphName != null;
    }

    /// <summary>
    /// A List for: ((IN | OF)dataNameReferenceOrFileNameReference)+
    /// </summary>
    public class InOfDataNameReferenceOrFileNameReferences : List<Tuple<Token, Token> >
    {
        public InOfDataNameReferenceOrFileNameReferences()
        {
            
        }
    }

    /// <summary>
    /// Representation of:
    /// 
    /// qualifiedDataName1: dataNameReference((IN | OF) dataNameReferenceOrFileNameReference)+;
    /// 
    /// </summary>
    public class QualifiedDataName1
    {
        public Token DataNameReference {get; private set;}

        public InOfDataNameReferenceOrFileNameReferences InOfDataReferences { get; private set; }

        public QualifiedDataName1(Token dataNameReference, InOfDataNameReferenceOrFileNameReferences inOfDataReferences)
        {
            DataNameReference = dataNameReference;
            InOfDataReferences = inOfDataReferences;
        }
    }

    /// <summary>
    /// Representation of:
    /// 
    /// qualifiedDataName: dataNameReference | qualifiedDataName1;
    /// 
    /// </summary>
    public class QualifiedDataName
    {
        public Token DataNameReference { get; private set; }
        public QualifiedDataName1 QualifiedDataName1 { get; private set; }

        /// <summary>
        /// Constructor for
        /// qualifiedDataName: dataNameReference
        /// </summary>
        /// <param name="dataNameReference"></param>
        public QualifiedDataName(Token dataNameReference)
        {
            this.DataNameReference = dataNameReference;
        }

        /// <summary>
        /// Constructor for:
        /// qualifiedDataName: qualifiedDataName1;
        /// </summary>
        /// <param name="qualifiedDataName1"></param>
        public QualifiedDataName(QualifiedDataName1 qualifiedDataName1)
        {
            this.QualifiedDataName1 = qualifiedDataName1;
        }

        /// <summary>
        /// Determine if this QualifiedDataName is a QualifiedDataName1.
        /// </summary>
        public bool IsQualifiedDataName1 => this.QualifiedDataName1 != null;
    }

    /// <summary>
    /// Representation of:
    /// qualifiedDataNameOrIndexName: dataNameReferenceOrIndexNameReference | qualifiedDataName1;
    /// </summary>
    public class QualifiedDataNameOrIndexName
    {
        public Token DataNameReferenceOrIndexNameReference { get; private set; }
        public QualifiedDataName1 QualifiedDataName1 { get; private set; }

        /// <summary>
        /// Constructor for
        /// qualifiedDataNameOrIndexName: dataNameReferenceOrIndexNameReference
        /// </summary>
        /// <param name="dataNameReferenceOrIndexNameReference"></param>
        public QualifiedDataNameOrIndexName(Token dataNameReferenceOrIndexNameReference)
        {
            this.DataNameReferenceOrIndexNameReference = dataNameReferenceOrIndexNameReference;
        }

        /// <summary>
        /// Constructor for:
        /// qualifiedDataNameOrIndexName: qualifiedDataName1;
        /// </summary>
        /// <param name="qualifiedDataName1"></param>
        public QualifiedDataNameOrIndexName(QualifiedDataName1 qualifiedDataName1)
        {
            this.QualifiedDataName1 = qualifiedDataName1;
        }

        /// <summary>
        /// Determine if this QualifiedDataNameOrIndexName is a QualifiedDataName1.
        /// </summary>
        public bool IsQualifiedDataName1 => this.QualifiedDataName1 != null;
    }

    /// <summary>
    /// Representation of:
    /// 
    /// qualifiedConditionName:     conditionNameReferenceOrConditionForUPSISwitchNameReference((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
    /// 
    /// </summary>
    public class QualifiedConditionName
    {
        public Token ConditionNameReferenceOrConditionForUPSISwitchNameReference { get; private set; }

        public InOfDataNameReferenceOrFileNameReferences InOfDataReferences { get; private set; }

        public QualifiedConditionName(Token conditionNameReferenceOrConditionForUPSISwitchNameReference, InOfDataNameReferenceOrFileNameReferences inOfDataReferences)
        {
            ConditionNameReferenceOrConditionForUPSISwitchNameReference = conditionNameReferenceOrConditionForUPSISwitchNameReference;
            InOfDataReferences = inOfDataReferences;
        }
    }

    /// <summary>
    /// Representation of:
    /// 
    /// qualifiedDataNameOrQualifiedConditionName1:
    /// dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)+;
    /// 
    /// </summary>
    public class QualifiedDataNameOrQualifiedConditionName1
    {
        public Token DataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference { get; private set; }

        public InOfDataNameReferenceOrFileNameReferences InOfDataReferences { get; private set; }

        public QualifiedDataNameOrQualifiedConditionName1(Token dataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference, InOfDataNameReferenceOrFileNameReferences inOfDataReferences)
        {
            DataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference = dataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference;
            InOfDataReferences = inOfDataReferences;
        }
    }

    /// <summary>
    /// Representation of:
    /// qualifiedDataNameOrQualifiedConditionName:
    /// dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference | qualifiedDataNameOrQualifiedConditionName1;
    /// </summary>
    public class QualifiedDataNameOrQualifiedConditionName
    {
        public Token DataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference { get; private set;}
        public QualifiedDataNameOrQualifiedConditionName1 QualifiedDataNameOrQualifiedConditionName1 { get; private set; }

        /// <summary>
        /// Constructor of 
        /// qualifiedDataNameOrQualifiedConditionName: dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference
        /// </summary>
        /// <param name="dataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference"></param>
        public QualifiedDataNameOrQualifiedConditionName(Token dataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference)
        {
            this.DataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference =
                dataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference;
        }

        public QualifiedDataNameOrQualifiedConditionName(QualifiedDataNameOrQualifiedConditionName1 qualifiedDataNameOrQualifiedConditionName1)
        {
            this.QualifiedDataNameOrQualifiedConditionName1 = qualifiedDataNameOrQualifiedConditionName1;
        }

        /// <summary>
        /// Determines if this QualifiedDataNameOrQualifiedConditionName is a QualifiedDataNameOrQualifiedConditionName1
        /// </summary>
        public bool IsQualifiedDataNameOrQualifiedConditionName1
            => this.QualifiedDataNameOrQualifiedConditionName1 != null;
    }

    /// <summary>
    /// Representation of:
    /// qualifiedDataNameOrQualifiedConditionNameOrIndexName:
    /// dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference | qualifiedDataNameOrQualifiedConditionName1;
    /// </summary>
    public class QualifiedDataNameOrQualifiedConditionNameOrIndexName
    {
        public Token DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference { get; private set; }
        public QualifiedDataNameOrQualifiedConditionName1 QualifiedDataNameOrQualifiedConditionName1 { get; private set; }

        /// <summary>
        /// Constructor of 
        /// qualifiedDataNameOrQualifiedConditionNameOrIndexName: dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference
        /// </summary>
        /// <param name="dataNameReferenceOrConditionNameReferenceOrConditionForUpsiSwitchNameReference"></param>
        public QualifiedDataNameOrQualifiedConditionNameOrIndexName(Token dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference)
        {
            this.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference =
                dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference;
        }

        /// <summary>
        /// Constructor of
        /// qualifiedDataNameOrQualifiedConditionNameOrIndexName: qualifiedDataNameOrQualifiedConditionName1
        /// </summary>
        /// <param name="qualifiedDataNameOrQualifiedConditionName1"></param>
        public QualifiedDataNameOrQualifiedConditionNameOrIndexName(QualifiedDataNameOrQualifiedConditionName1 qualifiedDataNameOrQualifiedConditionName1)
        {
            this.QualifiedDataNameOrQualifiedConditionName1 = qualifiedDataNameOrQualifiedConditionName1;
        }

        /// <summary>
        /// Determines if this QualifiedDataNameOrQualifiedConditionName is a QualifiedDataNameOrQualifiedConditionName1
        /// </summary>
        public bool IsQualifiedDataNameOrQualifiedConditionName1
            => this.QualifiedDataNameOrQualifiedConditionName1 != null;
    }


    /// <summary>
    /// Representation of
    /// qualifiedDataNameOrQualifiedConditionNameOrFileName:
    /// dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference 
    /// | qualifiedDataNameOrQualifiedConditionName1;
    /// </summary>
    public class QualifiedDataNameOrQualifiedConditionNameOrFileName
    {
        public Token DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference { get; private set; }
        public QualifiedDataNameOrQualifiedConditionName1 QualifiedDataNameOrQualifiedConditionName1 { get; private set; }

        /// <summary>
        /// Constructor of 
        /// qualifiedDataNameOrQualifiedConditionNameOrFileName: dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference 
        /// </summary>
        /// <param name="dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference"></param>
        public QualifiedDataNameOrQualifiedConditionNameOrFileName(
            Token dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference)
        {
            this.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference =
                dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference;
        }

        /// <summary>
        /// Constructor of
        /// qualifiedDataNameOrQualifiedConditionNameOrFileName: qualifiedDataNameOrQualifiedConditionName1
        /// </summary>
        /// <param name="qualifiedDataNameOrQualifiedConditionName1"></param>
        public QualifiedDataNameOrQualifiedConditionNameOrFileName(QualifiedDataNameOrQualifiedConditionName1 qualifiedDataNameOrQualifiedConditionName1)
        {
            this.QualifiedDataNameOrQualifiedConditionName1 = qualifiedDataNameOrQualifiedConditionName1;
        }

        /// <summary>
        /// Determines if this QualifiedDataNameOrQualifiedConditionName is a QualifiedDataNameOrQualifiedConditionName1
        /// </summary>
        public bool IsQualifiedDataNameOrQualifiedConditionName1
            => this.QualifiedDataNameOrQualifiedConditionName1 != null;

    }

    /// <summary>
    /// Representation for
    /// qualifiedDataNameOrQualifiedConditionNameOrClassName:
    /// dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference 
    /// | qualifiedDataNameOrQualifiedConditionName1;
    /// </summary>
    public class QualifiedDataNameOrQualifiedConditionNameOrClassName
    {
        public Token DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference { get; private set; }
        public QualifiedDataNameOrQualifiedConditionName1 QualifiedDataNameOrQualifiedConditionName1 { get; private set; }

        /// <summary>
        /// Constructor for
        /// qualifiedDataNameOrQualifiedConditionNameOrClassName: dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference 
        /// </summary>
        /// <param name="dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference"></param>
        public QualifiedDataNameOrQualifiedConditionNameOrClassName(Token dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference)
        {
            this.DataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference =
                dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference;
        }

        /// <summary>
        /// Constructor for
        /// qualifiedDataNameOrQualifiedConditionNameOrClassName: qualifiedDataNameOrQualifiedConditionName1;
        /// </summary>
        /// <param name="qualifiedDataNameOrQualifiedConditionName1"></param>
        public QualifiedDataNameOrQualifiedConditionNameOrClassName(QualifiedDataNameOrQualifiedConditionName1 qualifiedDataNameOrQualifiedConditionName1)
        {
            this.QualifiedDataNameOrQualifiedConditionName1 = qualifiedDataNameOrQualifiedConditionName1;
        }

        /// <summary>
        /// Determines if this QualifiedDataNameOrQualifiedConditionName is a QualifiedDataNameOrQualifiedConditionName1
        /// </summary>
        public bool IsQualifiedDataNameOrQualifiedConditionName1
            => this.QualifiedDataNameOrQualifiedConditionName1 != null;
    }

    /// <summary>
    /// Representation of:
    /// (UserDefinedWord QualifiedNameSeparator)+
    /// </summary>
    public class QualifiedPath : List<Tuple<Token, Token>>
    {
        
    }

    /// <summary>
    /// Representation of:
    /// qualifiedIndexName: indexName=UserDefinedWord 
    /// | (UserDefinedWord QualifiedNameSeparator)+ TcHeadDefiniiton=UserDefinedWord
    /// </summary>
    public class QualifiedIndexName
    {
        public Token IndexName { get; private  set; }
        public QualifiedPath Path { get; private set; }
        public Token TcHeadDefinition { get; private set; }

        /// <summary>
        /// Constructor of:
        /// qualifiedIndexName: indexName=UserDefinedWord 
        /// </summary>
        public QualifiedIndexName(Token indexName)
        {
            this.IndexName = indexName;
        }

        /// <summary>
        /// Constructor of
        /// qualifiedIndexName: (UserDefinedWord QualifiedNameSeparator)+ TcHeadDefiniiton=UserDefinedWord
        /// </summary>
        /// <param name="path"></param>
        /// <param name="tcHeadDefinition"></param>
        public QualifiedIndexName(QualifiedPath path, Token tcHeadDefinition)
        {
            this.Path = path;
            this.TcHeadDefinition = tcHeadDefinition;
        }
        public bool isSimpleIndexName => IndexName != null;
    }

    /// <summary>
    /// Representation of:
    /// qualifiedTextName: textName ((IN | OF) libraryName)?; 
    /// </summary>
    public class QualifiedTextName
    {
        public Token TextName { get; private set; }
        public Token InOf { get; private set; }
        public Token LibraryName { get; private set; }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="textName"></param>
        public QualifiedTextName(Token textName)
        {
            this.TextName = textName;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="textName"></param>
        /// <param name="inof"></param>
        /// <param name="libraryName"></param>
        public QualifiedTextName(Token textName, Token inof, Token libraryName)
        {
            this.TextName = textName;
            this.InOf = InOf;
            this.LibraryName = libraryName;
        }
    }

}
