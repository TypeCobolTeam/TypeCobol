using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler.Source;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// A Default Generator that can generate a Line Mapping source file between the original source
    /// code and the generated source code.
    /// </summary>
    public class DefaultGeneratorWithLineMap : DefaultGenerator
    {
        /// <summary>
        /// The Line Mapping intervals
        /// </summary>
        public Tuple<int, int>[] LineMapping
        {
            get;
            private set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="typeCobolVersion">Current version of the TypeCobol parser/codegen</param>
        public DefaultGeneratorWithLineMap(TypeCobol.Compiler.CompilationDocument document, StringBuilder destination, string typeCobolVersion)
            : base(document, destination, typeCobolVersion)
        {
            int count = CompilationResults.TokensLines.Count;
            LineMapping = new Tuple<int, int>[count];
        }

        /// <summary>
        /// Perform a linear Generation
        /// //1) A Non commented line with no Associated nodes is generated without any change.
        /// //2) If the line is commented then first comment all following lines that have the same intersection with the corresponding target Nodes.
        /// //3) For each node related to a line, and not already generated the corresponding code.
        /// //4) Flush of Function declations.
        /// <param name="mapper">The linearization representation</param>
        /// <param name="Input">Input source lines</param>
        /// <param name="clonedMapper">Linear mapper for cloned nodes</param>
        /// <returns>The Generated Source Document</returns>
        /// </summary>
        protected override SourceText LinearGeneration<A>(LinearNodeSourceCodeMapper mapper, IReadOnlyList<A> Input, LinearNodeSourceCodeMapper clonedMapper = null)
        {
            LineMappingCtx lmCtx = new LineMappingCtx(this.LineMapping);
            CurrentLineMappinCtx = lmCtx;
            return base.LinearGeneration<A>(mapper, clonedMapper, Input, lmCtx, 0, mapper.LineData.Length);
        }

        /// <summary>
        /// Generate the Line Mapping file in the given Stream..
        /// The format is 1-based for each line, and its a CSV format of the following from:
        /// LineNumber;StartLine;EndLine
        /// 
        /// where:
        ///     LineNumber is the Line number in the source file
        ///     StartLine is the starting line number in the generated file
        ///     EndLine is the ending line number in the generated file
        /// </summary>
        /// <param name="stream">The Stream in which to generate the mapping</param>
        public override void GenerateLineMapFile(System.IO.Stream stream)
        {
            if (LineMapping != null)
            {
                for(int i = 0; i <  LineMapping.Length; i++)
                {
                    var range = LineMapping[i];
                    if (range != null)
                    {
                        string lm = String.Format("{0};{1};{2}{3}", i + 1, range.Item1, range.Item2, Environment.NewLine);
                        byte[] bytes = ASCIIEncoding.Default.GetBytes(lm);
                        stream.Write(bytes, 0, bytes.Length);
                    }
                }
                //If there are inverse line mapping generate also
                if (CurrentLineMappinCtx.InverseLineMapping != null)
                {
                    foreach(var inv in CurrentLineMappinCtx.InverseLineMapping)
                    {
                        string lm = String.Format("{0};{1};{2}{3}", inv.Item1, inv.Item2, inv.Item2, Environment.NewLine);
                        byte[] bytes = ASCIIEncoding.Default.GetBytes(lm);
                        stream.Write(bytes, 0, bytes.Length);
                    }
                }
            }
            stream.Flush();
        }

        public override bool HasLineMapData => LineMapping != null;

    }
}
