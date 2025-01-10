using System.Diagnostics;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Types;
using TypeCobol.LanguageServer.Utilities;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Processor for GetDataLayout request
    /// </summary>
    public class DataLayoutProcessor
    {
        private const char SPACE = ' ';
        private const string UNDEFINED = "***";
        private const string DIMENSION_ITEM = "1";
        private const string GROUP = "GROUP";
        private const string FILLER = "FILLER";

        private readonly struct DataLayoutItem
        {
            // Logical level: +1 on parent one
            public int LogicalLevel { get; }

            // Line number (starting at 1)
            public int Line { get; }

            // Level number
            public long PhysicalLevel { get; }

            // Name
            public string Name { get; }

            // Declaration (Picture, Usage, REDEFINES, OCCURS, ...)
            public string Declaration { get; }

            // +1 on parent one if OCCURS
            public int OccursDimension { get; }

            // Start position
            public long Start { get; }

            // Physical length
            public long Length { get; }

            // Copy directive (possibly null)
            public string Copy { get; }

            // Position in parent node's children
            public int Index { get; }

            // Flags IsRedefines, Displayable, ...
            public DataLayoutNodeFlags Flags { get; }

            private readonly DataDefinition _dataDefinition;

            public DataLayoutItem(DataDefinition dataDefinition, int parentLogicalLevel, int parentOccursDimension, int index = -1)
            {
                _dataDefinition = dataDefinition;

                bool incrementDimension = dataDefinition.IsTableOccurence;

                //TODO manage slack bytes (property is dataDefinition.SlackBytes)

                LogicalLevel = parentLogicalLevel + 1;
                Line = dataDefinition.CodeElement.GetLineInMainSource() + 1;
                PhysicalLevel = dataDefinition.CodeElement.LevelNumber.Value;
                Name = dataDefinition.Name ?? FILLER;
                OccursDimension = parentOccursDimension + (incrementDimension ? 1 : 0);
                Start = dataDefinition.StartPosition;
                Length = dataDefinition.PhysicalLength;
                Copy = dataDefinition.CodeElement.FirstCopyDirective?.TextName;
                Index = index;

                Flags = DataLayoutNodeFlags.None;
                var declarationItems = new List<string>();
                if (dataDefinition.CodeElement is CommonDataDescriptionAndDataRedefines codeElement)
                {
                    if (dataDefinition.Picture != null)
                    {
                        declarationItems.Add($"PIC {dataDefinition.Picture.Value}");
                    }
                    var usage = codeElement.Usage;
                    if (usage != null)
                    {
                        declarationItems.Add(usage.Token.Text);
                    }

                    if (IsDisplayable(this))
                    {
                        Flags |= DataLayoutNodeFlags.Displayable;
                    }

                    bool IsDisplayable(DataLayoutItem current)
                    {
                        // FILLER with a National or NationalEdited picture are not displayable
                        if ((current.Name == FILLER) && IsNationalOrNationalEdited(current))
                        {
                            return false;
                        }

                        // Usage Index, FunctionPointer and ProcedurePointer are not displayable
                        return usage?.Value != DataUsage.Index && usage?.Value != DataUsage.FunctionPointer && usage?.Value != DataUsage.ProcedurePointer;
                    }

                    bool IsNationalOrNationalEdited(DataLayoutItem current)
                    {
                        var type = current._dataDefinition.SemanticData?.Type;
                        bool hasPicture = type?.Tag == Compiler.Types.Type.Tags.Picture;
                        if (hasPicture)
                        {
                            var picture = (PictureType)type;
                            return picture.Category == PictureCategory.National || picture.Category == PictureCategory.NationalEdited;
                        }

                        return false;
                    }
                }

                if (declarationItems.Count == 0 && dataDefinition.IsGroup)
                {
                    declarationItems.Add(GROUP);
                }

                if (dataDefinition.CodeElement.Type == CodeElementType.DataRedefinesEntry)
                {
                    Flags |= DataLayoutNodeFlags.IsRedefines;
                    declarationItems.Add($"REDEFINES {((DataRedefines)dataDefinition).CodeElement.RedefinesDataName.Name}");
                }

                if (incrementDimension)
                {
                    declarationItems.Add($"OCCURS {dataDefinition.MaxOccurencesCount}");
                }

                Declaration = string.Join(SPACE, declarationItems);
            }

            public string ToRow(string separator)
            {
                var row = new StringBuilder();

                AppendWithSeparator(Line);
                AppendWithSeparator(LogicalLevel);
                AppendWithSeparator(PhysicalLevel);
                AppendWithSeparator(GetNameWithDimensions()); // Ex.: Data-Name (1, 1) for 2 nested OCCURS
                AppendWithSeparator(Declaration);

                // Start/End/Length
                AppendWithSeparator(Start);
                AppendWithSeparator(GetEnd());
                row.Append(Length);

                return row.ToString();

                void AppendWithSeparator(object value)
                {
                    row.Append(value).Append(separator);
                }
            }

            public DataLayoutNode ToDataLayoutNode()
            {
                return new()
                {
                    logicalLevel = LogicalLevel,
                    line = Line,
                    physicalLevel = PhysicalLevel,
                    name = Name,
                    declaration = Declaration,
                    occursDimension = OccursDimension,
                    start = Start,
                    length = Length,
                    copy = Copy,
                    index = Index,
                    flags = Flags,
                    children = []
                };
            }

            private string GetNameWithDimensions()
            {
                if (OccursDimension == 0)
                {
                    return Name;
                }
                string dimensions = string.Join(SPACE, Enumerable.Repeat(DIMENSION_ITEM, OccursDimension));
                return $"{Name} ({dimensions})";
            }

            private string GetEnd()
            {
                var end = Start + Length - 1;
                return (end > 0) ? end.ToString() : UNDEFINED;
            }
        }

        /// <summary>
        /// Get the Data Layout rows for a Program or a Copy (output = CSV)
        /// </summary>
        /// <param name="compilationUnit">Compilation unit resulting from parsing the Program/Copy</param>
        /// <param name="position">Position determining the variables to be considered (i.e. from the main, stacked or nested program)</param>
        /// <param name="separator">Separator for fields to use</param>
        /// <returns>Tuple made of the root (the Copy or the Program containing the data), CSV header and CSV rows</returns>
        public (string Root, string Header, string[] Rows) GetDataLayoutAsCSV(CompilationUnit compilationUnit, Position position, string separator)
        {
            Program program = GetProgram(compilationUnit, position);

            var rows = new List<string>();
            CollectInProgram(program, CollectInSection);

            string header = $"LineNumber{separator}NodeLevel{separator}LevelNumber{separator}VariableName{separator}PictureTypeOrUsage{separator}Start{separator}End{separator}Length";
            return (program?.Name, header, rows.ToArray());

            void CollectInSection(DataSection section)
            {
                if (section != null)
                {
                    // Set level to -1 so that it will start at 0 for the rows
                    CollectRows(section, -1, 0);
                }
            }

            void CollectRows(Node node, int nodeLevel, int occursDimension)
            {
                foreach (var child in node.Children)
                {
                    if (child is DataDefinition childDefinition && IsInScope(childDefinition))
                    {
                        var dataLayoutItem = new DataLayoutItem(childDefinition, nodeLevel, occursDimension);
                        rows.Add(dataLayoutItem.ToRow(separator));

                        if (childDefinition.Children.Count > 0)
                        {
                            CollectRows(childDefinition, dataLayoutItem.LogicalLevel, dataLayoutItem.OccursDimension);
                        }
                    }
                }
            }
        }

        private static Program GetProgram(CompilationUnit compilationUnit, Position position)
        {
            Debug.Assert(compilationUnit != null);
            Debug.Assert(position != null);
            var location = CodeElementLocator.FindCodeElementAt(compilationUnit, position);
            // Get the node corresponding to the position (if null use the main program)
            var locationNode = location.Node ?? (compilationUnit.ProgramClassDocumentSnapshot.Root?.MainProgram);
            if (locationNode == null || locationNode.GetProgramNode() == null)
            {
                throw new Exception($"No program found in: '{compilationUnit.TextSourceInfo.Name}'"); ;
            }

            return locationNode.GetProgramNode();
        }
        private void CollectInProgram(Program program, Action<DataSection> collectInSection)
        {
            DataDivision dataDivision = program.GetChildren<DataDivision>()?.FirstOrDefault();
            if (dataDivision != null)
            {
                // Consider data declared in the Working and Local storage sections
                collectInSection(dataDivision.WorkingStorageSection);
                collectInSection(dataDivision.LocalStorageSection);
                // Consider also data declared in the Linkage section
                collectInSection(dataDivision.LinkageSection);
            }
        }

        private static bool IsInScope(DataDefinition dataDefinition)
        {
            DataDefinitionEntry codeElement = dataDefinition.CodeElement;
            if (codeElement == null || codeElement.Line < 0)
            {
                // Ignore node without CodeElement or with negative line number
                return false;
            }

            // Ignore level 88 and 66
            CodeElementType type = codeElement.Type;
            return type != CodeElementType.DataConditionEntry && type != CodeElementType.DataRenamesEntry;
        }

        /// <summary>
        /// Get the Data Layout nodes for a Copy or a Program (output = TREE)
        /// </summary>
        /// <param name="compilationUnit">Compilation unit resulting from parsing the Copy or Program</param>
        /// <param name="position">Position determining the data to be considered (useful in case of the stacked/nested Programs)</param>
        /// <returns>Root data layout node</returns>
        public DataLayoutNode GetDataLayoutAsTree(CompilationUnit compilationUnit, Position position)
        {
            Program program = GetProgram(compilationUnit, position);

            var rootDLN = DataLayoutNodeBuilder.From(program);
            var sectionDLNs = new List<DataLayoutNode>();

            CollectInProgram(program, CollectInSection);

            rootDLN.children = sectionDLNs.ToArray();

            return rootDLN;

            void CollectInSection(DataSection dataSection)
            {
                if (dataSection != null)
                {
                    var sectionDLN = DataLayoutNodeBuilder.From(dataSection);
                    sectionDLNs.Add(sectionDLN);
                    sectionDLN.children = CollectDataLayoutNodes(dataSection, sectionDLN).ToArray();
                }
            }

            List<DataLayoutNode> CollectDataLayoutNodes(Node parentNode, DataLayoutNode parentDLN)
            {
                var result = new List<DataLayoutNode>();

                for (int i = 0; i < parentNode.ChildrenCount; i++)
                {
                    var child = parentNode.Children[i];
                    if (child is DataDefinition childDefinition && IsInScope(childDefinition))
                    {
                        var childDLN = DataLayoutNodeBuilder.From(childDefinition, parentDLN, i);
                        result.Add(childDLN);
                        if (childDefinition.Children.Count > 0)
                        {
                            childDLN.children = CollectDataLayoutNodes(child, childDLN).ToArray();
                        }
                    }
                }

                return result;
            }
        }

        private class DataLayoutNodeBuilder
        {
            internal static DataLayoutNode From(Program program)
            {
                return new()
                {
                    name = program.Name,
                    //index = program.Parent.ChildIndex(program), //TODO not good. Is this index useful? set it to -1?
                    flags = DataLayoutNodeFlags.None
                };
            }

            internal static DataLayoutNode From(DataSection dataSection)
            {
                return new()
                {
                    logicalLevel = 1,
                    name = dataSection.ID,
                    //index = dataSection.Parent.ChildIndex(dataSection), //TODO not good. Is this index useful? if yes change code to loop on DataDivision else set it to -1?
                    flags = DataLayoutNodeFlags.None
                };
            }

            internal static DataLayoutNode From(DataDefinition dataDefinition, DataLayoutNode parent, int index)
            {
                return new DataLayoutItem(dataDefinition, parent.logicalLevel, parent.occursDimension, index).ToDataLayoutNode();
            }
        }
    }
}