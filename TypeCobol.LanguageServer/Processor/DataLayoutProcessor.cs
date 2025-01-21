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

        /// <summary>
        /// Get the Data Layout rows for a Copy or a Program (output = CSV)
        /// </summary>
        /// <param name="compilationUnit">Compilation unit resulting from parsing the Copy or Program</param>
        /// <param name="position">Position determining the data to be considered (useful in case of the stacked/nested Programs)</param>
        /// <param name="separator">Separator for fields to use</param>
        /// <returns>Tuple made of the root (the Copy or the Program containing the data), CSV header and CSV rows</returns>
        public (string Root, string Header, string[] Rows) GetDataLayoutAsCSV(CompilationUnit compilationUnit, Position position, string separator)
        {
            var rows = new List<string>();
            var rootDLN = CollectDataLayoutNodesAtPosition(compilationUnit, position, ConvertToRow);
            var root = rootDLN.name;

            string header = $"LineNumber{separator}NodeLevel{separator}LevelNumber{separator}VariableName{separator}PictureTypeOrUsage{separator}Start{separator}End{separator}Length";
            return (root, header, rows.ToArray());

            void ConvertToRow(DataLayoutNode dataLayoutNode)
            {
                var row = new StringBuilder();

                //TODO manage slack bytes (property is dataDefinition.SlackBytes)
                AppendToRow(dataLayoutNode.line);
                AppendToRow(dataLayoutNode.logicalLevel - 2); // To compensate Program and Section levels
                AppendToRow(dataLayoutNode.physicalLevel);
                AppendToRow(GetNameWithDimensions(dataLayoutNode)); // Ex.: Data-Name (1, 1) for 2 nested OCCURS
                AppendToRow(dataLayoutNode.declaration);

                // Start/End/Length
                var start = dataLayoutNode.start;
                var length = dataLayoutNode.length;
                AppendToRow(start);
                AppendToRow(GetEnd(start, length));
                row.Append(length);

                rows.Add(row.ToString());

                void AppendToRow(object value) => row.Append(value).Append(separator);

                string GetNameWithDimensions(DataLayoutNode dataLayoutNode)
                {
                    if (dataLayoutNode.occursDimension == 0)
                    {
                        return dataLayoutNode.name;
                    }
                    string dimensions = string.Join(SPACE, Enumerable.Repeat(DIMENSION_ITEM, dataLayoutNode.occursDimension));
                    return $"{dataLayoutNode.name} ({dimensions})";
                }

                static object GetEnd(long start, long length)
                {
                    var end = start + length - 1;
                    return (end > 0) ? end : UNDEFINED;
                }
            }
        }

        /// <summary>
        /// Get the Data Layout nodes for a Copy or a Program (output = TREE)
        /// </summary>
        /// <param name="compilationUnit">Compilation unit resulting from parsing the Copy or Program</param>
        /// <param name="position">Position determining the data to be considered (useful in case of the stacked/nested Programs)</param>
        /// <returns>Root data layout node</returns>
        public DataLayoutNode GetDataLayoutAsTree(CompilationUnit compilationUnit, Position position)
        {
            return CollectDataLayoutNodesAtPosition(compilationUnit, position, null);
        }

        private DataLayoutNode CollectDataLayoutNodesAtPosition(CompilationUnit compilationUnit, Position position, Action<DataLayoutNode> convert)
        {
            var program = GetProgram(compilationUnit, position);

            return CollectInProgram(program, convert);
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

        private DataLayoutNode CollectInProgram(Program program, Action<DataLayoutNode> convert)
        {
            var rootDLN = DataLayoutNodeBuilder.From(program);

            DataDivision dataDivision = program.GetChildren<DataDivision>()?.FirstOrDefault();
            if (dataDivision != null)
            {
                var sectionsDLN = new List<DataLayoutNode>();

                // Consider data declared in the Working and Local storage sections
                CollectInSection(dataDivision.WorkingStorageSection);
                CollectInSection(dataDivision.LocalStorageSection);
                // Consider also data declared in the Linkage section
                CollectInSection(dataDivision.LinkageSection);

                rootDLN.children = sectionsDLN.ToArray();

                void CollectInSection(DataSection dataSection)
                {
                    if (dataSection != null)
                    {
                        var sectionDLN = DataLayoutNodeBuilder.From(dataSection);
                        sectionsDLN.Add(sectionDLN);
                        sectionDLN.children = CollectDataLayoutNodes(dataSection, sectionDLN).ToArray();
                    }
                }
            }

            return rootDLN;

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

                        convert?.Invoke(childDLN);

                        if (childDefinition.Children.Count > 0)
                        {
                            childDLN.children = CollectDataLayoutNodes(child, childDLN).ToArray();
                        }
                    }
                }

                return result;

                static bool IsInScope(DataDefinition dataDefinition)
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
            }
        }

        private class DataLayoutNodeBuilder
        {
            internal static DataLayoutNode From(Program program)
            {
                return new()
                {
                    name = program.Name,
                    index = DataLayoutNode.UNDEFINED,
                    flags = DataLayoutNodeFlags.None
                };
            }

            internal static DataLayoutNode From(DataSection dataSection)
            {
                return new()
                {
                    logicalLevel = 1,
                    name = dataSection.ID,
                    index = DataLayoutNode.UNDEFINED,
                    flags = DataLayoutNodeFlags.None
                };
            }

            internal static DataLayoutNode From(DataDefinition dataDefinition, DataLayoutNode parent, int index)
            {
                Debug.Assert(parent != null);

                bool incrementDimension = dataDefinition.IsTableOccurence;

                int logicalLevel = parent.logicalLevel + 1;
                int line = dataDefinition.CodeElement.GetLineInMainSource() + 1;
                long physicalLevel = dataDefinition.CodeElement.LevelNumber.Value;
                var name = dataDefinition.Name ?? FILLER;
                int occursDimension = parent.occursDimension + (incrementDimension ? 1 : 0);
                long start = dataDefinition.StartPosition;
                long length = dataDefinition.PhysicalLength;
                string copy = dataDefinition.CodeElement.FirstCopyDirective?.TextName;

                DataLayoutNodeFlags flags = DataLayoutNodeFlags.None;
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

                    if (IsDisplayable())
                    {
                        flags |= DataLayoutNodeFlags.Displayable;
                    }

                    bool IsDisplayable()
                    {
                        // FILLER with a National or NationalEdited picture are not displayable
                        if (name.Equals(FILLER) && IsNationalOrNationalEdited(dataDefinition))
                        {
                            return false;
                        }

                        // Usage Index, FunctionPointer and ProcedurePointer are not displayable
                        return usage?.Value != DataUsage.Index && usage?.Value != DataUsage.FunctionPointer && usage?.Value != DataUsage.ProcedurePointer;

                        static bool IsNationalOrNationalEdited(DataDefinition dataDefinition)
                        {
                            bool hasPicture = dataDefinition.SemanticData?.Type?.Tag == Compiler.Types.Type.Tags.Picture;
                            if (hasPicture)
                            {
                                var picture = (PictureType)dataDefinition.SemanticData.Type;
                                return picture.Category == PictureCategory.National || picture.Category == PictureCategory.NationalEdited;
                            }

                            return false;
                        }
                    }

                }

                if (declarationItems.Count == 0 && dataDefinition.IsGroup)
                {
                    declarationItems.Add(GROUP);
                }

                if (dataDefinition.CodeElement.Type == CodeElementType.DataRedefinesEntry)
                {
                    flags |= DataLayoutNodeFlags.IsRedefines;
                    declarationItems.Add($"REDEFINES {((DataRedefines)dataDefinition).CodeElement.RedefinesDataName.Name}");
                }

                if (incrementDimension)
                {
                    declarationItems.Add($"OCCURS {dataDefinition.MaxOccurencesCount}");
                }

                return new()
                {
                    logicalLevel = logicalLevel,
                    line = line,
                    physicalLevel = physicalLevel,
                    name = name,
                    declaration = string.Join(SPACE, declarationItems),
                    occursDimension = occursDimension,
                    start = start,
                    length = length,
                    copy = copy,
                    index = index,
                    flags = flags,
                    children = []
                };
            }
        }
    }
}