using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Processor for GetDataLayout request
    /// </summary>
    public class DataLayoutProcessor
    {
        private const char SPACE = ' ';
        private const string UNDEFINED = "***";
        private const string FILLER = "FILLER";
        private const string GROUP = "GROUP";
        private const string OCCURS = "OCCURS {0}";
        private const string OCCURS_SUFFIX_START = " (";
        private const string OCCURS_SUFFIX = "1";
        private const string OCCURS_SUFFIX_END = ")";
        private const string PIC = "PIC {0}";
        private const string REDEFINES = "REDEFINES {0}";

        /// <summary>
        /// Get the Data Layout rows for a Program or a Copy (output = CSV)
        /// </summary>
        /// <param name="compilationUnit">Compilation unit resulting from parsing the Program/Copy</param>
        /// <param name="separator">Separator for fields to use</param>
        /// <returns>Tuple made of the CSV header and CSV rows</returns>
        public (string Header, string[] Rows) GetDataLayoutAsCSV(CompilationUnit compilationUnit, string separator)
        {
            var rows = new List<string>();
            foreach (var dataLayoutNode in CollectDataLayoutNodes(compilationUnit))
            {
                var row = CreateRow(dataLayoutNode, separator);
                if (row != null)
                {
                    rows.Add(row);
                }
            }

            string header = $"LineNumber{separator}NodeLevel{separator}LevelNumber{separator}VariableName{separator}PictureTypeOrUsage{separator}Start{separator}End{separator}Length";
            return (header, rows.ToArray());

            static string CreateRow(Tuple<int, DataDefinition, int> dataLayoutNode, string separator)
            {
                var nodeLevel = dataLayoutNode.Item1;
                var dataDefinition = dataLayoutNode.Item2;
                var occursDimension = dataLayoutNode.Item3;

                var row = new StringBuilder();

                //TODO manage slack bytes (property is dataDefinition.SlackBytes)

                // Line number (starting at 1)
                AppendToRow(dataDefinition.CodeElement.GetLineInMainSource() + 1);

                // Node level
                AppendToRow(nodeLevel);

                // Level number
                AppendToRow(dataDefinition.CodeElement.LevelNumber);

                // Name
                AppendNameToRow(dataDefinition, occursDimension);

                // Declaration (Picture, Usage, REDEFINES, OCCURS, ...)
                AppendDeclarationToRow(dataDefinition);

                // Start/End/Length
                var start = dataDefinition.StartPosition;
                var length = dataDefinition.PhysicalLength;
                AppendToRow(start);
                AppendToRow(GetEnd(start, length));
                row.Append(length);

                return row.ToString();

                void AppendToRow(object value)
                {
                    row.Append(value).Append(separator);
                }

                void AppendNameToRow(DataDefinition dataDefinition, int occursDimension)
                {
                    row.Append(dataDefinition.Name ?? FILLER);
                    if (occursDimension > 0)
                    {
                        AppendOccursSuffixToRow(occursDimension);
                    }
                    row.Append(separator);
                }

                void AppendOccursSuffixToRow(int occursDimension)
                {
                    row.Append(OCCURS_SUFFIX_START);
                    row.Append(OCCURS_SUFFIX);
                    for (int i = 1; i < occursDimension; i++)
                    {
                        row.Append(SPACE).Append(OCCURS_SUFFIX);
                    }
                    row.Append(OCCURS_SUFFIX_END);
                }

                void AppendDeclarationToRow(DataDefinition dataDefinition)
                {
                    var initialRowLength = row.Length;

                    CodeElementType type = dataDefinition.CodeElement.Type;
                    if (type == CodeElementType.DataRedefinesEntry)
                    {
                        row.Append(string.Format(REDEFINES, ((DataRedefines)dataDefinition).CodeElement.RedefinesDataName.Name));
                    }
                    else if (type == CodeElementType.DataDescriptionEntry)
                    {
                        if (dataDefinition.Picture != null)
                        {
                            row.Append(string.Format(PIC, dataDefinition.Picture.Value));
                        }
                        string usage = ((DataDescriptionEntry)dataDefinition.CodeElement).Usage?.Token.Text;
                        if (!string.IsNullOrWhiteSpace(usage))
                        {
                            AppendSpaceIfNeeded();
                            row.Append(usage);
                        }

                        if (row.Length == initialRowLength && dataDefinition.ChildrenCount > 0)
                        {
                            row.Append(GROUP);
                        }
                    }

                    if (dataDefinition.IsTableOccurence)
                    {
                        AppendSpaceIfNeeded();
                        row.Append(string.Format(OCCURS, dataDefinition.MaxOccurencesCount));
                    }

                    row.Append(separator);

                    void AppendSpaceIfNeeded()
                    {
                        if (row.Length != initialRowLength)
                        {
                            row.Append(SPACE);
                        }
                    }
                }

                static object GetEnd(long start, long length)
                {
                    var end = start + length - 1;
                    return (end > 0) ? end : UNDEFINED;
                }
            }
        }

        private List<Tuple<int, DataDefinition, int>> CollectDataLayoutNodes(CompilationUnit compilationUnit)
        {
            var dataLayoutNodes = new List<Tuple<int, DataDefinition, int>>();
            Node dataDivision = compilationUnit?.TemporaryProgramClassDocumentSnapshot?.Root?.MainProgram?.GetChildren<DataDivision>()?.FirstOrDefault();
            if (dataDivision != null)
            {
                // Consider data declared in the Working storage
                var workingStorage = dataDivision.GetChildren<WorkingStorageSection>().FirstOrDefault();
                if (workingStorage != null)
                {
                    CollectDataLayoutNodes(0, workingStorage, 0);
                }

                // Consider also data declared in the Local storage
                var localStorage = dataDivision.GetChildren<LocalStorageSection>().FirstOrDefault();
                if (localStorage != null)
                {
                    CollectDataLayoutNodes(0, localStorage, 0);
                }
            }

            return dataLayoutNodes;

            void CollectDataLayoutNodes(int nodeLevel, Node node, int occursDimension)
            {
                foreach (var child in node.Children)
                {
                    if (child is DataDefinition childDefinition)
                    {
                        var childOccursDimension = childDefinition.IsTableOccurence ? occursDimension + 1 : occursDimension;
                        if (IsInScope(childDefinition))
                        {
                            dataLayoutNodes.Add(new Tuple<int, DataDefinition, int>(nodeLevel, childDefinition, childOccursDimension));
                        }
                        if (childDefinition.Children.Count > 0)
                        {
                            CollectDataLayoutNodes(nodeLevel + 1, childDefinition, childOccursDimension);
                        }
                    }
                }

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
    }
}