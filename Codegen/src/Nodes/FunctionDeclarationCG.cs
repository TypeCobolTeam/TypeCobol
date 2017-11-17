using System;
using System.Linq;
using JetBrains.Annotations;

namespace TypeCobol.Codegen.Nodes {
    using System.Collections.Generic;
    using TypeCobol.Compiler.CodeElements;
    using TypeCobol.Compiler.CodeElements.Expressions;
    using TypeCobol.Compiler.Nodes;
    using TypeCobol.Compiler.Text;

    internal class FunctionDeclarationCG : Compiler.Nodes.FunctionDeclaration, Generated {
        private string ProgramHashName = null;
        private string OriginalProcName = string.Empty; //Limited to 22 characters
        FunctionDeclaration OriginalNode = null;

        public FunctionDeclarationCG(Compiler.Nodes.FunctionDeclaration originalNode) : base(originalNode.CodeElement()) {
            this.OriginalNode = originalNode;

            //Check if we need to generate something special for this Procedure
            bool needToGenerateParametersIntoLinkage = originalNode.CodeElement().Profile.InputParameters.Count + originalNode.CodeElement().Profile.InoutParameters.Count + originalNode.CodeElement().Profile.OutputParameters.Count +
                             (originalNode.CodeElement().Profile.ReturningParameter != null ? 1 : 0) > 0;
            //we'll generate things for public call
            var containsPublicCall = originalNode.ProcStyleCalls != null && originalNode.ProcStyleCalls.Count > 0;

            ProgramHashName = originalNode.Hash;
            //Get procedure original name and truncate it to 22 chars if over. 
            OriginalProcName = originalNode.Name.Substring(0,Math.Min(originalNode.Name.Length, 22));

            foreach (var child in originalNode.Children) {
                if (child is Compiler.Nodes.ProcedureDivision) {
                    Compiler.Nodes.LinkageSection linkageSection = null;
                    Compiler.Nodes.DataDivision dataDivision = null;

                    //Create DataDivision and LinkageSection if needed
                    //DataDivision must be created before ProcedureDivision because Program Node doesn't manage order of their children
                    //DataDivision manage order of their children so it's ok
                    if (needToGenerateParametersIntoLinkage || containsPublicCall) {
                        dataDivision = GetOrCreateNode<Compiler.Nodes.DataDivision>(originalNode, () => new DataDivision());
                        linkageSection = GetOrCreateNode<Compiler.Nodes.LinkageSection>(dataDivision, () => new LinkageSection(), dataDivision);


                        //declare procedure parameters into linkage
                        DeclareProceduresParametersIntoLinkage(originalNode, linkageSection, originalNode.CodeElement().Profile);
                    }
                    

                    //Replace ProcedureDivision node with a new one and keep all original children
                    var sentences = new List<Node>();
                    foreach (var sentence in child.Children)
                        sentences.Add(sentence);
                    var pdiv = new ProcedureDivision(originalNode, sentences);
                    children.Add(pdiv);

                    
                    //Generate code if this procedure call a public procedure in another source
                    
                    if (containsPublicCall) {
                        var workingStorageSection = GetOrCreateNode<Compiler.Nodes.WorkingStorageSection>(dataDivision, () => new WorkingStorageSection(), dataDivision);
                        GenerateCodeToCallPublicProc(originalNode, pdiv,  workingStorageSection, linkageSection);
                    }
                } else {
                    if (child.CodeElement is FunctionDeclarationEnd) {
                        children.Add(new ProgramEnd(new URI(ProgramHashName), OriginalProcName));
                    } else {
                        // TCRFUN_CODEGEN_NO_ADDITIONAL_DATA_SECTION
                        // TCRFUN_CODEGEN_DATA_SECTION_AS_IS
                        children.Add(child);
                    }
                }
            }

            
        }

        /// <summary>
        /// Get or create the node and add it as a child of this Node
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="parent"></param>
        /// <param name="func"></param>
        /// <returns></returns>
        private T GetOrCreateNode<T>(Node parent, Func<T> func) where T : Node {
            return GetOrCreateNode(parent, func, this);
        }

        private T GetOrCreateNode<T>(Node parent, Func<T> func, Node newParent) where T : Node {
            T newNode;
            var retrievedChild = parent.GetChildren<T>();
            if (retrievedChild == null || !retrievedChild.Any()) {
                newNode = func.Invoke();
                //Do not add to parent but to "this"
                newParent.Add(newNode);
            } else {
                newNode = retrievedChild.First();
            }
            return newNode;
        }


        protected void GenerateCodeToCallPublicProc(FunctionDeclaration originalNode, ProcedureDivision procedureDivision, Compiler.Nodes.WorkingStorageSection workingStorageSection, Compiler.Nodes.LinkageSection linkageSection) {
            ProgramImports imports = ProgramImportsAttribute.GetProgramImports(originalNode);

            foreach (var pgm in imports.Programs.Values) {
                workingStorageSection.Add(
                    new GeneratedNode2("01 TC-" + pgm.Name + " pic X(08) value '" + pgm.Name + "'.\n", true));
            }

            
            linkageSection.Add(new GeneratedNode2("*Common to all librairies used by the program.", true));
            linkageSection.Add(new GeneratedNode2("01 TC-Library-PntTab.", false));
            linkageSection.Add(new GeneratedNode2("    05 TC-Library-PntNbr          PIC S9(04) COMP.", true));
            linkageSection.Add(new GeneratedNode2(
                "    05 TC-Library-Item OCCURS 1000\n                        DEPENDING ON TC-Library-PntNbr\n                        INDEXED   BY TC-Library-Idx.",
                false));
            linkageSection.Add(new GeneratedNode2("       10 TC-Library-Item-Idt      PIC X(08).", true));
            linkageSection.Add(new GeneratedNode2("       10 TC-Library-Item-Pnt      PROCEDURE-POINTER.", true));


            foreach (var pgm in imports.Programs.Values) {
                foreach (var proc in pgm.Procedures.Values) {
                    proc.IsNotByExternalPointer = true;
                    linkageSection.Add(new GeneratedNode2(" ", true));
                    linkageSection.Add(new GeneratedNode2("*" + pgm.Name + "::" + proc.Name, true));
                    linkageSection.Add(new GeneratedNode2("01 TC-" + pgm.Name + "-" + proc.Hash + "-Item.", false));
                    linkageSection.Add(new GeneratedNode2("   05 TC-" + pgm.Name + "-" + proc.Hash + "-Idt PIC X(08).", true));
                    linkageSection.Add(new GeneratedNode2("   05 TC-" + pgm.Name + "-" + proc.Hash + " PROCEDURE-POINTER.",
                        true));
                }
            }


            Node whereToGenerate;

            //Generate a PERFORM, this must be the first instruction unless we have a Paragraph or a section
            //TODO manage declaratives, see #655
            var firstChildOfPDiv = procedureDivision.Children.First();
            if (firstChildOfPDiv is Section) {
                var temp = firstChildOfPDiv.Children.First();
                if (temp is Paragraph) {
                    whereToGenerate = temp;
                } else {
                    whereToGenerate = firstChildOfPDiv;
                }
            } else if (firstChildOfPDiv is Paragraph) {
                whereToGenerate = firstChildOfPDiv;
            } else {
                whereToGenerate = procedureDivision;
            }
            whereToGenerate.Add(new GeneratedNode2("    PERFORM TC-Initializations", true), 0);


            //Generate "TC-Initializations" paragraph
            procedureDivision.Add(new GeneratedNode2("*=================================================================", true));
            procedureDivision.Add(new ParagraphGen("TC-Initializations"));
            procedureDivision.Add(new SentenceEnd());
            procedureDivision.Add(new GeneratedNode2("*=================================================================", true));


            //Generate "TC-LOAD-POINTERS-" paragraph
            foreach (var pgm in imports.Programs.Values) {
                procedureDivision.Add(new GeneratedNode2("*=================================================================", true));
                procedureDivision.Add(new ParagraphGen("TC-LOAD-POINTERS-" + pgm.Name));
                procedureDivision.Add(new GeneratedNode2("*=================================================================",true));
                procedureDivision.Add(new GeneratedNode2("     CALL 'ZCALLPGM' USING TC-" + pgm.Name, true));

                procedureDivision.Add(new GeneratedNode2("     ADDRESS OF TC-Library-PntTab", true));
                procedureDivision.Add(new GeneratedNode2("     PERFORM VARYING TC-Library-Idx FROM 1 BY 1", true));
                procedureDivision.Add(new GeneratedNode2("     UNTIL TC-Library-Idx > TC-Library-PntNbr", true));
                procedureDivision.Add(new GeneratedNode2("         EVALUATE TC-Library-Item-Idt (TC-Library-Idx)", true));
                foreach (var proc in pgm.Procedures.Values) {
                    procedureDivision.Add(new GeneratedNode2("         WHEN '" + proc.Hash + "'", true));
                    procedureDivision.Add(new GeneratedNode2("              SET ADDRESS OF", true));
                    procedureDivision.Add(new GeneratedNode2("              TC-" + pgm.Name + "-" + proc.Hash + "-Item", true));
                    procedureDivision.Add(new GeneratedNode2("              TO ADDRESS OF", true));
                    procedureDivision.Add(new GeneratedNode2("              TC-Library-Item(TC-Library-Idx)", true));
                }
                procedureDivision.Add(new GeneratedNode2("         END-EVALUATE", true));
                procedureDivision.Add(new GeneratedNode2("     END-PERFORM", true));
                procedureDivision.Add(new GeneratedNode2("     .", true));
            }
        }

        private void DeclareProceduresParametersIntoLinkage(Compiler.Nodes.FunctionDeclaration node, Compiler.Nodes.LinkageSection linkage, ParametersProfile profile) {
            var data = linkage.Children();

            // TCRFUN_CODEGEN_PARAMETERS_ORDER
            var generated = new List<string>();
            foreach (var parameter in profile.InputParameters) {
                if (!generated.Contains(parameter.Name) && !Contains(data, parameter.Name)) {
                    linkage.Add(CreateParameterEntry(parameter, node.SymbolTable));
                    generated.Add(parameter.Name);
                }
            }
            foreach (var parameter in profile.InoutParameters) {
                if (!generated.Contains(parameter.Name) && !Contains(data, parameter.Name)) {
                    linkage.Add(CreateParameterEntry(parameter, node.SymbolTable));
                    generated.Add(parameter.Name);
                }
            }
            foreach (var parameter in profile.OutputParameters) {
                if (!generated.Contains(parameter.Name) && !Contains(data, parameter.Name)) {
                    linkage.Add(CreateParameterEntry(parameter, node.SymbolTable));
                    generated.Add(parameter.Name);
                }
            }
            if (profile.ReturningParameter != null) {
                if (!generated.Contains(profile.ReturningParameter.Name) &&
                    !Contains(data, profile.ReturningParameter.Name)) {
                    linkage.Add(CreateParameterEntry(profile.ReturningParameter, node.SymbolTable));
                    generated.Add(profile.ReturningParameter.Name);
                }
            }
            
        }

        private ParameterEntry CreateParameterEntry(ParameterDescriptionEntry parameter, Compiler.CodeModel.SymbolTable table) {
            var generated = new ParameterEntry(parameter, table);
            if (parameter.DataConditions != null) {
                foreach (var child in parameter.DataConditions) generated.Add(new DataCondition(child));
            }
            return generated;
        }

        private bool Contains([NotNull] IEnumerable<DataDefinition> data, string dataname) {
            foreach (var node in data)
                if (dataname.Equals(node.Name))
                    return true;
            return false;
        }

        private List<ITextLine> _cache = null;

        public override IEnumerable<ITextLine> Lines {
            get {
                if (_cache == null) {
                    _cache = new List<ITextLine>(); // TCRFUN_CODEGEN_AS_NESTED_PROGRAM
                    //_cache.Add(new TextLineSnapshot(-1, "*", null));
                    //TODO add Function signature as comment
                    _cache.Add(new TextLineSnapshot(-1, "*_________________________________________________________________",
                        null));
                    _cache.Add(new TextLineSnapshot(-1, "IDENTIFICATION DIVISION.", null));
                    _cache.Add(new TextLineSnapshot(-1, "PROGRAM-ID. " + ProgramHashName + OriginalProcName + '.', null));
                    var envDiv = OriginalNode.GetProgramNode().GetChildren<EnvironmentDivision>();
                    if (envDiv != null && envDiv.Count == 1) {
                        _cache.Add(new TextLineSnapshot(-1, "ENVIRONMENT DIVISION. ", null));

                        var configSections = envDiv.First().GetChildren<ConfigurationSection>();

                        if (configSections != null && configSections.Count == 1) {
                            foreach (var line in configSections.First().SelfAndChildrenLines) {
                                _cache.Add(line);
                            }
                        }
                    } //else no config or semantic error, no my job  
                }
                return _cache;
            }
        }

        public bool IsLeaf {
            get { return false; }
        }
    }
}