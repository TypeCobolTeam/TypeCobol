using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Codegen.Generators;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// The class that Manages all GeneratorFactory
    /// </summary>
    public class GeneratorFactoryManager
    {

        /// <summary>
        /// The Unique instanceof this class
        /// </summary>
        private static GeneratorFactoryManager Singleton
        {
            get;
            set;
        }

        /// <summary>
        /// Static Constructor
        /// </summary>
        static GeneratorFactoryManager()
        {
            Singleton = new GeneratorFactoryManager();
            Instance.RegisterFactory(OutputFormat.Cobol85.ToString(),          (id, document, destination, typeCobolVersion, bWithLineMap) => bWithLineMap ? new DefaultGeneratorWithLineMap(document, destination, typeCobolVersion) : new DefaultGenerator(document, destination, typeCobolVersion));
            Instance.RegisterFactory(OutputFormat.PublicSignatures.ToString(), (id, document, destination, typeCobolVersion, bWithLineMap) => new SignaturesGenerator(destination, typeCobolVersion));
            Instance.RegisterFactory(OutputFormat.ExpandingCopy.ToString(),    (id, document, destination, typeCobolVersion, bWithLineMap) => new ExpandingCopyGenerator(document, destination));
            Instance.RegisterFactory(OutputFormat.Cobol85Mixed.ToString(),     (id, document, destination, typeCobolVersion, bWithLineMap) => new MixedTransformGenerator(document, destination, bWithLineMap ? new DefaultGeneratorWithLineMap(document, destination, typeCobolVersion) : new DefaultGenerator(document, destination, typeCobolVersion)));
            Instance.RegisterFactory(OutputFormat.Cobol85Nested.ToString(),    (id, document, destination, typeCobolVersion, bWithLineMap) => new NestedGenerator(document, destination, typeCobolVersion));
            Instance.RegisterFactory(OutputFormat.Documentation.ToString(),    (id, document, destination, typeCobolVersion, bWithLineMap) => new DocumentationGenerator(destination, typeCobolVersion));
        }

        /// <summary>
        /// Default constructor
        /// </summary>
        private GeneratorFactoryManager()
        {
            RegistryMap = new Dictionary<String, GeneratorFactory>();
        }

        /// <summary>
        /// Get the unique Instance of the Generator Factory Manager
        /// </summary>
        public static GeneratorFactoryManager Instance
        {
            get
            {
                return Singleton;
            }
        }
        /// <summary>
        /// The Registry Map of registered IGeneratorFactory instance
        /// </summary>
        private Dictionary<String, GeneratorFactory> RegistryMap;

        /// <summary>
        /// Register a GeneratorFactory delegation faactory.
        /// </summary>
        /// <param name="ID">Unique ID of the generator to register</param>
        /// <param name="factory">The delegate factory for Generator creation</param>
        /// <returns>true if the delgation factory was registered, false if there was already a registered delegation factory with the same ID</returns>
        public bool RegisterFactory(String ID, GeneratorFactory factory)
        {
            if (RegistryMap.ContainsKey(ID))
                return false;
            RegistryMap[ID] = factory;
            return true;
        }

        /// <summary>
        /// Create the IGenerator instance corresponding to the given ID.
        /// </summary>
        /// <param name="ID">The Unique ID of the Generator instance to be created</param>
        /// <param name="document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="typeCobolVersion">Current version of TypeCobol parser/codegen</param>
        /// <param name="bWithLineMap">True if Line Map can be generated</param>
        /// <returns>The IGenerator instance if one has been created, null otherwise.</returns>
        public IGenerator Create(string ID, TypeCobol.Compiler.CompilationDocument document, StringBuilder destination, string typeCobolVersion, bool bWithLineMap)
        {
            if (!RegistryMap.ContainsKey(ID))
                return null;
            return RegistryMap[ID](ID, document, destination, typeCobolVersion, bWithLineMap);
        }
    }
}
