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
            Instance.RegisterFactory(OutputFormat.Cobol85.ToString(), (id, document, destination, skeletons) => new DefaultGenerator(document, destination, skeletons));
            Instance.RegisterFactory(OutputFormat.PublicSignatures.ToString(), (id, document, destination, skeletons) => new SignaturesGenerator(destination));
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
        /// <param name="Document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        /// <returns>The IGenerator instance if one has been created, null otherwise.</returns>
        public IGenerator Create(string ID, TypeCobol.Compiler.CompilationDocument document, System.IO.TextWriter destination, List<Skeletons.Skeleton> skeletons)
        {
            if (!RegistryMap.ContainsKey(ID))
                return null;
            return RegistryMap[ID](ID, document, destination, skeletons);
        }
    }
}
